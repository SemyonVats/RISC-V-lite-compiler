(ns riscv-emulator.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; ===================== Defines =====================
(def opcodes
  {:OPC_3   0b0000011
   :OPC_15  0b0001111
   :OPC_19  0b0010011
   :OPC_23  0b0010111
   :OPC_35  0b0100011
   :OPC_51  0b0110011
   :OPC_55  0b0110111
   :OPC_99  0b1100011
   :OPC_103 0b1100111
   :OPC_111 0b1101111
   :OPC_115 0b1110011})

(def funct3
  {:F0 0b000 :F1 0b001 :F2 0b010 :F3 0b011 
   :F4 0b100 :F5 0b101 :F6 0b110 :F7 0b111})

(def funct7
  {:F0_7  0b0000000
   :F1_7  0b0000001
   :F32_7 0b0100000})

(def registers
  {"zero" 0 "ra" 1 "sp" 2 "gp" 3 "tp" 4 "t0" 5 "t1" 6 "t2" 7
   "s0" 8 "fp" 8 "s1" 9 "a0" 10 "a1" 11 "a2" 12 "a3" 13 "a4" 14 "a5" 15
   "a6" 16 "a7" 17 "s2" 18 "s3" 19 "s4" 20 "s5" 21 "s6" 22 "s7" 23
   "s8" 24 "s9" 25 "s10" 26 "s11" 27 "t3" 28 "t4" 29 "t5" 30 "t6" 31
   "r0" 0 "r1" 1 "r2" 2 "r3" 3 "r4" 4 "r5" 5 "r6" 6 "r7" 7 "r8" 8
   "r9" 8 "r10" 9 "r11" 10 "r12" 11 "r13" 12 "r14" 13 "r15" 14 "r16" 15
   "r17" 16 "r18" 17 "r19" 18 "r20" 19 "r21" 20 "r22" 21 "r23" 22
   "r24" 23 "r25" 24 "r26" 25 "r27" 26 "r28" 27 "r29" 28 "r30" 29 "r31" 30})

(defn to-uint32 [x]
  (bit-and x 0xFFFFFFFF))

(defn to-sint32 [x]
  (let [x (to-uint32 x)]
    (if (>= x 0x80000000)
      (- x 0x100000000)
      x)))

(defn parse-imm [s]
  (cond
    (str/starts-with? s "0x") (Long/parseLong (subs s 2) 16)
    (str/starts-with? s "-0x") (- (Long/parseLong (subs s 3) 16))
    :else (Long/parseLong s)))

(defn get-register [reg]
  (cond
    (number? reg) reg
    (str/starts-with? reg "x") (Integer/parseInt (subs reg 1))
    :else (get registers reg)))

(defn trim [s]
  (str/trim s))

;; ===================== Parser =====================
(defn make-r-type [command args]
  (let [rd (get-register (first args))
        rs1 (get-register (second args))
        rs2 (get-register (nth args 2))
        [f3 f7] (case command
                  "add"     [(:F0 funct3) (:F0_7 funct7)]
                  "sub"     [(:F0 funct3) (:F32_7 funct7)]
                  "sll"     [(:F1 funct3) (:F0_7 funct7)]
                  "slt"     [(:F2 funct3) (:F0_7 funct7)]
                  "sltu"    [(:F3 funct3) (:F0_7 funct7)]
                  "xor"     [(:F4 funct3) (:F0_7 funct7)]
                  "srl"     [(:F5 funct3) (:F0_7 funct7)]
                  "sra"     [(:F5 funct3) (:F32_7 funct7)]
                  "or"      [(:F6 funct3) (:F0_7 funct7)]
                  "and"     [(:F7 funct3) (:F0_7 funct7)]
                  "mul"     [(:F0 funct3) (:F1_7 funct7)]
                  "mulh"    [(:F1 funct3) (:F1_7 funct7)]
                  "mulhsu"  [(:F2 funct3) (:F1_7 funct7)]
                  "mulhu"   [(:F3 funct3) (:F1_7 funct7)]
                  "div"     [(:F4 funct3) (:F1_7 funct7)]
                  "divu"    [(:F5 funct3) (:F1_7 funct7)]
                  "rem"     [(:F6 funct3) (:F1_7 funct7)]
                  "remu"    [(:F7 funct3) (:F1_7 funct7)])]
    {:name command :opcode (:OPC_51 opcodes) :type :R-type
     :data {:rd rd :funct3 f3 :rs1 rs1 :rs2 rs2 :funct7 f7}}))

(defn make-i-type [command args]
  (let [rd (get-register (first args))
        rs1 (get-register (second args))
        imm (parse-imm (nth args 2))
        f3 (case command
             "addi" (:F0 funct3)
             "slti" (:F2 funct3)
             "sltiu" (:F3 funct3)
             "xori" (:F4 funct3)
             "ori" (:F6 funct3)
             "andi" (:F7 funct3)
             "slli" (:F1 funct3)
             "srli" (:F5 funct3)
             "srai" (do (when (>= imm 1024) (set! imm (- imm 1024))) (:F5 funct3)
             "lb" (:F0 funct3)
             "lh" (:F1 funct3)
             "lw" (:F2 funct3)
             "lbu" (:F4 funct3)
             "lhu" (:F5 funct3))]
    {:name command :opcode (if (#{"lb" "lh" "lw" "lbu" "lhu"} command) 
                   (:OPC_3 opcodes) (:OPC_19 opcodes))
     :type :I-type
     :data {:rd rd :funct3 f3 :rs1 rs1 :imm imm}}))

(defn make-s-type [command args]
  (let [rs2 (get-register (first args))
        imm (parse-imm (second args))
        rs1 (get-register (nth args 2))
        f3 (case command
             "sb" (:F0 funct3)
             "sh" (:F1 funct3)
             "sw" (:F2 funct3))]
    {:name command :opcode (:OPC_35 opcodes) :type :S-type
     :data {:funct3 f3 :rs1 rs1 :rs2 rs2 :imm imm}}))

(defn make-b-type [command args]
  (let [rs1 (get-register (first args))
        rs2 (get-register (second args))
        imm (parse-imm (nth args 2))
        f3 (case command
             "beq" (:F0 funct3)
             "bne" (:F1 funct3)
             "blt" (:F4 funct3)
             "bge" (:F5 funct3)
             "bltu" (:F6 funct3)
             "bgeu" (:F7 funct3))]
    {:name command :opcode (:OPC_99 opcodes) :type :B-type
     :data {:funct3 f3 :rs1 rs1 :rs2 rs2 :imm imm}}))

(defn make-u-type [command args opc]
  (let [rd (get-register (first args))
        imm (parse-imm (second args))]
    {:name command :opcode opc :type :U-type
     :data {:rd rd :imm imm}}))

(defn make-j-type [command args]
  (let [rd (get-register (first args))
        imm (parse-imm (second args))]
    {:name command :opcode (:OPC_111 opcodes) :type :J-type
     :data {:rd rd :imm imm}}))

(defn make-jalr [command args]
  (let [rd (get-register (first args))
        rs1 (get-register (second args))
        imm (parse-imm (nth args 2))]
    {:name command :opcode (:OPC_103 opcodes) :type :I-type
     :data {:rd rd :funct3 (:F0 funct3) :rs1 rs1 :imm imm}}))

(defn make-fence [command args]
  (let [pred (first args)
        succ (second args)]
    {:name command :opcode (:OPC_15 opcodes) :type :Fence-type
     :data {:pred pred :succ succ}}))

(defn make-system [command]
  {:name command :opcode (:OPC_115 opcodes) :type :System-type})

(defn make-instruction [command args]
  (cond
    (#{"add" "sub" "sll" "slt" "sltu" "xor" "srl" "sra" "or" "and"
       "mul" "mulh" "mulhsu" "mulhu" "div" "divu" "rem" "remu"} command)
    (make-r-type command args)
    
    (#{"addi" "slti" "sltiu" "xori" "ori" "andi" "slli" "srli" "srai"
       "lb" "lh" "lw" "lbu" "lhu"} command)
    (make-i-type command args)
    
    (#{"sb" "sh" "sw"} command)
    (make-s-type command args)
    
    (#{"beq" "bne" "blt" "bge" "bltu" "bgeu"} command)
    (make-b-type command args)
    
    (#{"lui"} command)
    (make-u-type command args (:OPC_55 opcodes))
    
    (#{"auipc"} command)
    (make-u-type command args (:OPC_23 opcodes))
    
    (#{"jal"} command)
    (make-j-type command args)
    
    (#{"jalr"} command)
    (make-jalr command args)
    
    (#{"fence"} command)
    (make-fence command args)
    
    (#{"ecall" "ebreak" "pause" "fence.tso" "nop"} command)
    (make-system command)))

(defn parse-file [filename]
  (with-open [rdr (io/reader filename)]
    (->> (line-seq rdr)
         (filter #(not (str/blank? %)))
         (map (fn [line]
                (let [tokens (->> (str/split line #"[,\\s]+")
                                  (map trim)
                                  (remove str/blank?))
                      command (first tokens)
                      args (rest tokens)]
                  (make-instruction command args))))
         (vec))))

;; ===================== CPU =====================
(defn execute [state inst]
  (let [pc (:pc state)
        regs (:regs state)
        opcode (:opcode inst)
        type (:type inst)
        data (:data inst)]
    (case opcode
      ;; I-type (OPC_3: LOAD)
      (:OPC_3 opcodes)
      (assoc state :pc (+ pc 4))
      
      ;; I-type (OPC_19: OP-IMM)
      (:OPC_19 opcodes)
      (let [{:keys [rd funct3 rs1 imm]} data
            rs1-val (get regs rs1)
            result (case funct3
                     (:F0 funct3) (to-uint32 (+ (to-sint32 rs1-val) imm)) ; ADDI
                     (:F2 funct3) (if (< (to-sint32 rs1-val) imm) 1 0)    ; SLTI
                     (:F3 funct3) (if (< rs1-val (to-uint32 imm)) 1 0)    ; SLTIU
                     (:F4 funct3) (bit-xor rs1-val (to-uint32 imm))       ; XORI
                     (:F6 funct3) (bit-or rs1-val (to-uint32 imm))        ; ORI
                     (:F7 funct3) (bit-and rs1-val (to-uint32 imm))       ; ANDI
                     (:F1 funct3) (bit-shift-left rs1-val (bit-and imm 0x1F)) ; SLLI
                     (:F5 funct3) (if (zero? (bit-and imm 0x400))
                                     (bit-shift-right rs1-val (bit-and imm 0x1F)) ; SRLI
                                     (bit-shift-right (to-sint32 rs1-val) (bit-and imm 0x1F)))] ; SRAI
        (-> state
            (assoc :pc (+ pc 4))
            (assoc-in [:regs rd] (if (zero? rd) (get regs rd) result))))
      
      ;; R-type (OPC_51: OP)
      (:OPC_51 opcodes)
      (let [{:keys [rd funct3 rs1 rs2 funct7]} data
            rs1-val (get regs rs1)
            rs2-val (get regs rs2)
            srs1 (to-sint32 rs1-val)
            srs2 (to-sint32 rs2-val)
            result (case [funct3 funct7]
                     [(:F0 funct3) (:F0_7 funct7)] (to-uint32 (+ srs1 srs2)) ; ADD
                     [(:F0 funct3) (:F32_7 funct7)] (to-uint32 (- srs1 srs2)) ; SUB
                     [(:F0 funct3) (:F1_7 funct7)] (to-uint32 (* srs1 srs2)) ; MUL
                     [(:F1 funct3) (:F0_7 funct7)] (bit-shift-left rs1-val (bit-and rs2-val 0x1F)) ; SLL
                     [(:F1 funct3) (:F1_7 funct7)] (to-uint32 (bit-shift-right (* srs1 srs2) 32)) ; MULH
                     [(:F2 funct3) (:F0_7 funct7)] (if (< srs1 srs2) 1 0) ; SLT
                     [(:F2 funct3) (:F1_7 funct7)] (to-uint32 (bit-shift-right (* srs1 (to-sint32 (bit-and rs2-val 0xFFFFFFFF))) 32)) ; MULHSU
                     [(:F3 funct3) (:F0_7 funct7)] (if (< rs1-val rs2-val) 1 0) ; SLTU
                     [(:F3 funct3) (:F1_7 funct7)] (to-uint32 (bit-shift-right (* rs1-val rs2-val) 32)) ; MULHU
                     [(:F4 funct3) (:F0_7 funct7)] (bit-xor rs1-val rs2-val) ; XOR
                     [(:F4 funct3) (:F1_7 funct7)] (if (zero? rs2-val) 
                                                     (to-uint32 -1) 
                                                     (to-uint32 (quot srs1 srs2))) ; DIV
                     [(:F5 funct3) (:F0_7 funct7)] (bit-shift-right rs1-val (bit-and rs2-val 0x1F)) ; SRL
                     [(:F5 funct3) (:F32_7 funct7)] (to-uint32 (bit-shift-right srs1 (bit-and rs2-val 0x1F))) ; SRA
                     [(:F5 funct3) (:F1_7 funct7)] (if (zero? rs2-val) 
                                                     (to-uint32 -1) 
                                                     (quot rs1-val rs2-val)) ; DIVU
                     [(:F6 funct3) (:F0_7 funct7)] (bit-or rs1-val rs2-val) ; OR
                     [(:F6 funct3) (:F1_7 funct7)] (if (zero? rs2-val) 
                                                     rs1-val 
                                                     (to-uint32 (rem srs1 srs2))) ; REM
                     [(:F7 funct3) (:F0_7 funct7)] (bit-and rs1-val rs2-val) ; AND
                     [(:F7 funct3) (:F1_7 funct7)] (if (zero? rs2-val) 
                                                     rs1-val 
                                                     (rem rs1-val rs2-val)))] ; REMU
        (-> state
            (assoc :pc (+ pc 4))
            (assoc-in [:regs rd] (if (zero? rd) (get regs rd) result))))
      
      ;; U-type (LUI, AUIPC)
      (if (#{(:OPC_55 opcodes) (:OPC_23 opcodes)} opcode)
        (let [{:keys [rd imm]} data
              result (if (= opcode (:OPC_55 opcodes))
                       (bit-shift-left (to-uint32 imm) 12) ; LUI
                       (to-uint32 (+ pc (bit-shift-left (to-uint32 imm) 12))))] ; AUIPC
          (-> state
              (assoc :pc (+ pc 4))
              (assoc-in [:regs rd] (if (zero? rd) (get regs rd) result))))
      
      ;; B-type
      (:OPC_99 opcodes)
      (let [{:keys [funct3 rs1 rs2 imm]} data
            rs1-val (get regs rs1)
            rs2-val (get regs rs2)
            jump? (case funct3
                    (:F0 funct3) (= rs1-val rs2-val) ; BEQ
                    (:F1 funct3) (not= rs1-val rs2-val) ; BNE
                    (:F4 funct3) (< (to-sint32 rs1-val) (to-sint32 rs2-val)) ; BLT
                    (:F5 funct3) (>= (to-sint32 rs1-val) (to-sint32 rs2-val)) ; BGE
                    (:F6 funct3) (< rs1-val rs2-val) ; BLTU
                    (:F7 funct3) (>= rs1-val rs2-val))] ; BGEU
        (assoc state :pc (if jump? 
                           (to-uint32 (+ pc imm)) 
                           (+ pc 4))))
      
      ;; J-type (JAL)
      (:OPC_111 opcodes)
      (let [{:keys [rd imm]} data]
        (-> state
            (assoc :pc (to-uint32 (+ pc imm)))
            (assoc-in [:regs rd] (if (zero? rd) (get regs rd) (+ pc 4)))))
      
      ;; I-type (JALR)
      (:OPC_103 opcodes)
      (let [{:keys [rd rs1 imm]} data
            base (get regs rs1)
            target (to-uint32 (+ (to-sint32 base) imm))]
        (-> state
            (assoc :pc (bit-and target 0xFFFFFFFE))
            (assoc-in [:regs rd] (if (zero? rd) (get regs rd) (+ pc 4)))))
      
      (assoc state :pc (+ pc 4)))))

(defn run-cpu [instructions]
  (loop [state {:pc 0 :regs (vec (repeat 32 0))}]
    (let [idx (quot (:pc state) 4)]
      (if (>= idx (count instructions))
        state
        (recur (execute state (nth instructions idx))))))

;; ===================== Main =====================
(defn -main [& args]
  (let [asm-file (second (drop-while #(not= "--asm" %) args))
        instructions (parse-file asm-file)
        result-state (run-cpu instructions)]
    (println "Final PC:" (:pc result-state))
    (println "Registers:")
    (doseq [[i val] (map-indexed vector (:regs result-state))]
      (println (str "x" i ":\t" val)))))


