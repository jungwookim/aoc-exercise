(ns aoc.2024.day17)

;; Register A: 61657405
;; Register B: 0
;; Register C: 0

;; Program: 2 4 1 2 7 5 4 3 0 3 1 7 5 5 3 0

(def initial-state
  {:registers {:A 729 :B 0 :C 0}  ; 레지스터 값
;;    :program [2 4 1 2 7 5 4 3 0 3 1 7 5 5 3 0]         ; 프로그램
   :program [0 1 5 4 3 0]
   :ip 0                          ; 명령어 포인터
   :output []})                   ; 출력값 저장

;; 콤보 타입 피연산자의 값을 계산하는 함수
(defn get-combo-value [operand registers]
  (case operand
    4 (:A registers)
    5 (:B registers)
    6 (:C registers)
    operand)) ; 리터럴 값(0~3)은 그대로 반환

(defn adv [state operand]
  (let [registers (:registers state)
        divisor (Math/pow 2 (get-combo-value operand registers))
        new-a (int (/ (:A registers) divisor))]
    (-> state
        (assoc-in [:registers :A] new-a)
        (update :ip + 2)))) ; 명령어 포인터 증가

(defn bxl [state operand]
  (let [registers (:registers state)
        new-b (bit-xor (:B registers) operand)]
    (-> state
        (assoc-in [:registers :B] new-b)
        (update :ip + 2))))

(defn bst [state operand]
  (let [registers (:registers state)
        value (mod (get-combo-value operand registers) 8)]
    (-> state
        (assoc-in [:registers :B] value)
        (update :ip + 2))))

(defn jnz [state operand]
  (let [registers (:registers state)]
    (if (zero? (:A registers))
      (update state :ip + 2)
      (assoc state :ip operand))))

(defn bxc [state _]
  (let [registers (:registers state)
        new-b (bit-xor (:B registers) (:C registers))]
    (-> state
        (assoc-in [:registers :B] new-b)
        (update :ip + 2))))

(defn out [state operand]
  (let [registers (:registers state)
        value (mod (get-combo-value operand registers) 8)]
    (-> state
        (update :output conj value)
        (update :ip + 2)))) ; 명령어 포인터 증가

(defn bdv [state operand]
  (let [registers (:registers state)
        divisor (Math/pow 2 (get-combo-value operand registers))
        new-b (int (/ (:A registers) divisor))]
    (-> state
        (assoc-in [:registers :B] new-b)
        (update :ip + 2))))

(defn cdv [state operand]
  (let [registers (:registers state)
        divisor (Math/pow 2 (get-combo-value operand registers))
        new-c (int (/ (:A registers) divisor))]
    (-> state
        (assoc-in [:registers :C] new-c)
        (update :ip + 2))))

(defn execute-instruction [state]
  (let [program (:program state)
        ip (:ip state)
        opcode (nth program ip)
        operand (nth program (inc ip))]
    (case opcode
      0 (adv state operand)
      1 (bxl state operand)
      2 (bst state operand)
      3 (jnz state operand)
      4 (bxc state operand)
      5 (out state operand)
      6 (bdv state operand)
      7 (cdv state operand)
      state))) ; 알 수 없는 명령어가 오면 상태 그대로 반환

(defn run-program [initial-state]
  (->> (iterate execute-instruction initial-state) ; 초기 상태에서 시작
       (take-while #(let [ip (:ip %)
                          program (:program %)]
                      (< ip (count program)))) ; 프로그램 끝에 도달하기 전까지 실행
       last))

(defn part1 []
  (let [final-state (run-program initial-state)]
    (clojure.string/join "," (:output final-state))))

(comment
  (part1)
  )