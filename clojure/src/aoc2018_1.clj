(ns aoc2018-1
  (:require [clojure.java.io :as io]))

;; #################################
;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
;; #################################

(def sample-file "resources/day1.sample.txt")

(defn parse1
  "파일을 읽고 deseq로 내용 한 줄마다 proc-line에 전달해서 반복 호출"
  [path proc-line]
  (with-open [rdr (io/reader path)]
    (doseq [line (line-seq rdr)]
      (proc-line (Integer/parseInt line)))))

(defn parse2
  "파일을 읽고 파일 내용 전체를 sequence로 받아 proc-all에 전달"
  [path proc-all]
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)
          nums (map #(Integer/parseInt %) lines)]
      (proc-all nums))))

;; 1) atom + swap!으로 풀어본 것
(comment
  (def answer (atom 0))

  (defn add-line
    "answer에 합산된 값을 계속 swap!으로 반영"
    [input]
    (swap! answer + input))

  (parse1 sample-file add-line)
  )

;; 2) reduce를 사용해본 것
(comment  
  (defn add-all
    "inputs의 요소들을 int파싱해서 합산하는 reduce 적용"
    [inputs]
    (reduce + 0 inputs))
  
  (parse2 sample-file add-all)
)


;; #################################
;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; #################################

;;
(comment
  (defn in?
    "col에 el과 동일한 요소가 포함되어 있는지에 대해 true/false"
    [col el] (boolean (some #{el} col)))
  
  (defn get-twice
    "loop recur을 사용해서 누적 합계를 계산하고 저장한다. 그리고 이전에 저장된 결과 중 동일한게 있으면 리턴"
    [nums]
    (loop [i 0
           old-sums #{}
           sum 0]
      
      (print "num:" (nth nums i))
      (print " / sum:" sum)
      (println " / old-sums:" old-sums)

      (if (in? old-sums sum)
        sum
        (recur (mod (inc i) (count nums))
               (conj old-sums sum)
               (+ sum (nth nums i))))))

  (parse2 sample-file get-twice)
)

  
;; #################################
;; ###        Refactoring        ###
;; #################################

;; cycle 혹은 reductions 사용하기
;; loop-recur 시 let으로 바인딩하기