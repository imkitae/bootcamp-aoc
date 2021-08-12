(ns aoc2018-1
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str]))

;; #################################
;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
;; #################################
(def part1-sample-file "resources/day1-part1.sample.txt")

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
(def answer (atom 0))
(defn add-line
  "answer에 합산된 값을 계속 swap!으로 반영"
  [input]
  (swap! answer + input))

(parse1 part1-sample-file add-line)

;; 2) reduce를 사용해본 것
(defn add-all
  "inputs의 요소들을 int파싱해서 합산하는 reduce 적용"
  [inputs]
  (reduce + 0 inputs))

(parse2 part1-sample-file add-all)

;; 3) 리뷰 내용 반영
;; - slurp 사용
;; - thread macro 적용
(defn read-file
  "파일을 읽어 각 라인을 vector에 담아 리턴"
  [path]
  (str/split-lines (slurp path)))

(defn convert-to-int
  "collection의 문자열들을 정수로 파싱한 collection 리턴"
  [lines]
  (map #(Integer/parseInt %) lines))

(defn sum-up
  "collection 요소들의 총 합 계산"
  [data]
  (reduce + 0 data))

(defn print-sum-up
  "누적 합 결과 출력"
  [data]
  (println (format "전체 누적 합은 %d 입니다." data)))

(-> part1-sample-file
    read-file
    convert-to-int
    sum-up
    print-sum-up)



;; #################################
;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
;; #################################
(def part2-sample-file "resources/day1-part2.sample.txt")

(defn in-old?
  "col에 el과 동일한 요소가 포함되어 있는지에 대해 true/false"
  [col el] (boolean (some #{el} col)))

(defn get-duplicated-sum-old
  "loop recur을 사용해서 누적 합계를 계산하고 저장한다. 그리고 이전에 저장된 결과 중 동일한게 있으면 리턴"
  [nums]
  (loop [i 0
         old-sums #{}
         sum 0]

    (print "num:" (nth nums i))
    (print " / sum:" sum)
    (println " / old-sums:" old-sums)

    (if (in-old? old-sums sum)
      sum
      (recur (mod (inc i) (count nums))
             (conj old-sums sum)
             (+ sum (nth nums i)))))) ;; (nth coll i) => index를 안쓰는 방법으로 바꿔보자! -> reduce

;; index -> (nth coll (- i 1)) 압수

;; (take 10 (cycle [1 2 3])) => [1 2 3 1 2 3 1 2 3 1]
;; [1 2 3 4 ..] | 1 / [2 3 4 ...] ... inf loop, 지연평가
(parse2 part2-sample-file get-duplicated-sum-old)

;; 2) 리뷰 적용해본 것 
;; - thread macro 적용
;; - in?, get-duplicated-sum 개선
(defn in?
  "boolean 제거"
  [col el] (some #{el} col))

(defn get-duplicated-sum
  "- print문 제거
   - index를 사용하지 않는 방법으로 변경"
  [nums]
  (loop [items nums
         old-sums #{}
         sum 0] 
    (if (in? old-sums sum)
      sum
      (recur (rest (cycle items))
             (conj old-sums sum)
             (+ sum (first items))))))

(defn print-duplicated-sum
  "중복 발생 누적 합 결과 출력"
  [data]
  (println (format "중복 발생한 누적 합은 %d 입니다." data)))

(-> part2-sample-file
    read-file
    convert-to-int
    get-duplicated-sum
    print-duplicated-sum)

;; #################################
;; ###        Refactoring        ###
;; #################################

;; cycle 혹은 reductions 사용하기
;; loop-recur 시 let으로 바인딩하기
;; 
