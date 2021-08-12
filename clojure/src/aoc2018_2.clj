(ns aoc2018-2
  (:require [clojure.string :as str])
  (:require [clojure.data :as data]))

;; #################################
;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12
;; #################################

(def part1-sample-file "resources/day2-part1.sample.txt")

(defn read-file
  "파일을 읽어 각 라인을 vector에 담아 리턴"
  [path]
  (str/split-lines (slurp path)))

(defn n-repeats?
  "str에 n번 반복되는 문자가 존재하는지 여부 (있으면 n, 없으면 nil 리턴)"
  [str n]
  (some #{n} (vals (frequencies str))))

(defn map-to-repeats
  "주어진 문자열들을 2, 3번 반복되는 문자가 있는지 여부를 나타내는 [2-or-nil, 3-or-nil] 형태로 변환
   - 2번 반복되는 문자만 있음 -> [2, nil]
   - 3번 반복되는 문자만 있음 -> [nil, 3]
   - 2번 반복되는 문자, 3번 반복되는 문자 다 있음 -> [2, 3]"
  [str-list]
  (map #(vector (n-repeats? % 2) (n-repeats? % 3)) str-list))

(defn reduce-repeats
  "각 문자열들에 map-to-repeats 호출해서 얻어진 [2-or-nil, 3-or-nil] 결과들을 취합하여
   전체 중 2반복 문자 수, 3반복 경우 수를 [int, int] 형태로 리턴"
  [repeats-list]
  (reduce
   (fn [prev repeats]
     (vector
      (+ (prev 0) (if (repeats 0) 1 0))
      (+ (prev 1) (if (repeats 1) 1 0))))
   [0 0]
   repeats-list))

(defn print-result
  "결과 출력"
  [repeats-sum]
  (println
   (format "동일 문자가 2번, 3번 나오는 경우의 총 합은 각각 %d, %d이고 이 두 수의 곱은 %d 입니다."
           (repeats-sum 0)
           (repeats-sum 1)
           (* (repeats-sum 0) (repeats-sum 1)))))

(->> part1-sample-file
     read-file
     map-to-repeats
     reduce-repeats
     print-result)

;; #################################
;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.
;; #################################

(def part2-sample-file "resources/day2-part2.sample.txt")

(defn same-parts-if-diff-one
  "두 문자열이 한 글자만 다른 경우 그 부분을 제외한 같은 부분을 리턴하고, 그렇지 않으면 nil 리턴
   문자열을 vector로 변환 후 diff를 호출해서 차집합들과 교집합을 구한다.
   https://clojuredocs.org/clojure.data/diff"
  [str1 str2]
  (let [vec1 (str/split str1 #"")
        vec2 (str/split str2 #"")
        diff (data/diff vec1 vec2)
        only-in-1 (remove nil? (diff 0))
        only-in-2 (remove nil? (diff 1))]
    (if (and (== (count only-in-1) 1) (== (count only-in-2) 1))
      (str/join (diff 2))
      nil)))

(defn find-same-parts
  "col에서 str과 한 글자만 다른 문자열을 찾아서 같은 부분 리턴, 못 찾은 경우 nil 리턴"
  [str col]
  (loop [items col]
    (if-let [same-part (same-parts-if-diff-one str (first items))]
      same-part
      (when (> (count items) 1) (recur (rest items))))))

(defn find-same-parts-for-each
  "col의 각 문자열들을 서로 비교해서 한 글자만 다른 문자열 쌍을 찾으면 같은 부분을 리턴, 못 찾은 경우 nil 리턴"
  [col]
  (loop [target-col col]
    (let [first-one (first target-col)
          rest-col (rest target-col)
          found (find-same-parts first-one rest-col)]
      (if (or found (== 1 (count rest-col)))
        found
        (recur rest-col)))))

(-> part2-sample-file
    read-file
    find-same-parts-for-each)

;; #################################
;; ###        Refactoring        ###
;; #################################

;; frequencies 사용하기
;; PPAP (parse-process-aggregate-print) 원칙 따르기
;; declarative 한 함수 이름 사용하기
