(ns aoc2018_5
  (:require [clojure.string :as str]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(def sample-file "resources/day5-part1.sample.txt")

(defn read-file [path]
  (str/split-lines (slurp path)))

(defn reactable? [a b]
  (and
   (= ((fnil str/upper-case "") a) ((fnil str/upper-case "") b))
   (not= a b)))

(defn reacted-old [str]
  (if (< (count str) 2)
    str
    (let [head (first str)
          tail (subs str 1)]
      (if (reactable? head (first tail))
        (reacted-old (subs tail 1))
        (reacted-old (str/join [head (reacted-old tail)]))))))

(defn reacted [str]
  (reduce
   (fn [acc ch]
     (if (reactable? (last acc) ch)
       (str/join (drop-last acc))
       (str/join [acc ch])))
   "" str))

(defn parse-to-react [lines]
  (map reacted lines))

(defn print-part1-result [reacted-lines]
  (format "최종으로 남는 문자열 길이는 %d 입니다." (count (first reacted-lines))))

(->> sample-file
     read-file
     parse-to-react
     print-part1-result)

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(def abcd (vec "abcdefghijklmnopqrstuvwxyz"))

(defn reacted-ignore [str ignore]
  (reduce
   (fn [acc ch]
     (if (or (= ignore ch) (= (first (str/upper-case ignore)) ch))
       acc
       (if (reactable? (last acc) ch)
         (str/join (drop-last acc))
         (str/join [acc ch]))))
   "" str))

(defn min-reacted [str ignores]
  (reduce
   (fn [min-val ignore]
     (min (count (reacted-ignore str ignore)) min-val))
   Integer/MAX_VALUE ignores))

(defn print-part2-result [reacted-lines]
  (format "한 유닛 소거 후 가장 짧은 문자열 길이는 %d 입니다." (min-reacted (first reacted-lines) abcd)))

(->> sample-file
     read-file
     parse-to-react
     print-part2-result)
