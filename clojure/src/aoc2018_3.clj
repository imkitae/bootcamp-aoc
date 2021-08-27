(ns aoc2018_3
  (:require [clojure.string :as str]
            [clojure.set :as set]))


;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def sample-file "resources/day3.sample.txt")

(defn read-file
  "파일을 읽어 각 라인을 vector에 담아 리턴"
  [path]
  (str/split-lines (slurp path)))

(defn parse-line
  "`#1 @ 1,3: 4x4` 형태의 문자열 한 줄을 {:id :x :y :w :h} 형태의 맵으로 변환"
  [str]
  (let [tokens (str/split str #"[#@:\s]+")
        id (Integer/parseInt (tokens 1))
        [x y] (map #(Integer/parseInt %) (str/split (tokens 2) #","))
        [w h] (map #(Integer/parseInt %) (str/split (tokens 3) #"x"))]
    (->> (hash-map :id id, :x x, :y y, :w w, :h h))))

(defn parse-to-rects [lines]
  (map parse-line lines))

(defn rect-to-coords
  "rect 데이터를 좌표들의 집합인, x-y vector set으로 변환"
  [{x :x, y :y, w :w, h :h}]
  (set
   (for [y-pos (range y (+ y h))
         x-pos (range x (+ x w))]
     (vector x-pos y-pos))))

(defn convert-to-coords [rects]
  (map rect-to-coords rects))

(defn coord-intersections
  "coord와 others 내 각 coord들과 intersection을 계산한 결과들을 리턴 (1:N)"
  [coord others]
  (for [other others] (set/intersection coord other)))

(defn all-intersections
  "coords의 각 coord들의 모든 intersection을 계산한 결과들을 리턴 (N:M)"
  [coords]
  (loop [coord (first coords)
         others (rest coords)
         result []]
    (if (empty? others)
      result
      (recur (first others)
             (rest others)
             (concat result (coord-intersections coord others))))))

(defn unify-intersections [intersections]
  (reduce set/union intersections))

(defn print-part1-result [coords]
  (format "겹치는 지역의 갯수는 %d 입니다." (count coords)))

(->> sample-file
     read-file
     parse-to-rects
     convert-to-coords
     all-intersections
     unify-intersections
     print-part1-result)


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn intersect-rect?
  "두 rect의 겹침 여부를 리턴
   두 rect가 동일 rect면 체크가 유효하지 않다는 의미에서 (false)로 계산"
  [rect1 rect2]
  (if (== (rect1 :id) (rect2 :id))
    false ;; if rects are same, return false
    (not (empty? (set/intersection (rect-to-coords rect1) (rect-to-coords rect2))))))

(defn all-intersection-maps
  "rects의 각 rect들에 대해 겹침 여부 결과를 취합해서 리턴
   
   ex) 리턴값 예시
   ({:id 1, :intersection? [false true false]},
    {:id 2, :intersection? [true false false]},
    {:id 3, :intersection? [false false false]})
   
   -> ID 1인 rect는 ID 2와 겹치고, (false, true, false)
      ID 2인 rect는 ID 1와 겹치고, (true, false, false)
      ID 3인 rect는 겹치지 않는다. (false, false, false)"
  [rects]
  (for [rect1 rects]
    {:id (:id rect1)
     :intersection? (for [rect2 rects] (intersect-rect? rect1 rect2))}))

;; (defn not-intersect-map? [intersection-map]
;;   (every? false? (:intersection? intersection-map)))

(defn intersect-exists? [intersection-map]
  (some true? (:intersection? intersection-map)))

(defn print-part2-result [intersection-maps]
  (format
   "겹치는 않는 영역을 가진 ID는 %d 입니다."
   ;; (:id (first (filter not-intersect-map? intersection-maps)))))
   (:id (some #(if (intersect-exists? %) nil %) intersection-maps)))) ;; threading macro


(->> sample-file
     read-file
     parse-to-rects
     all-intersection-maps
     print-part2-result)

