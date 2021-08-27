(ns aoc2018_6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(def sample-file "resources/day6-part1.sample2.txt")

(defn read-file [path]
  (str/split-lines (slurp path)))

(defn parse-to-all-area [file]
  (->> (read-file file)
       (map
        #(re-find #"(\d+), (\d+)" %))
       (map rest)
       (map
        (fn [[x y]]
          [(Integer/parseInt x) (Integer/parseInt y)]))))

(defn xy-minmax
  "주어진 좌표들 중 최소 최대인 x, y값을 리턴"
  [coords]
  (let [[x-min _] (apply min-key first coords)
        [x-max _] (apply max-key first coords)
        [_ y-min] (apply min-key second coords)
        [_ y-max] (apply max-key second coords)]
    {:x-min x-min
     :x-max x-max
     :y-min y-min
     :y-max y-max}))

(defn minmax-to-area
  "x y축 각 최소 최대 좌표를 받아 그 4 좌표 내에 속하는 영역의 모든 좌표를 리턴"
  [{:keys [x-min x-max y-min y-max]}]
  (for [y (range y-min (inc y-max))
        x (range x-min (inc x-max))]
    [x y]))

(defn manhattan-dist
  "두 좌표 간 맨하튼 거리를 리턴"
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn nearest-in-coords
  "coords의 좌표들 중 origin까지의 맨하튼 거리가 최소인 좌표를 리턴.
   만약 최소 거리가 동일한 좌표가 둘 이상이면 nil 리턴"
  [origin coords]
  (let [mapped-coords (map #(hash-map :manh-dist (manhattan-dist origin %)
                                      :coord %) coords)
        ordered-coords (sort-by :manh-dist mapped-coords)]
    (when-not (== (:manh-dist (first ordered-coords))
                  (:manh-dist (second ordered-coords)))
      (:coord (first ordered-coords)))))

(defn boundary-values
  "직사각 영역 형태로 배치된 값의 리스트에서 사각형 네 변에 위치한 값들의 종류를 리턴
   ex) [1 2 3 4 5 6] 너비 3 높이 2
    -> [1 2 3
        4 5 6] 사각형 형태의 각 변의 값들
    -> [1 2 3] [1 4] [4 5 6] [3 6 3]"
  [rect-values width]
  (let [horizontals (partition width rect-values)
        top (first horizontals)
        left (map first horizontals)
        bottom (last horizontals)
        right (map last horizontals)]
    [top left bottom right]))

(defn nearest-coords-area
  "1) coords의 좌표들의 xy 최대최소 값으로 사각 영역을 정의
   2) 그 사각 영역의 모든 좌표들을 coords 좌표들 중 맨하튼 거리가 최소인 coords의 좌표(=nearest-in-coords)로 맵핑
   3) nearest-in-coords로 맵핑된 영역의 네 변에 위치판 좌표 리스트들을 구함"
  [coords]
  (let [coords-xy-minmax (xy-minmax coords)
        coords-area-width (inc (- (:x-max coords-xy-minmax) (:x-min coords-xy-minmax)))
        coords-area (minmax-to-area coords-xy-minmax)]
    [(map #(nearest-in-coords % coords) coords-area)
     coords-area-width]))

(defn infinite-coords
  "# Input
   - nearest-coords-area: 주어진 좌표들과 xy 최대최소 영역까지의 맨하튼거리 결과 리스트
   - width: 영역의 너비
   # Output
   - 영역 경계를 구성하는 좌표들의 종류 (= 무한 맨하튼거리 영역을 구성하는 좌표)"
  [nearest-coord-area width]
  (->> (boundary-values nearest-coord-area width)
       (apply concat)
       (filter some?)
       distinct))

(defn finite-coords
  "무한 맨하튼거리 영역 좌표 = 전체 좌표셋 - 유한 맨하튼거리 영역 좌표셋"
  [coords infinite-coords]
  (set/difference (into #{} coords) (into #{} infinite-coords)))

(defn finite-area-max-size
  "# Input
   - nearest-coord-area: 최소 맨하튼거리 영역 좌표 리스트
   - finite-coords: 유한 맨하튼거리 영역을 구성하는 좌표 리스트
   # Ouput
   유한 맨하튼거리 영역 중 최대 사이즈"
  [nearest-coord-area finite-coords]
  (->> nearest-coord-area
       (filter finite-coords)
       frequencies
       (apply max-key second)))

(defn solve1
  [coords]
  (let [[nearest-area width] (nearest-coords-area coords)
        infinite-area (infinite-coords nearest-area width)
        finite-area (finite-coords nearest-area infinite-area)]
    (finite-area-max-size nearest-area finite-area)))

(defn print-part1-result [[[x y] area-size]]
  (format "가장 큰 유한한 면적은 [%d %d]에 가까운 좌표들로 이루어져 있으며, 크기는 %d 입니다."
          x y area-size))

(->> (parse-to-all-area sample-file)
     solve1
     print-part1-result)

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 이하인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.  
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn manh-dist-sum-coords
  "coords의 좌표들의 origin까지의 맨하튼 거리 총합을 리턴"
  [origin coords]
  (reduce (fn [sum coord]
            (+ sum (manhattan-dist origin coord))) 0 coords))

;; sum 가변인자를 이용하면 reduce 합을 안써도 map & (apply + )

(defn safe-area-size
  "전체 좌표 중 모든 coords 좌표들과의 맨하튼 거리 총 합이 safe-limit 미만인 좌표 개수를 리턴"
  [safe-limit manh-dist-sum-coords]
  (->> manh-dist-sum-coords
       (filter #(< % safe-limit))
       count))

(defn solve2
  [coords]
  (let [coords-xy-minmax (xy-minmax coords)
        coords-area (minmax-to-area coords-xy-minmax)
        manh-dist-sum-coord-area (map #(manh-dist-sum-coords % coords) coords-area)]
    (safe-area-size 10000 manh-dist-sum-coord-area)))

(defn print-part2-result [area-size]
  (format "N이 10000 미만인 안전한 지역의 사이즈는 %d 입니다." area-size))

(->> (parse-to-all-area sample-file)
     solve2
     print-part2-result)
