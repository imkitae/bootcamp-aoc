(ns aoc2018_7
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

;; 파트 1
;; 스케줄이 주어질 때, 일이 처리되는 순서를 반환하시오. 알파벳 캐릭터 하나로 대표되는 일(work)이 주어지고, 각 일을 처리하기 위해서 선행되어야 하는 일들이 스케줄 형식으로 주어짐.

;; Step C must be finished before step A can begin.
;; Step C must be finished before step F can begin.
;; Step A must be finished before step B can begin.
;; Step A must be finished before step D can begin.
;; Step B must be finished before step E can begin.
;; Step D must be finished before step E can begin.
;; Step F must be finished before step E can begin.
;; 위와 같은 입력의 경우 아래 형태의 그래프가 만들어짐.

;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;; 순서는 아래와 같음.

;; 처음엔 C만 가능함. C에서 시작. 만약 다른 시작점이 존재한다면 알파벳 순서로 진행.
;; C 다음으로 A와 F가 가능한데, A가 알파벳 우선순위가 높으므로 A로 감.
;; A 를 완료하면 B, D, F가 가능한데, 역시 알파벳 우선순위가 높은 B가 수행됨.
;; 남은 D와 F중에서 D가 수행됨
;; F가 수행됨
;; E가 수행됨 (E는 B, D, F 모두가 선행되어야 수행될 수 있음)
;; 결과: CABDFE

(def sample-file "resources/day7-part1.sample2.txt")

(defn read-file [path]
  (str/split-lines (slurp path)))

(defn parse-to-pairs
  "file에서 읽은 문장들을 의존 관계 리스트 ((A B) (A C) ..) 형식으로 파싱
   ((A B)의 의미는 B를 수행하는데 A가 선행되어야 한다는 의미)"
  [file]
  (->> (read-file file)
       (map
        #(re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." %))
       (map rest)
       (map #(map keyword %))))

(defn pair-to-child-parent-map
  "의존 관계 리스트를 취합하여 해시맵으로 표현
   키에 해당하는 작업을 하려면 선행되어야 할 작업 리스트를 값으로 설정
   (예를 들어 {:A #{:C}, :B #{:A}, :C #{}} 의 의미는, 
   A 이전에 C가 선행되어야 하고, B를 하는데 A가 선행되어야 하며, C는 제일 처음에 실행 가능하다는 의미)"
  [pairs]
  (reduce (fn [m [parent child]]
            (let [parent-key (keyword parent)
                  child-key (keyword child)]
              (-> m
                  (update parent-key #(or % #{}))
                  (update child-key #(conj (or % #{}) parent-key)))))
          {} pairs))

(defn empty-value-keys
  "src-map에서 empty?가 true인 값을 가지는, nil이 아닌 키들만 리스트로 리턴"
  [src-map]
  (->> src-map
       (filter #((complement nil?) (second %)))
       (filter #(empty? (second %)))
       (map first)))

(defn init-state [child-parent-map]
  {:child-parent child-parent-map
   :result []})

(defn next-to-do
  "작업 의존 맵을 보고 지금 수행 가능한 작업의 리스트를 리턴
   지금 수행 가능한 작업 = parent가 empty인 작업"
  [child-parent-map]
  (->> (empty-value-keys child-parent-map)
       sort
       first))

(defn update-child-parent
  "to-do 작업을 수행하고 난 뒤의 의존 맵 상태를 리턴
   child-parent-map의 키에서 to-do가 있다면 제거하고, 값에서 to-do를 포함한 경우 제거"
  [child-parent-map to-do]
  (->> child-parent-map
       (filter (fn [[child _]] (not= child to-do)))
       (map (fn [[child parents]]
              [child (filter #(not= % to-do) parents)]))
       (into {})))

(defn work
  "state를 받아 규칙에 맞게 해야할 다음 일을 진행한 state 리턴"
  [{:keys [result child-parent]}]
  (let [todo (next-to-do child-parent)]
    {:result (conj result todo)
     :child-parent (update-child-parent child-parent todo)}))

(defn solve1 [child-parent-map]
  (->> (init-state child-parent-map)
       (iterate #(work %))
       (drop-while #(not-empty (:child-parent %)))
       first
       :result
       (map name)
       str/join))

(defn print-part1-result [result]
  (format "일을 처리하는 순서는 %s 입니다." result))

(->> (parse-to-pairs sample-file)
     pair-to-child-parent-map
     solve1
     print-part1-result)


;; 파트 2
;; 파트 1에서는 일을 워커(worker)가 하나였지만, 파트 2는 5명. 즉, 동시에 5개의 일을 처리할 수 있게 됨.
;; 그리고 각각의 일 (A, Z)은 처리하는데 (60+1, 60+26)의 시간이 걸림. B는 62초, D는 64초, etc.

;; 이 때, 주어진 모든 일을 처리하는데 걸리는 시간을 구하시오.

;; 아래는 파트 1의 예시에서 워커가 2명이 된 케이스임.

;; Second   Worker 1   Worker 2   Done
;;    0        C          .        
;;    1        C          .        
;;    2        C          .        
;;    3        A          F       C
;;    4        B          F       CA
;;    5        B          F       CA
;;    6        D          F       CAB
;;    7        D          F       CAB
;;    8        D          F       CAB
;;    9        D          .       CABF
;;   10        E          .       CABFD
;;   11        E          .       CABFD
;;   12        E          .       CABFD
;;   13        E          .       CABFD
;;   14        E          .       CABFD
;;   15        .          .       CABFDE

(def MAX-WORKER-NUM 5)
(def A-TIME 61)

(defn work-remains []
  (->> (range (int \A) (inc (int \Z)))
       (map #(vector
              (keyword (str (char %)))
              (+ A-TIME (- % (int \A)))))
       (into {})))

(defn init-state2 [child-parent-map]
  {:time -1
   :child-parent child-parent-map
   :remains (work-remains)
   :working #{}
   :result []})

(defn update-child-parent2
  ""
  [child-parent-map to-dos]
  (->> child-parent-map
       (filter (fn [[child _]] (not (child to-dos))))
       (map (fn [[child parents]]
              [child (set/difference parents to-dos)]))
       (into {})))

(defn next-works [child-parent-map]
  (->> (empty-value-keys child-parent-map)
       sort))

(defn get-finished-works [remains working]
  (->>  remains
        (filter (fn [[work time]] (and (work working) (= 0 time))))
        (map first)
        (into #{})))

(defn update-remains [remains working]
  (->> remains
       (map (fn [[work time]]
         [work
          (if (some #(= work %) working) (max (dec time) 0) time)]))
       (into {})))

;; 시작된 일 구하기
;; 끝난 일 구하기
(defn work2 [{:keys [child-parent
                     remains
                     working
                     result
                     time]}]
  (let [now-remains (update-remains remains working)
        finished-works (get-finished-works now-remains working)
        now-child-parent (update-child-parent2 child-parent finished-works)
        workers-available (+ (- MAX-WORKER-NUM (count working)) (count finished-works))
        works-available (next-works now-child-parent)
        works-to-do (set (take workers-available (sort (seq (set/difference (set works-available) working)))))]
    {:child-parent now-child-parent
     :remains now-remains
     :working (set/union (set/difference working finished-works) works-to-do)
     :result (concat result finished-works)
    ;;  :finished finished-works
    ;;  :work-available work-available
    ;;  :workers-available workers-available
    ;;  :to-dos to-dos
     :time (inc time)}))

;; (->> {:child-parent {:C #{}, :A #{:C}, :F #{:C}, :B #{:A}, :D #{:A}, :E #{:F :D :B}}
;;       ;; :remains {:L 12, :M 13, :I 9, :R 18, :O 15, :A 1, :F 6, :W 23, :Q 17, :P 16
;;       ;;           :D 4, :B 2, :J 10, :Z 26, :T 20, :C 3, :E 5, :G 7, :Y 25, :X 24, :H 8
;;       ;;           :V 22, :U 21, :S 19, :N 14, :K 11}
;;       :remains {:L 72 :M 73 :I 69 :R 78 :O 75 :A 61 :F 66 :W 83 :Q 77 :P 76
;;                 :D 64 :B 62 :J 70 :Z 86 :T 80 :C 63 :E 65 :G 67
;;                 :Y 85 :X 84 :H 68 :V 82 :U 81 :S 79 :N 74 :K 71}
;;       :working #{}
;;       :result []
;;       :time -1}
;;      (iterate work2)
;;      (take (+ 258 2))
;;      last)


(defn solve2 [child-parent-map]
  (->> (init-state2 child-parent-map)
       (iterate #(work2 %))
       (drop-while #(not-empty (:child-parent %)))
       first
       :time))

(defn print-part2-result [result]
  (format "일을 전부 처리하는데 %d초가 소요됩니다." result))

(->> (parse-to-pairs sample-file)
     pair-to-child-parent-map
     solve2
     print-part2-result)
