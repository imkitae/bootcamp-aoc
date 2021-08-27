(ns aoc2018_7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn available-ordered-works
  "작업 의존 맵을 보고 지금 수행 가능한 작업의 리스트를 리턴
   지금 수행 가능한 작업 = parent가 empty인 작업"
  [child-parent-map working]
  (->> (empty-value-keys child-parent-map)
       (filter #((complement working) %))
       sort))

(def part1-worker-num 1)

(defn part1-work-remains []
  (->> (range (int \A) (inc (int \Z)))
       (map #(vector (keyword (str (char %))) 1))
       (into {})))

(defn init-state
  "초기 state를 생성
   - remains: 키=일, 값=완료되기까지 남은 시간 으로 만들어진 맵
   - child-parent 키=일, 값=먼저 완료해야 할 다른 작업들 로 만들어진 맵"
  [remains child-parent]
  {:time -1
   :child-parent child-parent
   :remains remains
   :working #{}
   :result []})

(defn next-child-parent
  "일의 의존 관계 맵인 child-parent에서 의존 정보를 제거한 결과 리턴
   1) to-removes에 들어있는 일 중 child-parent에 동일한 키가 있다면 해당 엔트리 제거
   2) child-parent 값 콜렉션들을 탐색해서 동일한 아이템이 있다면 콜렉션에서 제거"
  [child-parent to-removes]
  (->> child-parent
       (filter (fn [[child _]]
                 ((complement to-removes) child)))
       (map (fn [[child parents]]
              [child (set/difference (set parents) to-removes)]))
       (into {})))

(defn next-remains
  "일이 남은 상황인 remains와 현재 작업 중인 일 목록을 바탕으로 다음 시간 대의 remains 정보 리턴
   1) remains에서 키가 working에 들어있는 일과 동일한 엔트리의 값을 1 감소
   2) 엔트리 콜렉션을 into를 통해 맵으로 리턴"
  [remains working]
  (->> remains
       (map (fn [[work remain]]
              [work (if (working work) (dec remain) remain)]))
       (into {})))

(defn filter-working-remains
  "전체 일 남은 상황(=remains)과 작업 중인 일 목록(=working)으로 주어진 조건(=pred)에 맞는 일 목록을 구한다.
   1) 우선 remains에서 working에 존재하는 엔트리를 필터링하고
   2) 해당 엔트리들에 pred로 다시 필터링한 결과를 리턴"
  [working pred remains]
  (->> remains
       (filter #(working (first %)))
       (filter pred)))

(defn work
  "주어진 state에서 일을 진행한 1분 뒤 state를 반환
   1) 전체 일이 남은 상황(remains)과 현재 진행 중인 일(working)로 1분 뒤 일이 남은 상황을 구한다. (remains')
   2) 1분 뒤 일이 남은 상황을 읽고 1분 뒤 완료될 일들과 남은 일들을 계산한다. (finished, not-finished)
   3) 1분 뒤 남은 일의 수와 최대 작업자 수의 차이로 1분 뒤 추가로 진행 가능한 일의 수를 구한다. (waiting-num)
   4) 1분 뒤 완료될 일들을 의존 관계 맵(child-parent)에서 제거해서 1분 뒤의 의존 관계를 계산한다. (child-parent')
   5) 1분 뒤 의존 관계 맵에서 의존이 없어 1분 뒤 시작 가능한 일들을 구하고 (현재 작업 중인 일도 의존이 없는 상태라 포함되어 있으므로 제외 처리),
      우선 순위로 정렬한 뒤 추가 진행 가능한 일만큼을 가져온다. (works-to-add)
   6) 1분 뒤 작업 중인 일은 `끝나지 않은 일` + `추가될 일`
   7) 1분 뒤 완료될 일들을 전체 완료된 일(result) 목록에 추가한다."
  [worker-num
   {:keys [time
           child-parent
           remains
           working
           result]}]
  (let [remains' (next-remains remains working)
        finished (->> remains'
                      (filter-working-remains working #(= 0 (second %)))
                      (map first)
                      set)
        not-finished (->> remains'
                          (filter-working-remains working #(< 0 (second %)))
                          (map first)
                          set)
        waiting-num (- worker-num (count not-finished))
        child-parent' (next-child-parent child-parent finished)
        works-to-add (->> working
                          (available-ordered-works child-parent')
                          (take waiting-num))]
    {:time (inc time)
     :child-parent child-parent'
     :remains remains'
     :working (set/union not-finished (set works-to-add))
     :result (concat result finished)}))

(defn solve [worker-num remains child-parent-map]
  (->> (init-state remains child-parent-map)
       (iterate #(work worker-num %))
       (drop-while #(not-empty (:child-parent %)))
       first))

(defn print-part1-result [{result :result}]
  (format "일을 처리하는 순서는 %s 입니다." (str/join (map name result))))

(->> (parse-to-pairs sample-file)
     pair-to-child-parent-map
     (solve part1-worker-num (part1-work-remains))
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

(def part2-worker-num 5)

(defn part2-work-remains
  "A ~ Z를 61부터 시작하는 순차적으로 증가하는 수로 치환한 리스트"
  []
  (->> (range (int \A) (inc (int \Z)))
       (map #(vector
              (keyword (str (char %)))
              (+ 61 (- % (int \A)))))
       (into {})))

(defn print-part2-result [{time :time}]
  (format "일을 전부 처리하는데 %d초가 소요됩니다." time))

(->> (parse-to-pairs sample-file)
     pair-to-child-parent-map
     (solve part2-worker-num (part2-work-remains))
     print-part2-result)
