(ns aoc2018_4
  (:require [clojure.string :as str]))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 2시 5분~11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(def sample-file "resources/day4.sample.txt")

(defn read-file [path]
  (str/split-lines (slurp path)))

(defn parse-guard-id [str]
  (Integer/parseInt (subs str 1)))

(defn parse-event
  "head 토큰 내용에 따라 :type을 :shift, :falls, :wakes로 대입한다."
  [date time [head & rest]]
  (case head
    "Guard" {:date date :time time :type :shift :guard (parse-guard-id (first rest))}
    "falls" {:date date :time time :type :falls}
    "wakes" {:date date :time time :type :wakes}))

(defn parse-line
  "`[1518-11-01 23:58] Guard #99 begins shift` 와 같은 형태의 문자열을 파싱한다.
   앞에서 부터 date, time, 그리고 그 외 나머지를 묶어서 parse-event에 파라미터로 넘긴다."
  [str]
  (let [[time1 time2 & log] (str/split str #"\s")
        date (subs time1 1)
        time (subs time2 0 5)]
    (parse-event date time log)))

(defn parse-to-records [lines]
  (map parse-line lines))

(defn update-last
  "vector의 마지막 아이템 key 값에 f 함수를 적용해서 변경한 결과를 리턴한다.
   peek으로 마지막 아이템을 읽어 update하고, 이를 pop으로 마지막 아이템을 제거한 vector에 다시 추가하는 방식으로 구현"
  [vec key f]
  (if (empty? vec)
    (conj vec (update {key nil} key f))
    (conj (pop vec) (update (peek vec) key f))))

(defn log-from-event [event]
  (select-keys event [:date :time :guard]))

(defn start-sleep
  "잠자기 시작 기록을 주어진 vector에 추가하는 함수를 리턴
   이 함수는 입력으로 받은 벡터에 {:start `잠을 잔 시간`}을 추가하는 동작을 한다."
  [start]
  (fnil #(conj % {:start start}) []))

(defn end-sleep
  "잠자기 끝난 기록을 주어진 vector에 추가하는 함수를 리턴
   이 함수는 입력으로 받은 벡터 마지막 아이템에 {:end `잠을 깬 시간`} entry를 update하는 동작을 한다."
  [end]
  #(update-last % :end (fnil identity end)))

(defn make-logs
  "발생한 event들을 시간 순으로 받아 취합해서 경비원들의 기록을 데이터로 만든다.
   리턴되는 데이터 형태는 아래와 같다.
   {:date \"yyyy-MM-dd\",
    :time \"hh:mm\",
    :guard 123,
    :sleeps [
      {:start \"hh:mm\", :end \"hh:mm\"}
      {:start \"hh:mm\", :end \"hh:mm\"}
      ...
    ]}"
  [events]
  (reduce
   (fn [logs event]
     (case (:type event)
       :shift
       (conj logs (log-from-event event))

       :falls
       (update-last logs :sleeps (start-sleep (:time event)))

       :wakes
       (update-last logs :sleeps (end-sleep (:time event)))))
   [] events))

(defn reduce-by-guards
  "경비원들의 기록들을 취합해서, 경비 ID를 키로 삼아 잠을 잔 기록들을 매팡한다.
   
   # Input
   [{:guard 123, :sleeps [{:start \"hh:mm\", :end \"hh:mm\"}, ...]}, ...]

   # Output
   {123 (
     {:start \"hh:mm\", :end \"hh:mm\"}
     {:start \"hh:mm\", :end \"hh:mm\"}
     ...
   )}"
  [logs]
  (reduce
   (fn [acc log]
     (update acc (:guard log) #(concat % (:sleeps log))))
   {} logs))

;; reduce -> group-by + map

(defn reduce-by-guards
  "경비원들의 기록들을 취합해서, 경비 ID를 키로 삼아 잠을 잔 기록들을 매팡한다.
   
   # Input
   [{:guard 123, :sleeps [{:start \"hh:mm\", :end \"hh:mm\"}, ...]}, ...]

   # Output
   {123 (
     {:start \"hh:mm\", :end \"hh:mm\"}
     {:start \"hh:mm\", :end \"hh:mm\"}
     ...
   )}"
  [logs]
  (->> logs
       (group-by :guard)
       (map (fn [[guard log]]
              [guard (reduce #(concat %1 (:sleeps %2)) [] log)]) ,,,)))


;; text -> raw data (record) -> data (aggregated)
;; 알 필요 있는 것만 노출하자 (parse-to-records, make-logs, reduce-by-guards ?)

(def sleep-logs (->> sample-file
                     read-file
                     parse-to-records ; string -> record
                     (sort-by #(str/join [(:date %) (:time %)]))
                     make-logs ; record -> log
                     reduce-by-guards ; log -> data
                     ))

sleep-logs



(defn time-to-int [str]
  (Integer/parseInt (subs str 3)))

(defn sleep-time-length [{sleep-start :start sleep-end :end}]
  (- (time-to-int sleep-end) (time-to-int sleep-start)))

;; %이 2개 이상 필요하면 그냥 fn을 쓰는게 낫다.
(defn reduce-to-time-sum [logs]
  (reduce
   (fn [acc entry]
     (assoc acc (key entry) (reduce #(+ %1 (sleep-time-length %2)) 0 (val entry))))
   {}
   logs))

(defn max-time-sum-guard [logs]
  (->> logs
       reduce-to-time-sum
       (apply max-key val ,,,)
       key))



(defn sleep-time-minutes [{sleep-start :start sleep-end :end}]
  (range (time-to-int sleep-start) (time-to-int sleep-end)))

;; {123 [], 124 []}
;;
;; (reduce #(concat %1 (sleep-time-minutes %2)) [] (val entry))
;;
;; {123 [...], 124 [...] ...} => map => {123 500, 124 400 ...}
;; 
(defn reduce-to-time-minutes [logs]
  (reduce
   (fn [acc [k v]]
     (assoc acc k (reduce #(concat %1 (sleep-time-minutes %2)) [] v)))
   {}
   logs))

;; 가설1. map으로 풀 수 있는 것을 reduce로 풀었기 때문입니다. -> acc라는 상태가 생기기 때문
;; 가설2. 1개에 대해 동작하는 함수를 n개에 대해 동작하는 함수로 만들었기 때문입니다.
;; 가설3. group-by?
;;
;; loop/recur -> reduce -> map / filter

(defn max-sleep-minutes [logs guard-id]
  (key (apply max-key val (frequencies ((reduce-to-time-minutes logs) guard-id)))))

;; 중간에도 스레딩 메크로를 활용해보자


(let [guard-id (max-time-sum-guard sleep-logs)
      sleep-minutes (max-sleep-minutes sleep-logs guard-id)]
  (format
   "가장 오랜시간 잠들어있었던 가드의 ID는 %d이고, 그 가드가 가장 잠들어 있었던 분은 %d분 입니다. 정답은 %d 입니다."
   guard-id sleep-minutes (* guard-id sleep-minutes)))


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(defn max-frequency-minute [minutes]
  (if (empty? minutes)
    [0 0]
    (apply max-key val (frequencies minutes))))

(defn get-max-frequency-minute-and-guard [logs]
  (let [guard-sleep-minutes (reduce-to-time-minutes logs)]
    (apply max-key #(second (second %))
           (map (fn [[guard minutes]]
                  [guard (max-frequency-minute minutes)]) guard-sleep-minutes))))

(let [[guard-id [minute _]] (get-max-frequency-minute-and-guard sleep-logs)]
  (format
   "동 시간에 가장 비번히 잠에 든 가드의 ID는 %d이고, 그 분(minute)은 %d분 입니다. 정답은 %d 입니다."
   guard-id minute (* guard-id minute)))
