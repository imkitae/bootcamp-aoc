(ns aoc2018_4_2
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

(def sample-file-path "resources/day4.sample.txt")

(defn read-file [path]
  (str/split-lines (slurp path)))

(defn str-to-record
  "# Input
   `[1518-11-01 23:58] Guard #99 begins shift` 와 같은 형태의 문자열

   # Output
   {:date 'yyyy-MM-dd', :time 'hh:mm', :type :shift/:falls/:wakes, :guard 123}"
  [str]
  (let [[time1 time2 type guard-id] (str/split str #"\s")
        date (subs time1 1)
        time (subs time2 0 5)]
    (case type
      "Guard" {:date date, :time time, :type :shift, :guard (Integer/parseInt (subs guard-id 1))}
      "falls" {:date date, :time time, :type :falls}
      "wakes" {:date date, :time time, :type :wakes})))

(defn map-records-to-guard-sleeps
  "# Input - (shift record 1개 + falls/wakes record 쌍 n개)의 반복
   [{:date 'yyyy-MM-dd', :time 'hh:mm', :type :shift, :guard 123}
    {:date 'yyyy-MM-dd', :time 'hh:mm', :type :falls}
    {:date 'yyyy-MM-dd', :time 'hh:mm', :type :wakes}
    ...]
   
   # Output - 경비원 아이디와 잠잔 분(minutes) list
   ({:guard 1, :sleeps (5 6 7 ... 24 30 31 32 ... 54)}
    {:guard 2, :sleeps (...))}
    ...)"
  [records]
  (->> records
       (partition-by :guard)
       (partition 2)
       (map (fn [[[{guard-id :guard}] fall-wakes]]
              {:guard guard-id
               :sleeps (->> fall-wakes
                            (map :time) ;; ('00:12' '00:15' '00:20' '00:25')
                            (map #(Integer/parseInt (subs % 3))) ;; (12 15 20 25)
                            (partition 2) ;; ((12 15), (20 25))
                            (mapcat #(apply range %)))})) ;; (12 13 14 15 20 21 22 23 24 25)
       (group-by :guard)
       (map (fn [[guard-id logs]]
              {:guard guard-id, :sleeps (mapcat :sleeps logs)}))))

(defn parse-to-guard-sleeps-list
  "파일의 각 라인에 해당하는 문자열 리스트를 guard-sleeps-list 데이터로 변경
   
   # Input - 파일에서 읽은 문자열 리스트
   [\"[1518-11-01 23:58] Guard #99 begins shift\"
    \"[1518-11-01 00:05] falls asleep\"
    \"[1518-11-01 00:25] wakes up\"]
   
   # Output - 경비원 아이디와 잠잔 분(minutes)들의 맵 리스트
   ({:guard 1, :sleeps (5 6 7 ... 24 30 31 32 ... 54)}
    {:guard 2, :sleeps (...))}
    ...)"
  [lines]
  (->> lines
       (map str-to-record)
       (sort-by #(str/join [(:date %) (:time %)]))
       map-records-to-guard-sleeps))

(defn max-sleep-guard
  "guard-sleeps 리스트 아이템 중 :sleeps에 들어있는 분의 개수가 가장 많은 아이템을 리턴
   # Input
   ({:guard 10, :sleeps (2 3 4 5)}
    {:guard 11, :sleeps (3 4 5)}
    {:guard 12, :sleeps (1 2)})
   # Output
   {:guard 10, :sleeps (2 3 4 5)}"
  [guard-sleeps]
  (->> guard-sleeps
       (apply max-key #(count (:sleeps %)))))

(defn most-frequency-int
  "정수 리스트에서 가장 많이 등장하는 정수와 횟수 리턴 [정수, 횟수]"
  [ints]
  (if (empty? ints)
    [nil 0]
    (->> ints
         frequencies
         (apply max-key second))))

(defn print-part1-result [max-sleep-guard]
  (let [guard-id (:guard max-sleep-guard)
        most-minute (->> (:sleeps max-sleep-guard)
                         most-frequency-int
                         first)]
    (format
     "가장 오랜시간 잠들어있었던 가드의 ID는 %d이고, 그 가드가 가장 잠들어 있었던 분은 %d분 입니다. 정답은 %d 입니다."
     guard-id most-minute (* guard-id most-minute))))

(->> sample-file-path
     read-file
     parse-to-guard-sleeps-list
     max-sleep-guard
     print-part1-result)


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(defn max-frequency-minute-guard
  "guard-sleeps 리스트 아이템 중 :sleeps에 가장 많이 포함된 분을 계산하고, 가장 빈도가 높은 것을 리턴
   # Input
   ({:guard 10, :sleeps (22 23 24 25 10 21 22)}
    {:guard 11, :sleeps (10 11 12 20 21 22)}
    {:guard 12, :sleeps (45 46 47 42 43 44 45 46 44 45)})
   # Output
   {:guard 12, :minutes 45 :frequency 3}"
  [guard-sleeps]
  (->> guard-sleeps
       (map (fn [{guard-id :guard
                  sleeps :sleeps}]
              (let [[minute frequency] (most-frequency-int sleeps)]
                {:guard guard-id
                 :minute minute
                 :frequency frequency})))
       (apply max-key :frequency)))

(defn print-part2-result [max-frequency]
  (let [{guard-id :guard,
         minute :minute} max-frequency]
    (format
     "동 시간에 가장 비번히 잠에 든 가드의 ID는 %d이고, 그 분(minute)은 %d분 입니다. 정답은 %d 입니다."
     guard-id minute (* guard-id minute))))

(->> sample-file-path
     read-file
     parse-to-guard-sleeps-list
     max-frequency-minute-guard
     print-part2-result)
