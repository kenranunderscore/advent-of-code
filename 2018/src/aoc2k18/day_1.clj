(ns aoc2k18.day-1
  (:require [clojure.string :as string]))

;; part 1

(defn read-data!
  [file]
  (->> file
       slurp
       string/split-lines
       (map read-string)))

(defn frequency
  [data]
  (apply + data))

(frequency (read-data! "data/day-1"))

;; part 2

(defn first-duplicate-frequency
  [data]
  (loop [visited #{0}
         current-frequency 0
         r (cycle data)]
    (let [next-frequency (+ current-frequency
                            (first r))]
      (if (contains? visited
                     next-frequency)
        next-frequency
        (recur (conj visited next-frequency)
               next-frequency
               (rest r))))))

(first-duplicate-frequency (read-data! "data/day-1"))
