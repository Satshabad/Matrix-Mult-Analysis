(ns project1.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]))

(def matrix-1 [[1, 2]
               [3, 4]])

(def matrix-2 [[7, 8]
               [9, 10]])

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn sum [l]
  (reduce + l))

(defn transpose [s]
  (apply mapv vector s))

(defn matrix-of-size [n]
  (letfn [(random-row [m]
           (vec (take m
                  (repeatedly
                    (partial rand-int 100000)))))]
    (vec (take n (repeatedly (partial random-row n))))))

(defn matrix-mult [m1 m2]
  (let [m2 (transpose m2) n (count m1)]
    (mapv
      (fn
        [ith-row]
        (mapv (fn [jth-row] (sum (mapv * ith-row jth-row)))
             m2))
      m1)))

(defn time-of-mult [n]
  (read-string ((clojure.string/split
                 (with-out-str
                   (time
                     (matrix-mult (matrix-of-size n) (matrix-of-size n))))
                  #" ")
               2)))

(defn n-powers-of-2 [n]
  (take n (for [x (range)] (exp 2N x))))

(with-open [out-file (io/writer "run-time-data/threeloops.csv")]
  (csv/write-csv out-file
   (transpose (for [x (n-powers-of-2 9)]
     [x (time-of-mult x)]))))
