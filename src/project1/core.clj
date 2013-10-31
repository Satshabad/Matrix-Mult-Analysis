(ns project1.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io])
  (use [criterium.core :only [bench quick-bench benchmark quick-benchmark with-progress-reporting]] ))

;--------------------  Classic -------------------

(defn sum [l]
  (reduce + l))

(defn transpose [s]
  (apply mapv vector s))

(defn mult-classic [m1 m2]
  (let [m2 (transpose m2) n (count m1)]
    (mapv
      (fn
        [ith-row]
        (mapv (fn [jth-row] (sum (mapv * ith-row jth-row)))
             m2))
      m1)))

;--------------------  Divide and Conquer  -------------------

(defn subsection [matrix row-start row-stop col-start col-stop]
  (subvec (mapv #(subvec % col-start col-stop) matrix) row-start row-stop))

(defn quarters [matrix]
  (let [n (count matrix)
        half-n (/ n 2)]
    [(subsection matrix 0      half-n 0      half-n)
     (subsection matrix 0      half-n half-n n)
     (subsection matrix half-n n      0      half-n)
     (subsection matrix half-n n half-n      n)
     ]))

(defn add [m1 m2]
  (mapv #(mapv + %1 %2) m1 m2))

(def concatv
  (comp vec concat))

(defn combine [M11 M12 M21 M22]
  (let [top (mapv concatv M11 M12)
        bottom (mapv concatv M21 M22)]
    (concatv top bottom)))

(defn mult-divide-and-conquer [A B]
  (if (= 2 (count A))
    (mult-classic A B)
    (let [quartersA (quarters A)
          quartersB (quarters B)
          A11 (quartersA 0)
          A12 (quartersA 1)
          A21 (quartersA 2)
          A22 (quartersA 3)
          B11 (quartersB 0)
          B12 (quartersB 1)
          B21 (quartersB 2)
          B22 (quartersB 3)

          C11 (add (mult-divide-and-conquer A11 B11) (mult-divide-and-conquer A12 B21))
          C12 (add (mult-divide-and-conquer A11 B12) (mult-divide-and-conquer A12 B22))
          C21 (add (mult-divide-and-conquer A21 B11) (mult-divide-and-conquer A22 B21))
          C22 (add (mult-divide-and-conquer A21 B12) (mult-divide-and-conquer A22 B22))]
      (combine C11 C12 C21 C22))))

;--------------------  Strassen -------------------

(defn sub [m1 m2]
  (mapv #(mapv - %1 %2) m1 m2))

(defn neg [m]
  (mapv #(mapv (partial * -1) %) m))

(defn mult-strassen [A B]
  (if (= (count A) 2)
    (mult-classic A B)
    (let [quartersA (quarters A)
          quartersB (quarters B)
          A11 (quartersA 0)
          A12 (quartersA 1)
          A21 (quartersA 2)
          A22 (quartersA 3)
          B11 (quartersB 0)
          B12 (quartersB 1)
          B21 (quartersB 2)
          B22 (quartersB 3)

          P (mult-divide-and-conquer (add A11 A22) (add B11 B22))
          Q (mult-divide-and-conquer (add A21 A22) B11)
          R (mult-divide-and-conquer A11 (sub B12 B22))
          S (mult-divide-and-conquer A22 (sub B21 B11))
          T (mult-divide-and-conquer (add A11 A12) B22)
          U (mult-divide-and-conquer (sub A21 A11) (add B11 B12))
          V (mult-divide-and-conquer (sub A12 A22) (add B21 B22))

          C11 (add P (add S (add (neg T) V)))
          C12 (add R T)
          C21 (add Q S)
          C22 (add P (add R (add (neg Q) U)))]
      (combine C11 C12 C21 C22))))

;--------------------  Analysis  -------------------

(defn matrix-of-size [n]
  (letfn [(random-row [m]
           (vec (take m
                  (repeatedly
                    (partial rand-int 100000)))))]
    (vec (take n (repeatedly (partial random-row n))))))

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn n-powers-of-2 [n]
  (map exp (repeat n 2) (range 1 (inc n))))

; taken from http://clojuredocs.org/clojure_core/clojure.core/time and modified
(defmacro time-of
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn avg-time-of-mult [mult-fn size num-of-runs]
  ; do 3 warm up runs to get JIT benifts
  (let [total-time (do (mult-fn (matrix-of-size size) (matrix-of-size size))
                       (mult-fn (matrix-of-size size) (matrix-of-size size))
                       (mult-fn (matrix-of-size size) (matrix-of-size size))
                       (time-of
                         (dotimes [n num-of-runs]
                                  (do
                                    (println (str "Running " mult-fn " of size " size " run: " n))
                                    (mult-fn (matrix-of-size size) (matrix-of-size size))))))]
    (/ total-time num-of-runs)))


(defn run-and-record [up-to-size num-of-runs out-file-name f]
  (with-open [out-file (io/writer out-file-name)]
    (csv/write-csv out-file
     (transpose
       (for [x (n-powers-of-2 up-to-size)]
         [x (avg-time-of-mult f x num-of-runs)])))))

(defn -main []
  (do
    (run-and-record 9 30 "run-time-data/classic.csv" mult-classic)
    (run-and-record 9 30 "run-time-data/divide.csv" mult-divide-and-conquer)
    (run-and-record 9 30 "run-time-data/strassen.csv" mult-strassen)))

