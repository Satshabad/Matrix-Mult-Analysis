(ns project1.core
  (require [clojure.data.csv :as csv]
           [clojure.java.io :as io]))

(defn sum [l]
  (reduce + l))

(defn transpose [s]
  (apply mapv vector s))

(defn mult-iter [m1 m2]
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
        bottom (map concatv M21 M22)]
    (concatv top bottom)))

(defn mult-divide-and-conquer [A B]
  (if (= 2 (count A))
    (mult-iter A B)
    (let [[A11 A12 A21 A22] (quarters A)
          [B11 B12 B21 B22] (quarters B)

          C11 (add (mult-divide-and-conquer A11 B11) (mult-divide-and-conquer A12 B21))
          C12 (add (mult-divide-and-conquer A11 B12) (mult-divide-and-conquer A12 B22))
          C21 (add (mult-divide-and-conquer A21 B11) (mult-divide-and-conquer A22 B21))
          C22 (add (mult-divide-and-conquer A21 B12) (mult-divide-and-conquer A22 B22)) ]
      (combine C11 C12 C21 C22))))


;--------------------  Strassen -------------------
;
;
(defn sub [m1 m2]
  (mapv #(mapv - %1 %2) m1 m2))

(defn neg [m]
  (mapv #(mapv (partial * -1) %) m))

(defn mult-strassen [A B]
  (if (= (count A) 2)
    (mult-iter A B)
    (let [[A11 A12 A21 A22] (quarters A)
          [B11 B12 B21 B22] (quarters B)

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

(defn time-of-mult [n]
  (read-string ((clojure.string/split
                 (with-out-str
                   (time
                     (mult-iter (matrix-of-size n) (matrix-of-size n))))
                  #" ")
               2)))

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

(defn n-powers-of-2 [n]
  (take n (for [x (range)] (exp 2N x))))

; (with-open [out-file (io/writer "run-time-data/threeloops.csv")]
;   (csv/write-csv out-file
;    (transpose (for [x (n-powers-of-2 9)]
;      [x (time-of-mult x)]))))
;
;
