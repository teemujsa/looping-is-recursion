(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn[acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (- exp 1))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
          s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) n
      :else (recur (inc n) (rest s)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [n 0
           sum 0
           s a-seq]
      (if (empty? s)
        (/ sum n)
        (recur (inc n) (+ sum (first s)) (rest s))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [odd-seq []
         base-seq a-seq]
    (if (empty? base-seq)
      odd-seq
      (recur (toggle (set odd-seq) (first base-seq)) (rest base-seq)))))

(defn fast-fibo [n]
  (if (zero? n)
    0
    (loop [loop-n 2
           loop-fn-1 1
           loop-fn 1]
      (cond
        (<= n loop-n) loop-fn
        :else (recur (inc loop-n) loop-fn (+ loop-fn loop-fn-1))))))

(defn cut-at-repetition [a-seq]
  (loop [l-seq []
         s a-seq]
    (if (or (empty? s) (contains? (set l-seq) (first s)))
      l-seq
      (recur (conj l-seq (first s)) (rest s)))))

