(ns four-clojure.core)

;; Question 1 - Nothing but the Truth
;; (= __ true)
(= true true)

;; Question 2 - Simple Math
;; (= (- 10 (* 2 3)) __)
(= (- 10 (* 2 3)) 4)

;; Question 3 - Intro to Strings
;; (= __ (.toUpperCase "hello world"))
(= "HELLO WORLD" (.toUpperCase "hello world"))

;; Question 4 - Intro to Lists
;; (= (list __) '(:a :b :c))
(= (list :a :b :c) '(:a :b :c))

;; Question 5 - Lists: conj
;; (= __ (conj '(2 3 4) 1))
;; (= __ (conj '(3 4) 2 1))
(= '(1 2 3 4) (conj '(3 4) 2 1))

;; Question 6 - Intro to Vectors
;; (= [__] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
(= [:a :b :c] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))

;; Question 7 - Vectors: conj
;; (= __ (conj [1 2 3] 4))
;; (= __ (conj [1 2] 3 4))
(= [1 2 3 4] (conj [1 2] 3 4))

;; Question 8 - Intro to Sets
;; (= __ (set '(:a :a :b :c :c :c :c :d :d)))
;; (= __ (clojure.set/union #{:a :b :c} #{:b :c :d}))
(= #{:a :b :c :d} (clojure.set/union #{:a :b :c} #{:b :c :d}))

;; Question 9 - Sets: conj
;; (= #{1 2 3 4} (conj #{1 4 3} __))
(= #{1 2 3 4} (conj #{1 4 3} 2))

;; Question 10 - Intro to Maps
;; (= __ ((hash-map :a 10, :b 20, :c 30) :b))
;; (= __ (:b {:a 10, :b 20, :c 30}))
(= 20 (:b {:a 10, :b 20, :c 30}))

;; Question 11 - Maps: conj
;; (= {:a 1, :b 2, :c 3} (conj {:a 1} __ [:c 3]))
(= {:a 1, :b 2, :c 3} (conj {:a 1} {:b 2} [:c 3]))

;; Question 12 - Intro to Sequences
;; (= __ (first '(3 2 1)))
;; (= __ (second [2 3 4]))
;; (= __ (last (list 1 2 3)))
(= 3 (last (list 1 2 3)))

;; Question 13 - Sequences: rest
;; (= __ (rest [10 20 30 40]))
(= [20 30 40] (rest [10 20 30 40]))

;; Question 14 - Intro to Functions
;; (= __ ((fn add-five [x] (+ x 5)) 3))
;; (= __ ((fn [x] (+ x 5)) 3))
;; (= __ (#(+ % 5) 3))
;; (= __ ((partial + 5) 3))
(= 8 ((partial + 5) 3))

;; Question 15 - Double Down
;; (= (__ 2) 4)
;; (= (__ 3) 6)
;; (= (__ 11) 22)
;; (= (__ 7) 14)
(= (* 2 7) 14)

;; Question 16 - Hello World
;; (= (__ "Dave") "Hello, Dave!")
;; (= (__ "Jenn") "Hello, Jenn!")
;; (= (__ "Rhea") "Hello, Rhea!")
(= (#(str "Hello, " % "!") "Rhea") "Hello, Rhea!")

;; Question 17 - Sequences: map
;; (= __ (map #(+ % 5) '(1 2 3)))
(= '(6 7 8) (map #(+ % 5) '(1 2 3)))

;; Question 18 - Sequences: filter
;; (= __ (filter #(> % 5) '(3 4 5 6 7)))
(= '(6 7) (filter #(> % 5) '(3 4 5 6 7)))

; Question 19 - Last Element
(defn last-el
  [list]
  (if (empty? (rest list))
    (first list)
    (recur (rest list))))

; Question 20 - Penultimate Element
(defn second-to-last
  [coll]
  (second (reverse coll)))

; Question 21 - Nth Element
(defn nth'
  [coll n]
  (last (take (+ n 1) coll)))

; Question 22 - Count a Sequence
(defn count-seq
  [seq]
  (if (empty? seq)
    0
    (inc (count-seq (rest seq)))))

; Question 23 - Reverse a Sequence
(defn reverse'
  [coll]
  (reduce conj '() coll))

; Question 24 -  Sum It
(defn sum-all
  [coll]
  (reduce + coll))

; Question 25 - Find thee odd numbers
(defn odd?'
  [list]
  (filter odd? list))

; Question 26 - Fibonacci -> tail recursion
(defn fibonacci
  ([number]
   (fibonacci [1 1] number))
  ([list number]
   (if (= number 2)
     list
     (recur (conj list (+ (peek list) (last (butlast list)))) (dec number)))))

; Question 26 alternative - Fibonacci -> direct recursion
(defn fibonacci'
  ([num]
   (fibonacci' num 0 1))
  ([num x y]
   (lazy-seq (cons y (when (> num 1)
                       (fibonacci' (dec num) y (+ x y)))))))

; Question 27 - Palindrome Detector
(defn palindrome?'
  [x]
  (= (reverse x) (seq x)))

; Question 28 - Flatten a Sequence
(defn my-flatten
  [[head & tail :as coll]]
  (cond
    (coll? head) (concat (my-flatten head) (my-flatten tail))
    (empty? coll) nil
    :else (cons head (my-flatten tail))))

; Question 28 - Alternative
(defn my-flatten'
  [coll]
  (mapcat (fn [x]
            (if (coll? x) (my-flatten' x) [x])) coll))

; Question 29 - Get the Caps
(defn get-caps
  [x]
  (apply str (filter #(Character/isUpperCase %) x)))

; Question 29 - Alternative
(defn get-caps'
  [s]
  (clojure.string/replace s #"[^A-Z]" ""))

; Question 30 - Compress a Sequence
(defn compress-seqs
  [seq]
  (reduce (fn
            [list item]
            (if (= (last list) item)
              list
              (conj list item))) [] seq))

; Question 31 - Pack a Sequence
(defn pack-a-seq
  [seq]
  (reverse
    (reduce (fn
              [list item]
              (if (= (first (first list)) item)
                (conj (rest list) (conj (first list) item))
                (conj list [item]))) '() seq)))

; Question 32 - Duplicate a Sequence
(defn duplicate-a-seq
  [seq]
  (sort (flatten (repeat 2 [1 2 3]))))

; Question 32 - Alternative
(defn duplicate-a-seq'
  [list]
  (sort (reduce conj (into '() list) list)))


; Question 33 - Replicate a Sequence
(defn replicate-a-seq'
  [coll x]
  (reduce (fn [list item]
            (concat list (repeat x item))) '() coll))

; Question 33 - Alternative
(defn replicate-a-seq
  [seq x]
  (sort (flatten (repeat x [1 2 3]))))


; Question 34
(defn range'
  ([start end]
   (range' [] start end))
  ([vector start end]
   (if (< start end)
     (recur (conj vector start) (inc start) end)
     vector)))

; Question 34 alternative
(defn range'' [start end]
  (if (< start end)
    (cons start (range'' (inc start) end))
    '()))

;; Question 34 alternative 2
(defn lazy-range' [start end]
  (lazy-seq
    ;(println "lazy range" start end)
    (if (< start end)
      (cons start (lazy-range' (inc start) end))
      '())))

; Question 35 - Local bindings
(= 7 (let [x 5] (+ 2 x)))

; Question 36 - Let it be
(= 1 (let [z 1, y 3, x 7] z))

; Question 37 - Regular Expressions
(= "ABC" (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))

; Question 38 - Maximum value
(defn max-value
  [& nums]
  (reduce #(if (> %1 %2) %1 %2) nums))

; Question 38 - Alternative
(defn max-value'
  [& %]
  (into [] %)
  (last (sort %)))

; Question 39 - Interleave Two Seqs
(defn interleave-seqs
  [seq1 seq2]
  (mapcat vector seq1 seq2))

; Question 39 - Alternative
(defn two-seqs
  [%1 %2]
  (if (and (> (count %1) 0) (> (count %2) 0))
    (concat (list (first %1) (first %2)) (two-seqs (rest %1) (rest %2)))
    ()))

; Question 40 - Interpose a Seq
(defn interpose-a-seq
  [x seq]
  (butlast (reduce #(conj %1 %2 x) [] seq)))                ;%1 is list %2 item


; Question 40 - Alternative
(fn
  [x collection]
  (into [] (butlast (reduce
                      (fn
                        [coll item]
                        (conj (conj coll item) x)) [] collection))))

; Question 41 - Drop Every Nth Item
(defn drop-nth
  [coll n]
  (mapcat #(if (= (count %) n)
             (drop-last %)
             %)
          (partition-all n coll)))

; Question 42 - Factorial Fun
(defn factorial-fun
  [x]
  (reduce #(* %1 %2) 1 (range 1 (+ x 1))))

; Question 43 - Reverse Interleave
(defn reverse-interleave'
  [coll x]
  (apply (partial map list) (partition x coll)))

; Question 44 - Rotate Sequence
(defn rotate-seq
  [x coll]
  (if (< (Math/abs x) (count coll))
    (if (< x 0)
      (concat (drop (+ x (count coll)) coll) (take (+ x (count coll)) coll))
      (concat (drop x coll) (take x coll)))
    (if (< x 0)
      (rotate-seq (+ x (count coll)) coll)
      (rotate-seq (- x (count coll)) coll))))
; split?

; Question 45 - Intro to Iterate
(= '(1 4 7 10 13) (take 5 (iterate #(+ 3 %) 1)))

; Question 46 - Flipping out
(defn flipping-out
  [f]
  #(f %2 %1))

; Question 47 - Contain Yourself
; with maps, it returns true if the map contains the value
; with vectors it checks if it contains the index
(contains? #{4 5 6} 4)
(contains? [1 1 1 1 1] 4)

; Question 48 - Intro to Some
; (= __ (some #{2 7 6} [5 6 7 8])) - returns first true
; where item is in the coll
; (= __ (some #(when (even? %) %) [5 6 7 8]))
(= 6 (some #(when (even? %) %) [5 6 7 8]))

; Question 49 - Split a sequence
(defn split-a-seq
  [x coll]
  [(take x coll) (drop x coll)])

; Question 50 - Split by Type
(defn split-by-type
  [coll]
  (vals (group-by type coll)))

; Question 51 - Advanced Destructuring
(= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] [1 2 3 4 5]] [a b c d]))

;; Question 52 Intro to Destructuring
;; (= [2 4] (let [[a b c d e] [0 1 2 3 4]] __))
(= [2 4] (let [[a b c d e] [0 1 2 3 4]] [c e]))

; Question 54 - Partition a Sequence
(defn partition-a-seq
  ([x coll]
   (partition-a-seq x (drop x coll) [(take x coll)]))
  ([x coll new-coll]
   (if (< (count coll) x)
     new-coll
     (recur x (drop x coll) (conj new-coll (take x coll))))))



; Question 55 - Count Occurrences
(defn count-occurrences
  [coll]
  (reduce-kv (fn [m k v]
               (assoc m k (count v)))
             {} (group-by identity coll)))

(defn count-occurrences'
  [coll]
  (reduce-kv (fn [m k v]
               (assoc m k v))
             {} coll))

(defn count-occurrences''
  [coll]
  (reduce (fn [m n]
            (println m n)
            (update m n (fnil inc 0)))
          {}
          coll))

(let [{a 2} [1 2 3 4 5]]
  a)

; Question 56 - Find Distinct Items
(defn find-distinct-items                                   ; can use distinct here
  [coll]
  (reduce (fn [my-list item]
            (if (nil? (some #{item} my-list))
              (conj my-list item)
              my-list)) [] coll))

;; Question 57
;; (= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
(= '(5 4 3 2 1) ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))

; Question 58 - Function Composition
(defn function-comp                                         ;comp
  [& fs]
  (fn [& coll]
    (first (reduce (fn [my-list f]
                     [(apply f my-list)])
                   coll
                   (reverse fs)))))

; Question 59 - Juxtaposition
(defn juxtaposition                                         ; juxt
  [& fs]
  (fn [& coll]
    (map #(apply % coll) fs)))

; Question 60 - Sequence Reductions
(defn my-reductions'
  ([f [head & tail :as coll]]
   (if (empty? coll)
     (lazy-seq [(f)])
     (my-reductions' f head tail)))
  ([f start [head & tail :as coll]]
   (lazy-seq
     (cons start (when (not-empty coll)
                   (my-reductions' f
                                   (f start head)
                                   tail))))))

; Question 61 - Map Construction
(defn map-const
  [coll1 coll2]
  (reduce conj (map hash-map coll1 coll2)))

; Question 62 - Re-implement Iterate
(defn re-implement-iterate
  [f x]
  (reductions (fn [last-result item]
                (f last-result)) x (range)))

; Question 62 - Alternative
(defn iterate'''
  [f num]
  (lazy-seq (cons num (iterate''' f (f num)))))

; Question 63
(defn group-a-seq
  [f seq]
  (reduce (fn [m item]
            (assoc m (f item)
                     (into [] (conj (get m (f item)) item))))
          {} seq))

; Question 64
(= 6 (reduce + 1 [2 3]))

; Question 65 - Black Box Testing
(defn black-box-testing
  [coll]
  (cond
    (= (first (str coll)) \{) :map
    (= (first (str coll)) \#) :set
    (= (first (str coll)) \[) :vector
    :else :list))

; map, set, list, vector

; map count changes

; Question 66 - Greatest Common Divisor
(defn greatest-common-d
  [x y]
  (last
    (remove
      #(not (and (= (rem x %) 0) (= (rem y %) 0)))
      (range 1 (+ x 1)))))

; Question 67 - Prime Numbers
(defn prime-number'''
  [x]
  (loop [prime-list []
         index 2]
    (cond
      (= (count prime-list) x) prime-list
      (every?
        #(not (= (mod index %) 0))
        (take-while (constantly true) #_(>= (Math/sqrt index) %) prime-list))
      (recur (conj prime-list index) (inc index))
      :else (recur prime-list (inc index)))))

(defn is-prime?
  [x my-list]
  (reduce
    #(or
       (and %1
            (if (< %2 (Math/sqrt x))
              (reduced %1)
              (not (= (mod x %2) 0))))
       (reduced false))
    true
    my-list))

(defn is-prime?'
  [x my-list]
  (every?
    #(not (= (mod x %) 0))
    (take-while #(>= (Math/sqrt x) %) my-list)))

(defn prime-number'
  [x]
  (take x (filter #(is-prime? % (drop 2 (take % (range)))) (drop 2 (range)))))

(defn prime-number
  [x]
  (reduce (fn [my-list item]
            (cond
              (>= (count my-list) x) (reduced my-list)
              (reduce
                #(or (and %1 (not (= (mod item %2) 0))) (reduced false))
                true my-list)
              (conj my-list item)
              :else my-list)) [] (drop 2 (range))))

(defn prime-number''
  [x]
  (loop [prime-list []
         index 2]
    (cond
      (= (count prime-list) x) prime-list
      (is-prime?' index prime-list) (recur (conj prime-list index) (inc index))
      :else (recur prime-list (inc index)))))

(defn is-prime-alternative?
  [x]
  (reduce (fn [b num]
            (and b (if (zero? (rem x num))
                     false
                     true)))
          true (range 2 x)))

; Question 68 - Recurring Theme
(= '(7 6 5 4 3)
   (loop [x 5
          result []]
     (if (> x 0)
       (recur (dec x) (conj result (+ 2 x)))
       result)))

; Question 69 - Merge with a Function
(defn my-merge-with
  [f & maps]
  (into {} (reverse (reduce (fn [my-map item]
                              (if (contains? my-map (first item))
                                (assoc my-map (first item) (f (second item) (get my-map (first item))))
                                (assoc my-map (first item) (second item))))
                            {} (reverse (mapcat #(keys (group-by identity %)) maps))))))

; Question 70 - Word Sorting
(defn word-sorting
  [s]
  (sort-by #(.toLowerCase %) (clojure.string/split (clojure.string/replace s #"[.!?/,]" "") #" ")))

; Question 71 - Rearranging Code: -> ;takes the coll as the first arg
(= (last (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (last))
   5)

; Question 72 - Rearranging Code: ->>
(= (reduce + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (reduce +))
   11)

; Question 74 - Filter Perfect Squares
(defn filter-perfect-squares
  [s]
  (remove #(not (= \0 (last (str (Math/sqrt (- (int %) 48)))))) s))

(defn filter-perfect-squares'
  [s]
  (clojure.string/join (reduce (fn [my-list item]
                                 (cond
                                   (and (= (last (str (Math/sqrt (Integer/parseInt item)))) \0) (empty? my-list)) (conj my-list item)
                                   (and (= (last (str (Math/sqrt (Integer/parseInt item)))) \0) (not (empty? my-list))) (conj my-list \, item)
                                   :else my-list))
                               []
                               (clojure.string/split s #","))))

;  Question 73 - 81 - MEDIUM AND HARD
;Question 75 - Euler's Totient Function
(defn euler's-function
  [x]
  (if (= 1 x)
    1
    (let [greatest-divisor (fn [x y]
                             (last
                               (remove
                                 #(not (and (= (rem x %) 0) (= (rem y %) 0)))
                                 (range 1 (+ x 1)))))]
      (reduce (fn [num item]
                (if (= 1 (greatest-divisor x item))
                  (inc num)
                  num)) 0 (range (inc x))))))

; Question 76 - trampoline
(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))

; Question 77 - Anagram Finder
(defn anagram-finder
  [coll]
  (into #{}
        (remove #(= (count %) 1)
                (map #(into #{} %) (vals (group-by sort coll))))))

; Question 78 - Reimplement Trampoline
(defn my-trampoline
  ([f]
   (if (fn? (f))
     (recur (f))
     (f)))
  ([f & args]
   (my-trampoline #(apply f args))))

; Question 80 - Perfect Numbers
(defn perfect-numbers
  [num]
  (= num (apply + (reduce (fn [my-list item]
                            (if (= (mod num item) 0)
                              (cons item my-list)
                              my-list)) [] (range 1 num)))))
; Question 81 - Set Intersection
(defn set-intersection
  [coll1 coll2]
  (reduce #(if (contains? coll2 %2) (conj %1 %2) %1) #{} coll1))

; Question 81 - Alternative
(defn set-intersection'
  [first-coll second-coll]
  (into #{}
        (remove
          #(not (contains? first-coll %)) second-coll)))

; Question 81 - 83 - MEDIUM

; Question 83 - A Half-Truth
(defn a-half-truth
  [& args]
  (not (or (every? true? args) (every? false? args))))

; Question 85 - Power Set
(defn power-set
  [my-set]
  (into #{} (conj (reduce (fn [set item]
                            (println set)
                            (if (empty? set)
                              (conj set #{item})
                              (concat set #{#{item}} (map #(conj % item) set)))) #{} my-set) #{})))

; Question 86 - Happy Numbers(defn power-set
;  [my-set]
;  (into #{} (conj (reduce (fn [set item]
;                            (println set)
;                            (if (empty? set)
;                              (conj set #{item})
;                              (concat set #{#{item}} (map #(conj % item) set)))) #{} my-set) #{})))
(defn happy-numbers
  [my-number]
  (let [sum-of-squares (fn [my-number]
                         (reduce (fn [num digit]
                                   (let [digit' (Integer/parseInt digit)]
                                     (+ num (* digit' digit'))))
                                 0
                                 (filter not-empty (clojure.string/split (str my-number) #""))))]
    (loop [x my-number
           y #{}]
      (let [x' (sum-of-squares x)]
        (print "#")
        (cond
          (= 1 x') true
          (y x') false
          :else (recur x' (conj y x'))
          )))))

(defn happy?
  ([my-number]
   (happy? my-number #{}))
  ([my-number my-set]
   (let [x' (sum-of-squares my-number)]
     (cond
       (= 1 x') true
       (my-set x') false
       :else (recur x' (conj my-set x'))))))

(defn nihan
  [my-number]
  (+ (* (mod my-number 10) (mod my-number 10))
     (if (= 0 my-number)
       0
       (nihan (quot my-number 10)))))

; Question 88 - Symmetric Difference
(defn symmetric-diff
  [coll1 coll2]
  (into #{} (remove #(and (contains? coll1 %)
                          (contains? coll2 %)) (concat coll1 coll2))))

; Question 90 - Cartesian Product
(defn cartesian-product
  [keys values]
  (if (empty? values)
    #{}
    (into #{} (concat (map vector keys (repeat (count keys) (first values)))
                      (cartesian-product keys (rest values))))))

; Question 93 - Partially Flatten a Sequence
(defn partial-flatten
  [coll]
  (println coll)
  (cond
    (not (coll? (first coll))) [coll]
    (= 1 (count coll)) (partial-flatten (first coll))
    :else (concat (partial-flatten (first coll)) (partial-flatten (rest coll)))))

; Question 95 - To Tree, or not to Tree
(defn tree-or-not
  [[first second third :as sequence]]
  (if (= (count sequence) 3)
    (cond
      (coll? first) (tree-or-not first)
      (coll? second) (tree-or-not second)
      (coll? third) (tree-or-not third)
      :else (not (some false? sequence)))
    false))

; Question 96 - Beauty is Symmetry
(defn beauty-is-symmetry
  [[head left right]]
  (= left ((fn mirroring [[head left right :as %]]
             (cond
               (and (coll? left) (coll? right))
               (conj [head] (mirroring right) (mirroring left))
               (and (coll? left) (not (coll? right)))
               (conj [head] right (mirroring left))
               (and (not (coll? left))
                    (coll? right)) (conj [head] (mirroring right) left)
               :else (conj [head] right left))) right)))

; Question 97 - Pascal's Triangle ; letfn lets you use things before they're defined
; letfn - this is not a good example of a good code, it allows you to do MUTUAL recursion
(defn pascals-triangle
  [r]
  (letfn [(c [n k]
            (/ (! n) (! k) (! (- n k))))
          (! [x]
            (apply * (range 1 (inc x))))]
    (map #(c (dec r) %)
         (range (inc r)))))

(defn pascals-triangle
  [r]
  (let [! (fn [x]
            (apply * (range 1 (inc x))))
        c (fn [n k]
            (/ (! n) (! k) (! (- n k))))]
    (map #(c (dec r) %)
         (range r))))
; Question 98 - Equivalence Classes
(defn equivalence-classes
  [f coll]
  (into #{} (map #(into #{} %) (vals (group-by f coll)))))

; Question 99 - Product Digits
(defn product-digits
  [x y]
  (map #(Integer/parseInt %)
       (filter not-empty (clojure.string/split
                           (str (* x y)) #""))))

; Question 100 - Least Common Multiple
(defn least-common-multiple
  ([first-num & tail]
   (least-common-multiple (cons (vector first-num) tail)))
  ([[first-num & tail]]
   (if (every? #(= (rem (last first-num) %) 0) tail)
     (last first-num)
     (least-common-multiple (cons (conj first-num (+ (first first-num) (last first-num))) tail)))))

;Question 102 - intoCamelCase
(defn camel-case
  [s]
  (reduce (fn [my-str item]
            (if (= (last my-str) \-)
              (str (clojure.string/replace my-str #"-" "") (clojure.string/upper-case item))
              (str my-str item))) "" s))

; Question 103 - Generation k-combinations
(defn k-combos
  [x coll]
  (let [power-sets (fn [my-set]
                     (into #{}
                           (conj (reduce (fn [set item]
                                           (if (empty? set)
                                             (conj set #{item})
                                             (concat set #{#{item}} (map #(conj % item) set)))) #{} my-set) #{})))]
    (into #{} (remove #(not= (count %) x) (power-sets coll)))))

; Question 104 - Write Roman Numerals
(defn num-to-roman
  [x]
  (cond
    (< x 4) (apply str (repeat x "I"))
    (= x 4) "IV"
    (= x 5) "V"
    (< x 9) (apply str "V" (num-to-roman (- x 5)))
    (= x 9) "IX"
    (= x 10) "X"
    (< x 40) (apply str "X" (num-to-roman (- x 10)))
    (< x 50) (apply str "XL" (num-to-roman (- x 40)))
    (< x 60)(apply str "L" (num-to-roman (- x 50)))
    (< x 70) (apply str "LX" (num-to-roman (- x 60)))
    (and (>= x 90) (< x 100)) (apply str "XC" (num-to-roman (- x 90)))
    (< x 500) (apply str "C" (num-to-roman (- x 100)))
    (and (>= x 900) (< x 1000)) (apply str "CM" (num-to-roman (- x 900)))
    (< x 1000) (apply str "D" (num-to-roman (- x 500)))
    :else (apply str "M" (num-to-roman (- x 1000)))
    ))

(defn roman-to-number
  ([s]
   (roman-numerals (reverse s) 0))
  ([s num]
   (if (empty? s)
     num
     (cond
       (= (first s) \I) (roman-numerals (rest s) (inc num))
       (and (= (first s) \X) (= (second s) \I)) (roman-numerals (drop 2 s) (+ 9 num))
       (= (first s) \X) (roman-numerals (rest s) (+ 10 num))
       (and (= (first s) \V) (= (second s) \I)) (roman-numerals (drop 2 s) (+ 4 num))
       (= (first s) \V) (roman-numerals (rest s) (+ 5 num))
       (and (= (first s) \C) (= (second s) \X)) (roman-numerals (drop 2 s) (+ 90 num))
       (= (first s) \C) (roman-numerals (rest s) (+ 100 num))
       (and (= (first s) \L) (= (second s) \X)) (roman-numerals (drop 2 s) (+ 40 num))
       (= (first s) \L) (roman-numerals (rest s) (+ 50 num))
       (= (first s) \D) (roman-numerals (rest s) (+ 500 num))
       (and (= (first s) \M) (= (second s) \C)) (roman-numerals (drop 2 s) (+ 900 num))
       (= (first s) \M) (roman-numerals (rest s) (+ 1000 num))
       :else "else"))))

; Question 105 - Identify keys and values
(defn identify-k-v
  [coll]
  (into {}
        (reduce (fn [my-list value]
                  (println my-list)
                  (if (keyword? value)
                    (reverse (conj my-list [value []]))
                    (conj (butlast my-list) [(first (last my-list)) (conj (second (last my-list)) value)]))) [] coll)))
; Question 107
(fn [x]
  (fn [n]
    (reduce * 1 (repeat x n))))

; Question 108 - Lazy Searching
(defn lazy-searching
  [first-coll & rest-of-colls]
  (loop [x first-coll
         y rest-of-colls]
    (let [searched-item (first x)
          searched-coll (first y)]
      #_(println searched-item (take 3 searched-coll) (some #(= searched-item %) searched-coll))
      (cond
        (empty? searched-coll) searched-item
        (= searched-item (first (drop-while #(> searched-item %) searched-coll))) (recur x (rest y))
        :else (recur (rest x) rest-of-colls)))))

; Question 115 - The Balance of N
(defn balanced?
  [num]
  (let [num' (str num)
        char->int (fn [x] (- (int x) 48))
        half (fn [x] (quot (count x) 2))
        reduce' (fn [coll]
                  (reduce (fn [sum digit]
                            (+ sum (char->int digit))) 0
                          coll))]
    (if (= (rem (count num') 2) 0)
      (= (reduce' (take (half num') num'))
         (reduce' (drop (half num') num')))
      (= (reduce' (take (+ (half num') 1) num'))
         (reduce' (drop (half num') num'))))))

; Question 116 - Prime Sandwich
(defn prime-sandwich
  [num]
  (let [prime-list (reduce (fn [my-list x]
                             (if (> (last my-list) num)
                               (reduced my-list)
                               (if (is-prime-alternative? x)
                                 (conj my-list x)
                                 my-list))) [0] (range))
        last-three (take-last 3 prime-list)
        before (first last-three)
        after (last last-three)]
    (= num (quot (+ before after) 2))))

; Question 116 - alternative bc 4clojure doesnt like reduced :(
(defn prime-sandwich-alternative
  [my-number]
  (let [is-prime? (fn [x] (reduce (fn [b num]
                                    (and b (if (zero? (rem x num))
                                             false
                                             true)))
                                  true (range 2 x)))
        prime-list (loop [x 0
                          y [0]]
                     (cond
                       (= (last (butlast y)) my-number) y
                       (> (last y) my-number) [0 0 0]
                       (is-prime? x) (recur (inc x) (conj y x))
                       :else (recur (inc x) y)))
        last-three (take-last 3 prime-list)
        before (first last-three)
        after (last last-three)]
    (if (< my-number 5)
      false
      (= my-number (quot (+ before after) 2)))))

(defn happy-numbers
  [my-number]
  (let [sum-of-squares (fn [my-number]
                         (reduce (fn [num digit]
                                   (let [digit' (Integer/parseInt digit)]
                                     (+ num (* digit' digit'))))
                                 0
                                 (filter not-empty (clojure.string/split (str my-number) #""))))]
    (loop [x my-number
           y #{}]
      (let [x' (sum-of-squares x)]
        (print "#")
        (cond
          (= 1 x') true
          (y x') false
          :else (recur x' (conj y x'))
          )))))
; Question 118 - Re-implement Map
(defn my-map
  [f [head & tail :as coll]]
  (lazy-seq (cons (f head) (when (not-empty tail)
                             (my-map f tail)))))

; Question 120 - Sum of square of digits
(defn sum-of-square-digits
  [coll]
  (reduce (fn
            [count element]
            (if (< element
                   (reduce (fn
                             [sum item]
                             (+ sum (* item item))) 0
                           (map #(Character/digit % 10) (str element))))
              (inc count)
              count))
          0 coll))

; Question 122 - Read Binary
(defn read-binary
  [str]
  (reduce +
          (map * (take (count str) (iterate #(* 2 %) 1))
               (reverse (map #(Integer/parseInt %)
                             (filter not-empty (clojure.string/split str #"")))))))
; !!!!
(BigInteger. "10101010101" 2)

; Question 122 - Alternative
(defn binary'
  [str]
  (int (reduce +
               (map * (map #(Math/pow 2 %) (range (count str)))
                    (reverse (map #(Integer/parseInt %)
                                  (filter not-empty (clojure.string/split str #""))))))))

; Question 128 - Recognize Playing Cards
(defn playing-cards
  [[suit rank]]
  {:suit ({\S :spades \H :heart \D :diamond \C :club} suit)
   :rank ({\T 8 \J 9 \Q 10 \K 11 \A 12} rank (- (int rank) 50))})

; Question 132 - Insert between two items
(defn insert-between-two
  [pred val [head & tail]]
  (if (nil? head)
    []
    (lazy-seq (cons head
                    (if (nil? tail)
                      []
                      (if (pred head (first tail))
                        (cons val (insert-between pred val tail))
                        (insert-between pred val tail)))))))

; Question 134
(false? ((fn
           [key map]
           (if (and (contains? map key) (= nil (get map key)))
             true
             false)) :c {:a nil :b 2}))

; Question 135 - Infix Calculator
(defn infix-cal'
  ([x operator y & more]
   (apply infix-cal'
          (operator x y) more))
  ([x]
   x))

; Question 136 - Through the Looking Class
(let [x Class]
  (and (= (class x) x) x))

; Question 132 - Digits and bases
(defn digits-and-bases
  [number base]
  (map #(Character/digit % 10) (Integer/toString number base)))

; Question 141 - Tricky card games
(defn tricky-card-games
  [trump-suit]
  (fn [played-cards]
    (if (nil? trump-suit)
      (reduce (fn [winning-card card]
                (if (and (= (:suit card) (:suit winning-card)) (> (:rank card) (:rank winning-card)))
                  card
                  winning-card)) (first played-cards) (rest played-cards))
      (reduce (fn [winning-card card]
                (if
                  (or (and (= (:suit winning-card) trump-suit) (= (:suit card) trump-suit) (> (:rank card) (:rank winning-card)))
                      (and (not (= (:suit winning-card) trump-suit)) (= (:suit card) trump-suit))
                      (and (not (= (:suit winning-card) trump-suit)) (> (:rank card) (:rank winning-card))))
                  card
                  winning-card
                  )) (first played-cards) (rest played-cards)))))

; Question 143 - dot product
(defn dot-product
  [coll1 coll2]
  (reduce + (map * coll1 coll2)))

; Question 144 - Oscilrate
(defn oscilrate
  [num & fs]
  (lazy-seq
    (cons num
          (apply oscilrate ((first fs) num) (conj (into [] (rest fs)) (first fs))))))

; Question 145 - For the win
(= '(1 5 9 13 17 21 25 29 33 37) (for [[x y] (partition 2 (range 20))]
                                   (+ x y)))

; Question 146 - Trees into tables
(defn trees-into-tables
  [coll]
  (into {} (for [[k1 v1] coll
                 [k2 v2] v1]
             [[k1 k2] v2])))

; Question 147 - Pascal's Trapezoid
(defn pascals-triangle''
  [[head & tail :as coll]]
  (lazy-seq (cons coll
                  (pascals-triangle''
                    (cons head (map +' coll (conj (into [] tail) 0)))))))

; Question 153
((fn
   [set]
   (= (reduce + (map count set))
      (count (reduce clojure.set/union #{} set)))) #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})

; Question 156 - Map Defaults
(= ((fn
      [value keys]
      (reduce (fn [m k] (assoc m k value))                  ;assoc takes map key val and adds it
              {} keys)) [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})


; Question 157 - Indexing Sequences
(defn index-seq
  [coll]
  (map vector coll (range)))

; Question 157 - Alternative
(defn index-seq'
  [coll]
  (map-indexed (fn [x y]
                 (vector y x)) coll))

; Question 161 Subset and Superset
(clojure.set/subset? #{1 2} #{1 2})

; Question 162 - Logical falsity and truth
(= 1 (if true 1 0))

;166
(defn comp'
  [x y]
  (< (count x) (count y)))

(fn compare'
  [f x y]
  (cond
    (f x y) :lt
    (f y x) :gt
    :else :eq))

; Question 173 - Intro to Destructuring 2 -> Sequential destructuring
(let [[f x] [+ (range 3)]]
  (apply f x))

(let [[[f x] b] [[+ 1] 2]]
  (f x b))

(let [[f x] [inc 2]]
  (f x))

; An example that makes this easier to understand ->
(let [[a b c & d :as e] [1 2 3 4 5 6 7]]
  [a b c d e])

; Question 177 - Balancing Brackets
(defn matching?
  [s]
  (empty? (reduce (fn [my-brackets c]
                    (println my-brackets)
                    (cond
                      (or (and (= (last my-brackets) \{) (= c \}))
                          (and (= (last my-brackets) \() (= c \)))
                          (and (= (last my-brackets) \[) (= c \]))) (into [] (butlast my-brackets))
                      (or (= c \[) (= c \() (= c \{) (= c \]) (= c \)) (= c \})) (conj my-brackets c)
                      :else my-brackets)) [] s)))


;Notes
;; MAPCAT PARTIAL (mapcat (partial repeat 3) [1 2 3])

;user=> (def hundred-times (partial * 100))
;#'user/hundred-times

;user=> (hundred-times 5)
;500

; conj adds an element to the end, cons adds it to the beginning
; '((1 2) 3 [4 [5 6]])

; partial
(def add1 (partial + 1))
(= (add1 2 3 4) (+ 1 2 3 4))

(fn calc [& exp]
  (reduce #(if (fn? %1) (%1 %2) (partial %2 %1)) identity exp))


;REDUCED
(reduce (fn
          [x item]
          (if (< item 10)
            (+ x item)
            (reduced x))) 0 (range))


; Destructuring maps
(def client {:name        "Super Co."
             :location    "Philadelphia"
             :description "The worldwide leader in plastic tableware."})

(let [name (:name client)
      location (:location client)
      description (:description client)]
  (println name location "-" description))

(let [{name :name :as all} client]
  (println "The name from" all "is" name))

(let [{:strs [name location description]} client]
  (println name location "-" description))

(println (:name client))

; Associative destructuring also allows you to supply a default value
; if the key is not present in the associative value with the :or key.
(let [{category :category, :or {category "Category not found"}} client]
  (println category))

(def my-map {:a "A" :b "B" :c 3 :d 4})

(let [{a :a, x :x, :or {x "Not found!"}, :as all} my-map]
  (println "I got" a "from" all)
  (println "Where is x?" x))


(def string-keys {"first-name" "Joe" "last-name" "Smith"})

(let [{:strs [first-name last-name]} string-keys]
  (println first-name last-name))

(defn configure [val options]
  (let [{:keys [debug verbose] :or {debug false, verbose false}} options]
    (println "val =" val " debug =" debug " verbose =" verbose)))

(configure 12 {:debug true})

; FLATTEN
(defn my-flatten
  [coll] (mapcat (fn [x] (if (coll? x) (my-flatten x) [x])) coll))

(defn my-map-flatten
  ([m] (apply hash-map (my-map-flatten [] m)))
  ([path m]
   #_(println path m)
   (mapcat (fn [[k v]]
             (if (map? v)
               (my-map-flatten (conj path k) v)
               [(conj path k) v]))
           m)))

(interleave)


;apply for collections
;partial is like anonymous function but can take multiple arguments
;put the function as the first elements ((partial + 2) 2 4) -> (+ 2 2 4)

; REDUCE KV

(defn count-occurrences
  [coll]
  (reduce-kv (fn [m k v]
               (assoc m k (count v)))
             {} (group-by identity coll)))

(defn count-occurrences'
  [coll]
  (reduce (fn [m [k v]]
            (assoc m k (count v)))
          {} (group-by identity coll)))

; FREQUENCIES
(defn count-occurrences
  [coll]
  (reduce-kv (fn [m k v]
               (assoc m k (count v)))
             {} (group-by identity coll)))


(defn scratchy
  [my-vector]
  (reduce-kv (fn [m k v]
               (if (= (:name (first (vals v))) "vala")
                 (assoc m :id (:id (first (vals v))))
                 m)) {} my-vector))
