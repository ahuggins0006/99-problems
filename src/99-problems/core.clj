(ns core)

;; P01 Find the last box of a list.

(defn my-last [l]
  (let [[x & xs :as all] l]
    (cond
      (empty? all) '()
      (empty? xs) x
      :else (recur xs))))

(my-last '(a b c d))
;; => d

;; P02 Find the last but one box of a list.

(defn my-but-last [l]
  (let [[x y & xs :as all] l]
    (cond
      (empty? all) '()
      (empty? xs) (list x y)
      :else (recur xs))))

(my-but-last '(a b c d))
;; => (c d)
(my-but-last [1 2 3 4])
;; => (3 4)


;; P03 Find the K'th element of a list

(defn element-at [l n]
  (let [[x & xs :as all] l]
    (cond
      (empty? all) '()
      (empty? xs) x
      (<= n 0) '()
      (= n 1) x
      :else (recur xs (dec n))
      )
    ))

(element-at '(a b c d e) 3)
;; => c

;; P04 Find the number of elements of a list

(defn my-count
  ([l] (my-count l 1))
  ([l acc]
  (let [[_ & xs :as all] l]
   (cond
     (empty? all) 0
     (empty? xs)  acc
     :else (recur xs (inc acc))
     )

   )))

(my-count '(a b c d e))
;; => 5

;; P05 Reverse a list.
(defn my-reverse
  ([l] (my-reverse l '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) acc 
       (empty? xs)  (conj acc x)
       :else (recur xs (conj acc x))))))

(my-reverse '(a b c d e))
;; => (e d c b a)

;; P06 Find out whether a list is a palindrome.

(defn palindrome? [l]
  (= l (my-reverse l))
  )

(palindrome? '(x a m a x))
;; => true
(palindrome? '(r a c e c a r))
;; => true

;; P07 Flatten a nested list structure.

(defn my-flatten
  ([l] (my-flatten l '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (and (list? x) (empty? xs)) (recur x acc)
       (and (list? x) (empty? x)) (recur xs acc)
       (and (list? x) (not-empty x)) (recur (list (first x) (rest x) xs) acc)
       :else (recur xs (conj acc x))))))

(my-flatten '(a (b (c d) e)))
;; => (a b c d e)

;; P08 Eliminate consecutive duplicates of list elements.

(defn compress
  ([l] (compress l '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (= x (first acc)) (recur xs acc)
       :else (recur xs (conj acc x))))))

(compress '(a a a a b c c a a d e e e e))
;; => (a b c a d e)

;; P09 Pack consecutive duplicates of list elements into sublists.

(defn pack
  ([l] (pack l '() '()))
  ([l acc accc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (rest (my-reverse (conj accc acc)))
       (not= x (first acc)) (recur xs (conj '() x) (conj accc acc))
       :else (recur xs (conj acc x) accc)))))

(pack '(a a a a b c c a a d e e e e))
;; => ((a a a a) (b) (c c) (a a) (d) (e e e e))

;; P10 Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

(defn encode
  ([l] (encode (pack l) '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       :else (recur xs (conj acc (list (my-count x) (first x))))))))

(encode '(a a a a b c c a a d e e e e))
;; => ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))

;; P11 Modified run-length encoding.
;; Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

(defn encode-modified
  ([l] (encode-modified (pack l) '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (= (my-count x) 1) (recur xs (conj acc (first x)))
       :else (recur xs (conj acc (list (my-count x) (first x))))))))

(encode-modified '(a a a a b c c a a d e e e e))
;; => ((4 a) b (2 c) (2 a) d (4 e))

;; P12 Decode a run-length encoded list.
;; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.

(defn decode
  ([l] (decode l '()))
  ([l acc]

   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (not (list? x)) (recur xs (conj acc x))
       (> (first x) 1) (recur (conj xs (list (dec (first x)) (last x))) (conj acc (last x)))
       :else (recur xs (conj acc (last x)))))))

(decode '((4 a) b (2 c) (2 a) d (4 e)))
;; => (a a a a b c c a a d e e e e)

;; P13 Run-length encoding of a list (direct solution).
;; Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

(defn encode-direct
  ([l] (encode-direct l '() '() 1))
  ([l acc accc ctr]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (rest (my-reverse (conj accc (list ctr (first acc)))))
       (and (not= x (first acc)) (> ctr 1)) (recur xs (conj '() x) (conj accc (list ctr (first acc))) 1)
       (and (not= x (first acc)) (= ctr 1)) (recur xs (conj '() x) (conj accc (first acc)) 1)
       :else (recur xs (conj acc x) accc (inc ctr))))))

(encode-direct '(a a a a b c c a a d e e e e))
;; => ((4 a) b (2 c) (2 a) d (4 e))

;; P14 Duplicate the elements of a list.

(defn dupli
  ([l] (dupli l '()))
  ([l acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       :else (recur xs (conj acc x x))))))

(dupli '(a b c c d))
;; => (a a b b c c c c d d)

;; P15 Repilcate the elements of a list of a given number of times.

;; Example:
;; * (repli '(a b c) 3)
;; (A A A B B B C C C)

(defn repli
  ([l n] (repli l n '() n))
  ([l n acc count]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (zero? count) (recur xs n acc n)
       :else (recur all n (conj acc x) (dec count))))))

(repli '(a b c) 3)
;; => (a a a b b b c c c)

;; P16 Drop every n'th element from a list

(defn my-drop
  ([l n] (my-drop l n '() (dec n)))
  ([l n acc count]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (zero? count) (recur xs n acc (dec n))
       :else (recur xs n (conj acc x) (dec count))))))

(my-drop '(a b c d e f g h i k) 3)
;; => (a b d e g h k)
(= (my-drop '(a b c d e f g h i k) 3) '(a b d e g h k))
;; => true

;; P17 Split a list into two parts; the length of the first part is given.

(defn my-split
  ([l n] (my-split l n '()))
  ([l n acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (my-reverse acc)
       (zero? n) (list (my-reverse acc) (conj xs x))
       :else (recur xs (dec n) (conj acc x))))))

(my-split '(a b c d e f g h i k) 3)
;; => ((a b c) (d e f g h i k))
(= (my-split '(a b c d e f g h i k) 3) '((a b c) (d e f g h i k)))
;; => true

;; P18 Extract a slice from a list.
;; Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.

(defn my-slice
  ([l i k] (my-slice l (dec i) (inc (- k i)) '()))
  ([l i k acc]
   (let [[x & xs :as all] l] (cond
       (empty? all) (my-reverse acc)
       (and (zero? i) (zero? k)) (recur '() i k acc)
       (zero? i) (recur xs i (dec k) (conj acc x))
       :else (recur xs (dec i) k acc)))))

(my-slice '(a b c d e f g h i k) 3 7)
;; => (c d e f g)
(= (my-slice '(a b c d e f g h i k) 3 7) '(c d e f g))
;; => true

;; P19 Rotate a list N places to the left.

(defn my-rotate [l n]
  (if (pos? n)
    (my-flatten (my-reverse (my-split l n)))
    (my-flatten (my-reverse (my-split l (+ (my-count l) n))))))

(my-rotate '(a b c d e f g h) 3)
;; => (d e f g h a b c)
(= (my-rotate '(a b c d e f g h) 3) '(d e f g h a b c))
;; => true
(my-rotate '(a b c d e f g h) -2)
;; => (g h a b c d e f)
(= (my-rotate '(a b c d e f g h) -2) '(g h a b c d e f))
;; => true

;; P20 Remove the k'th element from a list.
;;

(defn remove-at
  ([l n] (remove-at l (dec n) '()))
  ([l n acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (reverse acc)
       (zero? n) (recur xs (dec n) acc)
       :else (recur xs (dec n) (conj acc x))))))

(remove-at '(a b c d) 2)
;; => (a c d)
(= (remove-at '(a b c d) 2) '(a c d))
;; => true

;; P21 Insert an element at a given position into a list

(defn insert-at
  ([som l n] (insert-at som l (dec n) '()))
  ([som l n acc]
   (let [[x & xs :as all] l]
     (cond
       (empty? all) (reverse acc)
       (zero? n) (recur som xs (dec n) (conj acc som x))
       :else (recur som xs (dec n) (conj acc x))))))

(insert-at 'alfa '(a b c d) 2)
;; => (a alfa b c d)
(= (insert-at 'alfa '(a b c d) 2) '(a alfa b c d))
;; => true

;; P22 Create a list containing all integers within a given range
;; If first argument is smaller than second, produce a list in decreasing order.
(defn my-range
  ([i k] (my-range i k '()))
  ([i k acc]
   (cond
     (= i k) (my-reverse (conj acc i))
     (> i k) (recur (dec i) k (conj acc i))
     :else (recur (inc i) k (conj acc i)))))

(my-range 4 9)
;; => (4 5 6 7 8 9)

(my-range 9 4)

;; P23 Extract a given number of randomly selected elements from a list.
;; The selected items shall be returned in a list.
;; Example:
;; * (rnd-select '(a b c d e f g h) 3)
(defn rnd-select
  ([l n] (rnd-select l n '()))
  ([l n acc]
   (let [rando (inc (rand-int (my-count l)))]
     (cond
       (zero? n) (my-reverse acc)
       :else (recur (remove-at l rando) (dec n) (conj acc (element-at l rando)))))))

(rnd-select '(a b c d e f g h) 3)
;; => (e d b)

;; P24 Lotto: Draw N different random numbers from the set 1..M.
;; The selected numbers shall be returned in a list.
;; Example:
;; *(lotto-select 6 49)
;; (23 1 17 33 21 37)

(defn lotto-select [i m]
  (rnd-select (my-range 1 m) i))

(lotto-select 6 49)
;; => (38 9 41 21 12 15)
(= (lotto-select 6 49) '(23 1 17 33 21 37))
;; => false

;; P25 Generate a random permutation of the elements of a list
;; Example:
;; *(rnd-permu '(a b c d e f))
;; (B A D C E F)

(defn rnd-permu [l]
  (rnd-select l (my-count l))
  )

(rnd-permu '(a b c d e f));; => (c d a e f b)

;; P26 Generate the combinations of K distinct object chosen from the N elements of a list

(defn combination
  ([n l] (cond
          (> n 1)   (concat (combination (dec n) l '() '() l) (combination (dec n) (my-reverse l) '() '() (my-reverse l)))
          (= n 1)   l
          (zero? n) 1
          (< n 1)   0
          ))
  ([n l acc accc orig]

   (let [[x & xs :as all] l]
     (cond
       (empty? orig) (my-reverse accc)
       (empty? all)  (recur n (rest orig) '() accc (rest orig))
       (= (my-count acc) n) (recur n xs acc (conj accc (my-reverse (conj acc x))) orig)
       :else (recur n xs (conj acc x) accc orig)))))

(combination 3 '(a b c d e f))
;; => ((a b c) (a b d) (a b e) (a b f) (b c d) (b c e) (b c f) (c d e) (c d f) (d e f) (f e d) (f e c) (f e b) (f e a) (e d c) (e d b) (e d a) (d c b) (d c a) (c b a))

(combination 5 '(a b c d e f))
;; => ((a b c d e) (a b c d f) (b c d e f) (f e d c b) (f e d c a) (e d c b a))

(combination 1 '(a b c d e f))
;; => (a b c d e f)

(combination 0 '(a b c d e f))
;; => 1

(combination -1 '(a b c d e f))
;; => 0

;; P27 Group the elements of a set into disjoint subsets.
;; a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
;; Example:
;; * (group3 '(aldo beat carla david evi flip gary hugo ida))
;; b) Generalize the above function in a way that we can specify a list of group sizes and the funtion will return a list of groups.


(defn group
  ([l counts] (group (reverse l) '() '() '() counts (reverse l) counts))
  ([l acc accc acccc counts orig-l orig-counts]
   (let [[x & xs :as all] l
         [c & cts :as counts] counts]
     (cond
       (and (> (count acccc) 1) (= (first acccc) (last acccc))) (reverse (rest acccc))
       (empty? all) (recur (concat (rest orig-l) (list (first orig-l))) '() '() (conj acccc (reverse (conj accc (reverse acc)))) orig-counts (concat (rest orig-l) (list (first orig-l))) orig-counts)
       (= (count acc) c) (recur all '() (conj accc (reverse acc)) acccc cts orig-l orig-counts)
       :else (recur xs (conj acc x) accc acccc counts orig-l orig-counts)))))

(group '(aldo beat carla david evi flip gary hugo ida) '(2 3 4))
;; => (((aldo beat) (carla david evi) (flip gary hugo ida)) ((beat carla) (david evi flip) (gary hugo ida aldo)) ((carla david) (evi flip gary) (hugo ida aldo beat)) ((david evi) (flip gary hugo) (ida aldo beat carla)) ((evi flip) (gary hugo ida) (aldo beat carla david)) ((flip gary) (hugo ida aldo) (beat carla david evi)) ((gary hugo) (ida aldo beat) (carla david evi flip)) ((hugo ida) (aldo beat carla) (david evi flip gary)) ((ida aldo) (beat carla david) (evi flip gary hugo)))



