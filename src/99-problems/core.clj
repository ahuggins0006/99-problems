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

;; P06 Find oaut whether a list is a palindrome.

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
     (println [x xs acc accc ctr])
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
     (println [x xs acc])
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
     (println [x xs acc n count])
     (cond
       (empty? all) (my-reverse acc)
       (zero? count) (recur xs n acc n)
       :else (recur all n (conj acc x) (dec count))))))

(repli '(a b c) 3)
;; => (a a a b b b c c c)
