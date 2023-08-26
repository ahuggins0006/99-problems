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
     (empty? all) '()
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
       (empty? all) (reverse acc)
       (= x (first acc)) (recur xs acc)
       :else (recur xs (conj acc x))))))

(compress '(a a a a b c c a a d e e e e))
;; => (a b c a d e)
