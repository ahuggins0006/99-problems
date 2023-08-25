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
