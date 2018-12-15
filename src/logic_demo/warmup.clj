;; Written by Fereidoon Mehri. December/2018.

(ns logic-demo.warmup
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(run* [q]
  succeed)
(run* [q]
  (== q q))
(run* [q]
  (== q 2))
(run* [q]
  (== q 2)
  (== q q)
  (== q 1))
(run* [q]
  (conde
   [succeed succeed]
   [succeed succeed])
  (== q 2)
  (== q q)
  (== q 2))
(run* [q]
  (conde
   [succeed succeed]
   [succeed succeed]
   [succeed succeed]
   [succeed])
  (== q 2)
  (== q q)
  (== q 2))
(run* [q]
  (conde
   [succeed succeed]
   [succeed succeed succeed]
   [succeed succeed succeed succeed]
   [succeed])
  (== q 2)
  (== q q)
  (== q 2))
(run* [q]
  (conde
   [succeed succeed]
   [succeed succeed succeed]
   [succeed (== q 1) succeed succeed succeed]
   [succeed succeed])
  (== q 2)
  (== q q)
  (== q 2))
(run* [q]
  (conde
   [(!= q 2) succeed succeed]
   [(!= q 1) succeed succeed succeed]
   [succeed (== q 1) succeed succeed succeed]
   [succeed succeed])
  (== q 2)
  (== q q)
  (== q 2))
(run* [q]
  (conde
   [(!= q 2) succeed succeed]
   [(!= q 1) succeed succeed succeed]
   [succeed (== q 1) succeed succeed succeed]
   [])
  (== q 2)
  (== q q)
  (== q 2))

(run* [q]
  (fresh [c]
    (== c q)))
(run* [q]
  (fresh [c]
    (== c q))
  (== q 2))
(run* [q]
  (fresh [c]
    (== c q)
    (== c 4))
  (== q 2))
(run* [q]
  (fresh [c]
    (== c q)
    (== c 4))
  (== q 4))

(run* [a b]
  (fresh [c]
    (== a b)
    (== a c)))
(run* [a b]
  (fresh [c]
    (== a b)
    (== a c)
    (== b 's)))
(run* [a b]
  (fresh [c]
    (membero [a b] c)
    (== c [1 2 3 4 5])
    ))
(run* [a b]
  (fresh [c]
    (== c [1 2 3 4 5])
    (membero [a b] c)
    ))
(run* [a b]
  (fresh [c]
    (== c [1 2 3 4 5])
    (membero a c)
    ))
(run* [a b]
  (fresh [c]
    (== c [1 2 3 4 5])
    (membero a c)
    (membero b c)
    ))
(run* [a b]
  (fresh [c]
    (== c [1 2 3 4 5])
    (membero a c)
    (membero b c)
    (!= a b)
    ))

(defn my-membero [e l]
  (conde
   [(emptyo l) fail]
   [succeed (fresh [h t]
              (conso h t l)
              (conde
               [(== h e) succeed]
               [(my-membero e t) succeed]))]))

(run* [a b]
  (fresh [c]
    (== c [1 2 3])
    (my-membero a c)
    (my-membero b c)
    (!= a b)
    ))

(defn my-membero2 [e l]
  (conde
   [(fresh [h t]
      (conso h t l)
      (conde
       [(== h e)]
       [(my-membero2 e t)]))]))

(run* [a b]
  (fresh [c]
    (== c [1 2 3])
    (my-membero2 a c)
    (my-membero2 b c)
    (!= a b)
    ))

(run* [r]
  (fresh [x y]
    (== (lcons x (lcons y 'salad)) r)))
;; EXERCISE 1: Write listo.
(run 1 [x]
  (listo (llist 'a 'b 'c x)))


;; Answer 1:
(defn listo [l]
  (conde
   [(emptyo l)]
   [(fresh [h t]
      (conso h t l)
      (listo t))]))

(run 5 [x]
  (listo (llist 'a 'b 'c x)))

;; EXERCISE 2: Write my-rembero.
(run 1 [x]
  (my-rembero 'peas '(a b peas d peas e) x))

;; Answer 2:
(defn my-rembero [e l o]
  (conde
   [(emptyo l) (== o l)]
   [(fresh [h t tmp-o]
      (conso h t l)
      (conde
       [(== h e)
        (== o t)]
       [(conso h tmp-o o) (my-rembero e t tmp-o)]))]))


(run* [x]
  (my-rembero 'peas '(a b peas d peas e) x))

;; Home Exercise 1: Write memo so that it passes these tests:
(comment
  (test-check "4.10"
              (run 1 (out) 
                (memo 'tofu `(a b tofu d tofu e) out))
              `((tofu d tofu e)))

  (test-check "4.11"
              (run 1 (out) 
                (fresh (x)
                  (memo 'tofu `(a b ,x d tofu e) out)))
              `((tofu d tofu e)))

  (test-check "4.12"
              (run* (r)
                (memo r `(a b tofu d tofu e) `(tofu d tofu e)))
              (list `tofu))
  )



(def surpriseo
  (fn [s]
    (rembero s '(a b c) '(a b c))))
(def my-surpriseo
  (fn [s]
          (my-rembero s '(a b c) '(a b c))))

(run* (r)
  (== 'd r)
  (my-surpriseo r))
(run* (r)
  (my-surpriseo r))
(run* (r)
  (== 'b r)
  (my-surpriseo r))
(run* (r)
  (surpriseo r))
(run* (r)
  (== 'b r)
  (surpriseo r))
(run* (r)
  (== 'd r)
  (surpriseo r))
(run* [r]
  (rembero 'd '(a b c) '(a b c)))
(run* [r]
  (rembero 'a '(a b c) '(b c)))
