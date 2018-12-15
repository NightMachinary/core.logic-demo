;; Of Landon and Jason, one has the 7:30pm reservation and the other loves mozzarella.
;; The blue cheese enthusiast subscribed to Fortune.
;; The muenster enthusiast didn’t subscribe to Vogue.
;; The 5 people were the Fortune subscriber, Landon, the person with a reservation at 5:00pm, the mascarpone enthusiast, and the Vogue subscriber.
;; The person with a reservation at 5:00pm didn’t subscribe to Time.
;; The Cosmopolitan subscriber has an earlier reservation than the mascarpone enthusiast.
;; Bailey has a later reservation than the blue cheese enthusiast.
;; Either the person with a reservation at 7:00pm or the person with a reservation at 7:30pm subscribed to Fortune.
;; Landon has a later reservation than the Time subscriber.
;; The Fortune subscriber is not Jamari.
;; The person with a reservation at 5:00pm loves mozzarella.
(ns logic-demo.puzzle
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require
   [clojure.tools.macro :as mu]
   [clojure.core.logic.fd :as fd]))

(let [people (repeatedly 5 lvar)
      cheeses (repeatedly 5 lvar)
      magazines (repeatedly 5 lvar)
      reservations (repeatedly 5 lvar)]
  (mu/symbol-macrolet
   [_ (lvar)]
   (run 1
     [q]
     (== q (map list people cheeses magazines reservations))
     (everyg #(fd/in % (fd/domain 5 6 7 75 85)) reservations)
     (== people [:Amaya :Bailey :Jamari :Jason :Landon])
     (permuteo [:asiago :blue-cheese :mascarpone :mozzarella :muenster] cheeses)
     (permuteo [:fortune :time :cosmopolitan :us-weekly :vogue] magazines)
     (permuteo [5 6 7 75 85] reservations)
     (membero [_ :blue-cheese :fortune _] q)
     (membero [_ :mozzarella _ 5] q)
     (conde
      [(membero [:Landon _ _ 75] q) (membero [:Jason :mozzarella _ _] q)]
      [(membero [:Jason _ _ 75] q) (membero [:Landon :mozzarella _ _] q)])
     (fresh [moz-res]
       (membero [_ :mozzarella _ moz-res] q)
       (!= moz-res 75)
       (== moz-res 5))
     (fresh [mu-mag]
       (membero [_ :muenster mu-mag _] q)
       (!= mu-mag :vogue))
     (fresh [x]
       (membero [_ _ x 5] q)
       (!= x :time))
     (fresh [a b]
       (membero [_ _ :cosmopolitan a] q)
       (membero [_ :mascarpone _ b] q)
       (fd/< a b))
     (fresh [a b]
       (membero [:Bailey _ _ a] q)
       (membero [_ :blue-cheese _ b] q)
       (fd/> a b))
     (fresh [a b]
       (membero [:Landon _ _ a] q)
       (membero [_ _ :time b] q)
       (fd/> a b))
     (fresh [res sub]
       (membero [sub _ :fortune res] q)
       (conde
        [(== res 75) succeed]
        [(== res 7) succeed])
       (!= sub :Jamari))

     (permuteo
      [[_ _ :fortune _]
       [:Landon _ _ _]
       [_ _ _ 5]
       [_ :mascarpone _ _]
       [_ _ :vogue _]
       ] q)
     )))
