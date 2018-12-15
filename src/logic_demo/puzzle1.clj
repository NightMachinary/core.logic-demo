(ns logic-play.puzzle
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.tools.macro :as mu]
            [clojure.set :as set]
            [clojure.core.logic.fd :as fd]))

;; -----
;; CLP(Set) Boilerplate
(defn index [xs] (->> xs (map-indexed (fn [i x] [x (inc i)])) (into {})))
(def p->i
  {:name   (index [:amaya :bailey :jamari :jason :landon])
   :cheese (index [:asiago :blue :mascarpone :mozzarella :muenster])
   :mag    (index [:fortune :time :cosmopolitan :us-weekly :vogue])
   :reserv (index [:5pm :6pm :7pm :730pm :830pm])})
(def i->p (into {} (map (fn [[k v]] [k (set/map-invert v)]) p->i)))
(defn ->answer [m] (into {} (map (fn [[k v]] [k (get-in i->p [k v])]) m)))
(defn ->answers [ms] (map ->answer ms))
;; -----

(defn existso [q ps] (fresh [x] (featurec x ps) (membero x q)))

(defn ruleo [q p v tp tv]
  (let [v  (if-not (lvar? v) (-> p->i p v) v)
        tv (if-not (lvar? tv) (-> p->i tp tv) tv)]
    (existso q {p v tp tv})))

(defn neg-ruleo [q p v tp tv]
  (let [tv (if-not (lvar? tv) (-> p->i tp tv) tv)]
    (fresh [x] (!= x tv) (ruleo q p v tp x))))

(defn earliero [q p v op ov]
  (let [v  (-> p->i p v)
        ov (-> p->i op ov)]
    (fresh [x y t0 t1]
      (fd/< t0 t1)
      (existso q {p v :reserv t0})
      (existso q {op ov :reserv t1}))))

(defne peopleo [q ps]
  ([() _])
  ([[h . t] _]
    (let [[k :as kv] (first ps)]
      (featurec h {k (get-in p->i kv)}))
    (peopleo t (next ps))))

(defn puzzle []
  (let [vs (take 20 (repeatedly lvar))
        ps (->> (partition 4 vs)
             (map #(into {} (map vector [:name :cheese :mag :reserv] %)))
             (into []))]
    (run* [q]
      (== q ps)
      (everyg #(fd/in % (fd/interval 1 5)) vs)
      (everyg fd/distinct (apply map vector (map vals ps)))
      (conde
        [(ruleo q :name :landon :reserv :730pm) (ruleo q :name :jason :cheese :mozzarella)]
        [(ruleo q :name :landon :cheese :mozzarella) (ruleo q :name :jason :reserv :730pm)]) ;; 1
      (ruleo q :cheese :blue :mag :fortune) ;; 2
      (neg-ruleo q :cheese :muenster :mag :vogue) ;; 3
      (peopleo q [[:mag :fortune] [:name :landon] [:reserv :5pm] 
                  [:cheese :mascarpone] [:mag :vogue]]) ;; 4
      (neg-ruleo q :reserv :5pm :mag :time) ;; 5
      (earliero q :mag :cosmopolitan :cheese :mascarpone) ;; 6
      (earliero q :cheese :blue :name :bailey) ;; 7
      (conde
        [(ruleo q :reserv :7pm :mag :fortune)]
        [(ruleo q :reserv :730pm :mag :fortune)]) ;; 8
      (earliero q :mag :time :name :landon) ;; 9
      (neg-ruleo q :name :jamari :mag :fortune) ;; 10
      (ruleo q :reserv :5pm :cheese :mozzarella)))) ;; 11

(->answers (first (puzzle)))
(comment
  (time (->answers (first (puzzle))))

  ;; ~84ms for 1
  (dotimes [_ 5] (time (dotimes [_ 10] (->answers (first (puzzle))))))
  )
