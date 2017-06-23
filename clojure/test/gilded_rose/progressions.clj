(ns gilded-rose.progressions
  (:require
    [gilded-rose.refactored :as refactored]
    [gilded-rose.utils :as u]
    [gilded-rose.core :as c]))

;;
;; GENERAL NOTE
;; If the vector returned from these functions is a tuple then the error
;; output will be the frequencies of all the errors.
;; Thus the case of multiple errors will be handled without too much output.
;; If you add extra info so tuple becomes a triple then you will see every error.
;; Thus keep the #_ comments, and use them when you need to find out more about an error.
;;

;;
;; Between progressions
;; When these true there's a problem
;;
(def names-dont-get-worse-with-age
  #{"Sulfuras, Hand Of Ragnaros"
    "Aged Brie"
    "Backstage passes to a TAFKAL80ETC concert"})
(defn not-decreasing-quality-err [{quality0 :quality name0 :name sell-in0 :sell-in}
                                  {quality1 :quality name1 :name sell-in1 :sell-in}]
  (assert (= name0 name1))
  (if (not (names-dont-get-worse-with-age name0))
    [(neg? (u/probe-off (- quality0 quality1) (str "<" name0 ", " sell-in1 ">"))) name0 #_[quality0 quality1]]
    [false name]))

(defn not-decreasing-sell-in-err [{name0 :name sell-in0 :sell-in}
                                  {name1 :name sell-in1 :sell-in :as two}]
  (assert (= name0 name1) (str "Different names: <" name0 "> <" name1 "> <" two ">"))
  [(not (neg? (- sell-in1 sell-in0))) name0 #_[sell-in0 sell-in1]])

(defn not-compound-improving? [name sell-in improvement]
  (let [expected-improve (cond
                           (= -1 sell-in) -50
                           (neg? sell-in) 0
                           (< sell-in 5) 3
                           (< sell-in 10) 2
                           :default 1)
        res (not= improvement expected-improve)]
    (when res
      (println name improvement expected-improve sell-in))
    res))

(defn not-simple-improving? [name sell-in improvement]
  (let [expected-improve 1
        res (not= improvement expected-improve)]
    (when res
      (println name improvement expected-improve sell-in))
    res))

(def names-compound-better-with-age #{"Backstage passes to a TAFKAL80ETC concert"})
(def names-simply-better-with-age #{"Aged Brie"})
(defn not-increasing-quality-err [{quality0 :quality name0 :name sell-in0 :sell-in}
                                  {quality1 :quality name1 :name sell-in1 :sell-in}]
  (cond
    (names-compound-better-with-age name0)
    [(not-compound-improving? name0 sell-in1 (- quality1 quality0)) name0 #_[quality0 quality1 sell-in1]]

    (names-simply-better-with-age name0)
    [(not-simple-improving? name0 sell-in1 (- quality1 quality0)) name0]

    :default [false name0]))

(def still-increase-or-become-worthless #{"Backstage passes to a TAFKAL80ETC concert" "Aged Brie"})
(def always-twice-as-fast #{"Conjured"})
(defn not-degrade-twice-as-fast-after-err [{quality0 :quality name0 :name sell-in0 :sell-in}
                                           {quality1 :quality name1 :name sell-in1 :sell-in}]
  (cond
    (and (neg? sell-in1) (still-increase-or-become-worthless name0))
    [false name0]

    (and (not= name0 "Sulfuras, Hand Of Ragnaros") (neg? sell-in1))
    (let [degraded-by (- quality1 quality0)]
      (if (always-twice-as-fast name0)
        [(not= -4 degraded-by) name0 #_degraded-by #_sell-in1]
        [(not= -2 degraded-by) name0 #_degraded-by #_sell-in1]))

    :default [false name0]))

#_(defn progress-by [n]
  (->> (iterate update-current-inventory c/start-inventory)
       (take (inc n))))

(defn test-successive-progressions-for [start-inventory update-quality-fn num-progressions test-fn]
  (assert (= 2 (u/arg-count test-fn)) test-fn)
  (let [all-progressions ((u/progress-by update-quality-fn start-inventory) num-progressions)]
    (->> (mapcat #(map test-fn %1 %2) all-progressions (rest all-progressions))
         u/probe-off
         (filter first)
         frequencies)))

(def state-progression-err-fns [not-decreasing-sell-in-err
                                not-decreasing-quality-err
                                not-increasing-quality-err
                                not-degrade-twice-as-fast-after-err])

(defn test-results [start-inventory update-quality-fn num-progressions]
  (map (partial test-successive-progressions-for start-inventory update-quality-fn num-progressions)
       state-progression-err-fns))



