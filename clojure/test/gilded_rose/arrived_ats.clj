(ns gilded-rose.arrived-ats
  (:require [gilded-rose.core :as c]
            [gilded-rose.refactored :as refactored]
            [gilded-rose.utils :as u]))

;;
;; Each progression
;; When these true there's a problem
;;
(defn over-quality-err [{:keys [quality name sell-in]}]
  (if (not= name "Sulfuras, Hand Of Ragnaros")
    [(> quality 50) name]
    [false name]))

(defn under-quality-err [{:keys [quality name sell-in]}]
  [(and (pos? sell-in) (neg? quality)) name #_quality])

#_(def names-better-with-age #{"Aged Brie"
                             "Backstage passes to a TAFKAL80ETC concert"})
(def worthless-after #{"Backstage passes to a TAFKAL80ETC concert"})
(defn not-zero-quality-err [{:keys [quality name sell-in]}]
  [(and (worthless-after name) (neg? sell-in) (pos? quality))
   name
   quality
   sell-in])

(defn test-each-progression-for [start-inventory update-quality-fn num-progressions f]
  (->> (map #(map f %) ((u/progress-by update-quality-fn start-inventory) num-progressions))
       (apply concat)
       ;u/probe-on
       (filter first)
       frequencies))

(def arrived-at-err-state-fns [over-quality-err
                               under-quality-err
                               not-zero-quality-err])

(defn test-results [start-inventory update-quality-fn num-progressions]
  (map (partial test-each-progression-for start-inventory update-quality-fn num-progressions)
       arrived-at-err-state-fns))

