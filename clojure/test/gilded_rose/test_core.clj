(ns gilded-rose.test-core
  (:require [gilded-rose.refactored :as refactored]
            [gilded-rose.progressions :as progressions]
            [gilded-rose.arrived-ats :as arrived-ats]
            [gilded-rose.utils :as u]
            [gilded-rose.core :as orig]))

(def num-progressions (inc (inc 16)))

;;
;; Switch on and off, depending whether want to to use the original or my refactored update-quality function
;;
(def use-refactored? true)
;;
;; The refactored function may additionally support the enhancement
;;
(def include-enhancement? false)

(assert (not (and (not use-refactored?) include-enhancement?)) "Enhancement is only possible with refactored function")
;;
;; See force-error-start-inventory function
;;
(def force-error-starting-inventory? true)
(def change-by 10)

(def update-quality-fn (if use-refactored?
                         refactored/update-quality-new
                         orig/update-quality))

;;
;; "Conjured" at 10, 10 trips the gilded-rose.arrived-ats/under-quality-err check.
;; So perhaps other super power aids will also trip this check in the original code.
;; If so the stated rule that "The quality of an item is never negative" is bogus.
;; To try to trip we are going to alter starting sell-in to be higher and starting
;; quality to be lower for Dexterity and Elixir and run the original function.
;; CONCLUSION -> We are good - the statement "The quality of an item is never negative"
;; , and therefore the corresponding test, can be made to fail exactly same number of
;; times, no matter if use original or refactored function.
;; Perhaps there are checks done on Dexterity and Elixir before they ever come into the
;; inventory in the first place.
;;
(defn force-error-start-inventory [include-enhance?]
  (cond-> [(orig/item "+5 Dexterity Vest" (+ change-by 10) (- change-by 20))
           (orig/item "Aged Brie" 2 0)
           (orig/item "Elixir of the Mongoose" 5 7)
           (orig/item "Sulfuras, Hand Of Ragnaros" 0 80)
           (orig/item "Backstage passes to a TAFKAL80ETC concert" 15 20)]
          include-enhance? (conj (orig/item "Conjured" 10 10))))

(def start-inventory (if force-error-starting-inventory?
                       (force-error-start-inventory include-enhancement?)
                       (orig/start-inventory include-enhancement?)))

;;
;; Need an empty result here
;;
(defn show-num-progressions-need []
  (->> ((u/progress-by update-quality-fn start-inventory) num-progressions)
       last
       (map :sell-in)
       (remove neg?)))

(defn -main []
  (let [f (partial update-quality-fn include-enhancement?)]
    (->> (concat (progressions/test-results start-inventory f num-progressions)
                 (arrived-ats/test-results start-inventory f num-progressions))
         u/pp)))