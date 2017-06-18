(ns gilded-rose.refactored
  (:require [gilded-rose.core :as orig]))

(defn update-quality-new [items]
  (map
    (fn[item] (cond
                (and (< (:sell-in item) 0) (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))
                (merge item {:quality 0})
                (or (= (:name item) "Aged Brie") (= (:name item) "Backstage passes to a TAFKAL80ETC concert"))
                (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 5) (< (:sell-in item) 10))
                  (merge item {:quality (inc (inc (:quality item)))})
                  (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 0) (< (:sell-in item) 5))
                    (merge item {:quality (inc (inc (inc (:quality item))))})
                    (if (< (:quality item) 50)
                      (merge item {:quality (inc (:quality item))})
                      item)))
                (< (:sell-in item) 0)
                (if (= "Backstage passes to a TAFKAL80ETC concert" (:name item))
                  (merge item {:quality 0})
                  (if (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
                    (merge item {:quality (- (:quality item) 2)})
                    item))
                (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
                (merge item {:quality (dec (:quality item))})
                :else item))
    (map (fn [item]
           (if (not= "Sulfuras, Hand of Ragnaros" (:name item))
             (merge item {:sell-in (dec (:sell-in item))})
             item))
         items)))

;;
;; Switch on and off, depending whether want to to use the original or my refactored update-quality function
;;
(def use-refactored? true)

(def update-quality-fn (if use-refactored?
                         update-quality-new
                         orig/update-quality))
