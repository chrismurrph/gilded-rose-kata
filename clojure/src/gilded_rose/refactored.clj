(ns gilded-rose.refactored
  (:require [gilded-rose.core :as orig]))

(def kw->name {:backstage "Backstage passes to a TAFKAL80ETC concert"
               :dexterity "+5 Dexterity Vest"
               :elixir    "Elixir of the Mongoose"
               :brie      "Aged Brie"
               :sulfuras  "Sulfuras, Hand of Ragnaros"})

(defn kw->item [kw]
  (first (filter #(= (:name %) (kw kw->name)) orig/start-inventory)))

(def items {:backstage (kw->item :backstage)
            :dexterity (kw->item :dexterity)
            :elixir    (kw->item :elixir)
            :brie      (kw->item :brie)
            :sulfuras  (kw->item :sulfuras)})

(defn name-of [kw]
  (-> items kw :name))

(defn update-quality-new [items]
  (map
    (fn[item] (cond
                (and (< (:sell-in item) 0) (= (:name item) (name-of :backstage)))
                (merge item {:quality 0})
                (or (= (:name item) (name-of :brie)) (= (:name item) (name-of :backstage)))
                (if (and (= (:name item) (name-of :backstage)) (>= (:sell-in item) 5) (< (:sell-in item) 10))
                  (merge item {:quality (inc (inc (:quality item)))})
                  (if (and (= (:name item) (name-of :backstage)) (>= (:sell-in item) 0) (< (:sell-in item) 5))
                    (merge item {:quality (inc (inc (inc (:quality item))))})
                    (if (< (:quality item) 50)
                      (merge item {:quality (inc (:quality item))})
                      item)))
                (< (:sell-in item) 0)
                (if (= (name-of :backstage) (:name item))
                  (merge item {:quality 0})
                  (if (or (= (name-of :dexterity) (:name item)) (= (name-of :elixir) (:name item)))
                    (merge item {:quality (- (:quality item) 2)})
                    item))
                (or (= (name-of :dexterity) (:name item)) (= (name-of :elixir) (:name item)))
                (merge item {:quality (dec (:quality item))})
                :else item))
    (map (fn [item]
           (if (not= (name-of :sulfuras) (:name item))
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
