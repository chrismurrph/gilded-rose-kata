(ns gilded-rose.refactored
  (:require [gilded-rose.core :as orig]
            [gilded-rose.utils :as u]))

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

(def appreciating #{(-> items :backstage :name) (-> items :brie :name)})
(def depreciating #{(-> items :dexterity :name) (-> items :elixir :name)})

(defn name-of [kw]
  (-> items kw :name))

(def inc-quality (u/inc-kw :quality))
(def dec-quality (u/dec-kw :quality))
(def zero-quality (u/zero-kw :quality))
(def dec-sell-in (u/dec-kw :sell-in))

(defn update-quality-new [items]
  (map
    (fn[item] (cond
                (and (< (:sell-in item) 0) (= (:name item) (name-of :backstage)))
                (zero-quality item)

                (appreciating (:name item))
                (if (and (= (:name item) (name-of :backstage)) (>= (:sell-in item) 5) (< (:sell-in item) 10))
                  (inc-quality item 2)
                  (if (and (= (:name item) (name-of :backstage)) (>= (:sell-in item) 0) (< (:sell-in item) 5))
                    (inc-quality item 3)
                    (if (< (:quality item) 50)
                      (inc-quality item 1)
                      item)))

                (< (:sell-in item) 0)
                (if (= (name-of :backstage) (:name item))
                  (zero-quality item)
                  (if (depreciating (:name item))
                    (dec-quality item 2)
                    item))

                (depreciating (:name item))
                (dec-quality item 1)

                :else item))
    (map (fn [item]
           (if (not= (name-of :sulfuras) (:name item))
             (dec-sell-in item 1)
             item))
         items)))

;;
;; Switch on and off, depending whether want to to use the original or my refactored update-quality function
;;
(def use-refactored? true)

(def update-quality-fn (if use-refactored?
                         update-quality-new
                         orig/update-quality))
