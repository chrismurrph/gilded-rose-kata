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

(def calibrated-appreciating #{(-> items :backstage :name)})
(def simple-appreciating #{(-> items :brie :name)})
(def depreciating #{(-> items :dexterity :name) (-> items :elixir :name)})

(defn name-of [kw]
  (-> items kw :name))

(def inc-quality (u/inc-kw :quality))
(def dec-quality (u/dec-kw :quality))
(def zero-quality (u/zero-kw :quality))
(def dec-sell-in (u/dec-kw :sell-in))

(defn legendary? [{:keys [name]}]
  (= (name-of :sulfuras) name))

(defn transition-quality [itm]
  (if (legendary? itm)
    itm
    (let [item (dec-sell-in itm 1)]
      (cond
        (calibrated-appreciating (:name item))
        (cond
          (and (>= (:sell-in item) 5) (< (:sell-in item) 10))
          (inc-quality item 2)

          (and (>= (:sell-in item) 0) (< (:sell-in item) 5))
          (inc-quality item 3)

          (< (:sell-in item) 0)
          (zero-quality item)

          (< (:quality item) 50)
          (inc-quality item 1))

        (simple-appreciating (:name item))
        (cond
          (< (:quality item) 50)
          (inc-quality item 1))

        (< (:sell-in item) 0)
        (cond
          (= (name-of :backstage) (:name item))
          (zero-quality item)

          (depreciating (:name item))
          (dec-quality item 2)

          :else item)

        (depreciating (:name item))
        (dec-quality item 1)

        :else item))))

(defn update-quality-new [items]
  (map transition-quality items))

;;
;; Switch on and off, depending whether want to to use the original or my refactored update-quality function
;;
(def use-refactored? true)

(def update-quality-fn (if use-refactored?
                         update-quality-new
                         orig/update-quality))
