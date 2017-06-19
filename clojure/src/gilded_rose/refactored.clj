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

(defn past-use-by? [{:keys [sell-in]}]
  (neg? sell-in))

(defn legendary? [{:keys [name]}]
  (= (name-of :sulfuras) name))

(defn cheese? [item]
  (simple-appreciating (:name item)))

(defn backstage-pass? [{:keys [name]}]
  (calibrated-appreciating name))

(defn superpowers-aid? [{:keys [name]}]
  (depreciating name))

(defn sell-in-is-between? [lower upper {:keys [sell-in]}]
  (and (>= sell-in lower) (< sell-in upper)))

(defn calibrate-appreciation [item]
  (cond
    (sell-in-is-between? 5 10 item)
    (inc-quality item 2)

    (sell-in-is-between? 0 5 item)
    (inc-quality item 3)

    (< (:quality item) 50)
    (inc-quality item 1)))

(defn transition-quality [itm]
  (if (legendary? itm)
    itm
    (let [item (dec-sell-in itm 1)]
      (cond
        (backstage-pass? item)
        (if (past-use-by? item)
          (zero-quality item)
          (calibrate-appreciation item))

        (cheese? item)
        (cond
          (< (:quality item) 50)
          (inc-quality item 1))

        (superpowers-aid? item)
        (let [dec-quality-by (if (past-use-by? item) 2 1)]
          (dec-quality item dec-quality-by))

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
