(ns gilded-rose.refactored
  (:require [gilded-rose.core :as orig]
            [gilded-rose.utils :as u]))

(def kw->name {:backstage "Backstage passes to a TAFKAL80ETC concert"
               :dexterity "+5 Dexterity Vest"
               :elixir    "Elixir of the Mongoose"
               :brie      "Aged Brie"
               :sulfuras  "Sulfuras, Hand of Ragnaros"
               ;; Can always keep as won't be asked for if not included
               :conjured  "Conjured"})

(defn kw->item [kw]
  (first (filter #(= (:name %) (kw kw->name)) (orig/start-inventory true))))

(def items {:backstage (kw->item :backstage)
            :dexterity (kw->item :dexterity)
            :elixir    (kw->item :elixir)
            :brie      (kw->item :brie)
            :sulfuras  (kw->item :sulfuras)
            :conjured  (kw->item :conjured)})

(def calibrated-appreciating #{(-> items :backstage :name)})
(def simple-appreciating #{(-> items :brie :name)})
(def depreciating #{(-> items :dexterity :name) (-> items :elixir :name) (-> items :conjured :name)})
(def double-depreciating #{(-> items :conjured :name)})

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

(defn conjuring-aid? [{:keys [name]}]
  (double-depreciating name))

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

(defn transition-quality [include-enhance? itm]
  (if (legendary? itm)
    itm
    (let [item (dec-sell-in itm 1)]
      (cond
        (backstage-pass? item)
        (if (past-use-by? item)
          (zero-quality item)
          (calibrate-appreciation item))

        (cheese? item)
        (if (< (:quality item) 50)
          (inc-quality item 1)
          item)

        (superpowers-aid? item)
        (let [dec-quality-by (if (past-use-by? item) 2 1)
              maybe-double-dec-by (cond-> dec-quality-by
                                          (and include-enhance? (conjuring-aid? item)) (* 2))]
          (dec-quality item maybe-double-dec-by))

        :else item))))

(defn update-quality-new [include-enhance? items]
  (map (partial transition-quality include-enhance?) items))