(ns robot-finds-kitten.core
  (:require [lanterna.screen :as s])
  (:gen-class))

;;  (def x (atom {:x 1}))
;;  (swap! x assoc :x (inc (get @x :x)))
(def nki-decs ["Test 1"
               "Test 2"
               "Test 3"])

(def nkis (for [x (range 10)]
            [[(rand-int 10) (rand-int 10)] (str (rand-nth "ULAVQPEDY"))]))
(def valid-nki-pos (for [[[x y] _] nkis] [x y]))

(def state (atom {:x 1
                  :y 1
                  :tiles (vec (for [x (range 20) y (range 20)] [x y]))
                  :nkis nkis
                  :decr ""}))
(defn px! [] (get @state :x))
(defn py! [] (get @state :y))

(def scr (s/get-screen))
(def d (s/get-size scr))

(defn player-up
  []
  (swap! state
         assoc
         :y (dec (get @state :y)))) 

(defn player-down []
  (swap! state
         assoc
         :y (inc (get @state :y))))

(defn player-left []
  (swap! state
         assoc
         :x (dec (get @state :x))))

(defn player-right []
  (swap! state
         assoc
         :x (inc (get @state :x))))

(defn walkable? [t] true)

(defn calc-coords
   "return [-1 0] and let you deal with it."
  [x y dir]
  (case dir
    :left       [(dec x) y]
    :right      [(inc x) y]
    :up         [x (dec y)]
    :down       [x (inc y)]
    :up-left    [(dec x) (dec y)]
    :up-right   [(inc x) (dec y)]
    :down-left  [(dec x) (inc y)]
    :down-right [(inc x) (inc y)]))

(defn handle-input []
  (let [k (s/get-key-blocking scr)]
    (case k
      \q [:quit nil]
      :down  [:move :down]
      :up    [:move   :up]
      :left  [:move :left]
      :right [:move :right]
      \space [:dbg nil]
      [nil nil])))

(defmulti handle-command
  (fn [command _] command))

(defmethod handle-command nil [_ _]
  nil)

(defmethod handle-command :dbg [_ _]
  "Player has pressed Space"
  (if (and (= "" (get @state :decr)) (some #{[(get @state :x) (get @state :y)]} valid-nki-pos))
    (swap! state assoc :decr (rand-nth nki-decs))
    (swap! state assoc :decr (get @state :decr))))

(defmethod handle-command :move [_ dir]
  "Move the player in the given direction."
  (let [[x y] (calc-coords (px!) (py!) dir)]
    (swap! state assoc :decr "")
     (case dir
       :up (player-up)
       :down (player-down)
       :left (player-left)
       :right (player-right)
       "default")))

(defn render []
  (s/clear scr)
  (doseq [[x y] (get @state :tiles)]
    (s/put-string scr x y "."))
  (doseq [[[x y] ch] (get @state :nkis)]
    (s/put-string scr x y ch))
  (s/put-string scr 10 10 ".")
  (s/put-string scr (px!) (py!) "@")
  (s/put-string scr 0 20 "--------------------")
  (s/put-string scr 0 21 (get @state :decr))
  (s/redraw scr))

(defn game-loop []
  (render)
  (let [[command data] (handle-input)]
    (if (= command :quit)
      (s/stop scr)
      (do
        (handle-command command data)
        (recur)))))

(defn -main
  [& args]
  (s/start scr)
  (game-loop))
