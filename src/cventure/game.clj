(ns cventure.game
  (:require
   [cventure.world :as world]
   [cventure.action :as action]
   [cventure.location :as location]))

;; look
;; find the entity performing the command (all entities can perform commands, including rooms)
;; (for now we'll just do characters being able to run commands)
;;
;; Example command:
;; #1 - character look
;; * Find the location of the character
;; * print locationText of that location

;; a game has players and a world
(defrecord Game
           [world players queued-actions events-buffer])

(defn new
  [world]
  (Game.
   world ;; initial game world
   {} ;; players
   {} ;; queued-actions
   [])) ;; events-buffer

(defn parseInput
  [line]
  (if (or (= line "n") (= line "north"))
    '(action/walk 1 1 location/DIRECTION_NORTH)
    nil))

(defn queue-action
  [game-atom entity action-fn]
  (swap! game-atom (fn [game] (let [queued-action (get-in game [:queued-actions entity])]
                                (if queued-action
                                  game
                                  (assoc-in game [:queued-actions entity] action-fn))))))

;; Update the game atom and give the caller the events that happened
;; so we can "draw" them which means sending network packets prolly.
(defn tick
  [game-atom]
  (swap! game-atom (fn [game]
                     (assoc (reduce
                             (fn [game [entity action-fn]]
                               (let [{new-world :world events :events} (action-fn (:world game) entity)]
                                 (assoc (assoc game :events-buffer (into (:events-buffer game) events)) :world new-world)))
                             game (:queued-actions game))
                            :queued-actions {}))))
(defn parse
  [game pid cmd]
  (let [char 1] ;; we don't have a player database yet, just use Character#1(Awcrap) we know exists in the world
    (cond
      (or (= cmd "n") (= cmd "north")) (fn [world who] (action/walk world who location/DIRECTION_NORTH))
      (or (= cmd "e") (= cmd "east")) (fn [world who] (action/walk world who location/DIRECTION_EAST))
      (or (= cmd "s") (= cmd "south")) (fn [world who] (action/walk world who location/DIRECTION_SOUTH))
      (or (= cmd "w") (= cmd "west")) (fn [world who] (action/walk world who location/DIRECTION_WEST)))))

(defn ask-for-input
  [game-atom entity]
  (print "> ")
  (flush)
  (let [cmd (read-line)
        action-fn (parse @game-atom entity cmd)]
    (if action-fn
      (queue-action game-atom entity action-fn)
      (println "Eh, wha?"))))

(defn input-loop
  [game-atom entity]
  (loop []
    (print "> ")
    (flush)
    (let [cmd (read-line)]
      (if (= cmd "quit")
        nil
        (let [action-fn (parse @game-atom entity cmd)]
          (if action-fn
            (do
              (queue-action game-atom entity action-fn)
              (println "action queued" action-fn))
            (println "Eh, wha?"))
          (recur))))))
;; (defn join
;;   [game pid player character]
;;   (assoc-in (assoc game :world (first (world/putCharacter (.world game) character 0))) [:players pid] player)
;;   (assoc-in game [:players pid] player))

(defn join
  [game-atom entity]
  (swap! game-atom (fn [game]
                     (assoc-in game [:players 1] entity))))

(defn process-player-input
  [game-atom pid input]
  (if-let [action-fn (parse @game-atom pid input)]
    (queue-action game-atom (get-in @game-atom [:players pid]) action-fn)))