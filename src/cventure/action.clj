(ns cventure.action
  (:require [cventure.world :as world]))

;; commands vs actions
;; players perform commands
;; entities perform actions
;; player commands will often result in an entity action but it doesn't have to
;;
;; Some automatic actions like combat might not be performed by a player at all?
;; So all commands require a player to issue (even world commands like npc moving)
;; But not all actions require a command to be issued since some might come from a game loop
;; rather than the world forcing an entity
;;
;; an entity is always connected to just 1 player (like a server/client). only commands issued by
;; that player can be actioned by the entity
;; 
;; all entities can issue actions
;; - npc can move or emote
;; - player character can eat or say
;; - a room can shake or collapse from an earthquake or kick a player because there are too many inside

;; actions raise events
;; for example going north from room A to room B
;; - raises event #1: leave room A
;; - raises event #2: arrives room B
;;
;; for example open door
;; - raises door opens

;; if event is tied to a location only occupants see it?
;; what about a range?
;; some might be global, some might be local, some might have a distance like hearing a monster within a certain number of rooms
(def EVENT_LEAVE :event_leave)
(def EVENT_ENTER :event_enter)

(defn walk
  [world who direction]
  (println "WALKING" who direction (get-in world [:positions who]))
  ;; get location
  (if-let [current-location (get-in world [:positions who])]
    (if-let [to (get-in world [:locations current-location :exits direction])]
      {:world (world/moveEntity world who to)
       :events [{:type EVENT_LEAVE :eid who :location current-location}
                {:type EVENT_ENTER :eid who :location to}]}
      {:world world
       :events []})
    {:world world
     :events []}))

(defn where
  [world entity who]
  (let [location-eid (get-in world [:locations who])]
    (if location-eid
      {:world world
       :events [{:type :spell :eid who :spell-id 1}]}
      nil)))

  ;; check exits
  ;; moveEntity
