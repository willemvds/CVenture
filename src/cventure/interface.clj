(ns cventure.interface
  (:require
   [cventure.location :as location]
   [cventure.action :as action]))

(def NEWLINE "\r\n")
(def ANSI_BLACK "\u001b[30m")
(def ANSI_RED "\u001b[31m")
(def ANSI_GREEN "\u001b[32m")
(def ANSI_YELLOW "\u001b[33m")
(def ANSI_BLUE "\u001b[34m")
(def ANSI_MAGENTA "\u001b[35m")
(def ANSI_CYAN "\u001b[36m")
(def ANSI_WHITE "\u001b[37m")
(def ANSI_RESET "\u001b[0m")

(defn directionString
  [direction]
  (if-let [ds (get location/DIRECTIONS direction)]
    ds
    "????"))

(defn entityText [eId entity]
  (str "Entity#" eId "(" (.name entity) ")"))

(defn locationExitsText [world exits]
  (loop [remaining-exits exits
         text-so-far ""]
    (if (empty? remaining-exits)
      text-so-far
      (let [[direction target] (first remaining-exits)]
        (recur (dissoc remaining-exits direction) (str text-so-far "    " (directionString direction) " - "
                                                       (.name (get-in world [:locations target]))))))))

(defn locationOccupantsText [world occupants]
  (println occupants)
  (loop [remaining-occupants occupants
         text-so-far ""]
    (if (empty? remaining-occupants)
      text-so-far
      (let [eId (first (first remaining-occupants))]
        (recur (dissoc remaining-occupants eId) (str text-so-far NEWLINE (str (entityText eId (get-in world [:characters eId])) " is here.")))))))

(defn locationText
  [world locationEid]
  (let [location (get-in world [:locations locationEid])]
    (if location
      (str
       ANSI_YELLOW (.name location) NEWLINE
       ANSI_RESET (.description location) NEWLINE
       ANSI_CYAN "Exits:" ANSI_RESET NEWLINE
       (locationExitsText world (.exits location))
       (locationOccupantsText world (.occupants location)))
      "")))

(defn cmdLook
  [world eid]
  (let [location-eid (get-in world [:positions eid])
        location (get-in world [:locations location-eid])]
    (println (locationText world location-eid))))

(defn cmdNorth
  [world eid]
  ;; get the location of the entity
  ;; check if direction north is on the exits for the location
  ;; moveEntity
  )

(defn present-event
  [world event to]
  (cond
    (and (= (:type event) :event_enter) (= to (:eid event))) (println (locationText world (:location event)))
    (and (= (:type event) :event_leave) (not= to (:eid event))) (println (entityText (:eid event) (get-in world [:characters (:eid event)])) " leaves.")))
