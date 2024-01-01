(ns cventure.world
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [cventure.character :as character]
            [cventure.location :as location])
  (:gen-class))

;; World invariants
;; 1 - Character must always be at a location

(defn serialize
  [world]
  (pr-str world))

(defn location-reader
  [m]
  (location/map->Location m))

(defn character-reader
  [m]
  (character/map->Char m))

(defn deserialize
  [blob]
  (edn/read-string {:readers {'cventure.location.Location location-reader
                              'cventure.character.Char character-reader}} blob))

(defn new
  "Make a brand new world"
  []
  {:nextEid 0
   :locations {}
   :characters {}
   :positions {}})

(defn advanceEid
  [world]
  (let [nextEid (:nextEid world)]
    (assoc world :nextEid (inc nextEid))))

(defn putChr
  [world char]
  (let [eid (:nextEid world)]
    [(advanceEid (assoc-in world [:characters (:nextEid world)] char)), eid]))

(defn putLoc
  [world loca]
  (let [eid (:nextEid world)]
    [(advanceEid (assoc-in world [:locations (:nextEid world)] loca)), eid]))

(defn updateLocation
  [world eid loca]
  (assoc-in world [:locations eid] loca))

(defn putPosition
  [world who where]
  (assoc-in world [:positions who] where))

(defn putCharacter
  [world char where]
  (let [eid (:nextEid world)]
    [(putPosition
      (advanceEid (assoc-in (updateLocation world where (location/putOcc (get-in world [:locations where]) eid)) [:characters (:nextEid world)] char))
      eid where), eid]))

;; Todo make this code less awful
(defn moveEntity
  [world who where]
  (let [old-location-eid (get-in world [:positions who])
        old-location (get-in world [:locations old-location-eid])
        new-location (get-in world [:locations where])]
    (assoc-in (updateLocation
               (updateLocation world old-location-eid (location/removeOccupant old-location who))
               where (location/putOcc new-location who)) [:positions who] where)))

(defn moveTo
  [world who where])

(defn where
  [world what]
  nil)