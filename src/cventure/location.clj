(ns cventure.location)

(def DIRECTION_NORTH 0)
(def DIRECTION_NORTH_EAST 45)
(def DIRECTION_EAST 90)
(def DIRECTION_SOUTH_EAST 135)
(def DIRECTION_SOUTH 180)
(def DIRECTION_SOUTH_WEST 225)
(def DIRECTION_WEST 270)
(def DIRECTION_NORTH_WEST 315)

(def DIRECTIONS {DIRECTION_NORTH "North"
                 DIRECTION_NORTH_EAST "NorthEast"
                 DIRECTION_EAST "East"
                 DIRECTION_SOUTH_EAST "SouthEast"
                 DIRECTION_SOUTH "South"
                 DIRECTION_SOUTH_WEST "SouthWest"
                 DIRECTION_WEST "West"
                 DIRECTION_NORTH_WEST "NorthWest"})

(defrecord Location
           [name description exits occupants])

(defn new
  [name description]
  (Location. name description {} {}))

(defn putOcc
  [location eid]
  (assoc-in location [:occupants eid] (. System (nanoTime))))

(defn putExit
  [location direction eid]
  (assoc-in location [:exits direction] eid))

(defn removeOccupant
  [location occupant-eid]
  (if location
    (update-in location [:occupants] dissoc occupant-eid)
    location))