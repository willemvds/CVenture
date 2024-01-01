(ns cventure.character)

(defrecord Char
           [name hp inventory])

(defn new
  [name]
  (Char. name 100 []))

