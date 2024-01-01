(ns cventure.core
  (:require
   [cventure.wrap :as wrap]
   [cventure.world :as world]
   [cventure.location :as location]
   [cventure.character :as character]
   [cventure.interface :as interface]
   [cventure.game :as game]
   [cventure.action :as action]
   [aleph.tcp :as tcp]
   [manifold.stream :as s]
   [manifold.deferred :as d]
   [cventure.net :as net])
  ;;  [clojure.data.json :as json])
  (:gen-class))

;; convenience world building in REPL
(def wip (atom (world/new)))

(defn newLocation
  [name desc]
  (let [loc (location/new name desc)]
    (swap! wip (fn [wip] (first (world/putLoc wip loc))))))

(defn newCharacter
  [name]
  (let [char (character/new name)]
    (swap! wip (fn [wip] (first (world/putChr wip char))))))

(defn save
  [world]
  (spit "universe" (world/serialize world)))

(defn load
  []
  (swap! wip (fn [wip] (world/deserialize (slurp "universe")))))

(def g (atom (game/new @wip)))

(defn handler [game-atom reader writer]
  (.append writer "What is your name?")
  (.flush writer)
  (let [line (.readLine reader)]
    (game/join game-atom 1))

  (loop [line (.readLine reader)]
    (println "got" line)
    (let [done? (cond (= line "quit") (do
                                        (println "BYE")
                                        (.close writer)
                                        (.close reader)
                                        true)
                      (or (= line "look") (= line "l")) (do
                                                          (let [pid 1
                                                                world (:world @game-atom)
                                                                location-eid (get-in world [:positions pid])
                                                                view (interface/locationText world location-eid)]
                                                            (println "location for pid 1 is" location-eid)
                                                            (.append writer (wrap/wrap view 120))
                                                            (.flush writer))
                                                          false)
                      :else (let [act (game/process-player-input game-atom 1 line)]
                              (if act
                                (do
                                  (.append writer (str "action queued for next game tick: " act))
                                  (.flush writer))
                                (do
                                  (.append writer "Eh, wha?\n")
                                  (.flush writer)))
                              false))]
      (if done?
        nil
        (recur (.readLine reader))))))

(defn -main
  "Enter the CVenture"
  [& args]
  ;; (println (map #((roomText %)) rooms))
  (load)

  (let [G-atom (atom (game/new @wip))]
    (future (net/start (net/tcp-server
                        :port    13001
                        :handler (net/wrap-io (fn [reader writer] (handler G-atom reader writer))))))

    (loop []
      (print "tick ")
      (flush)
      (game/tick G-atom)
      (Thread/sleep 1000)
      (recur)))

  (game/tick g)
  (game/queue-action g 1 (fn [world entity] (action/walk world entity location/DIRECTION_NORTH)))
  (game/queue-action g 2 (fn [world entity] (action/walk world entity location/DIRECTION_NORTH)))
  (game/tick g))

