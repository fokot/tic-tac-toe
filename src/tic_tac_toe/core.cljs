(ns tic-tac-toe.core
  (:require [reagent.core :as r] 
            [cljs.pprint :refer [pprint]]))

(enable-console-print!)

(defonce default-game-state 
  { :winner :none
    :game-status :in-progress
    :current-player :cross
    :board [[:none :none :none] 
            [:none :none :none] 
            [:none :none :none]]})

(defn prompt-player [] (
  js/prompt "Make a move" "{:player :cross :i 0 :j 0}"))

;(let [answer (prompt-player)] (print answer))

(defn print-game-status [game-state] 
  (print "The game is" 
    (case (:game-status game-state)
      :in-progress "in progress"
      :completed "over")))

(defn test-print-game-status [test-statement game-state new-game-status]
 (do 
  (print test-statement)
  (let [new-game-state (assoc-in game-state [:game-status] new-game-status)] 
    (print-game-status new-game-state))))

(defn run-game [game-state]
  (do
    (print-game-status)
))
  (if (= :in-progress (:game-status game-state))
    (do
      (print-game-status game-state)

)))

(run-game default-game-state)

;(test-print-game-status "game is in progress" default-game-state :in-progress)
;(test-print-game-status "game is completed" default-game-state :completed)




; (run-game [game-state]
;     - print game status
;     - if game completed -> print winner
;     - if game status in progress ->    
;       - ask for turn
;       - update board
;       - calculate new game-status
;       - toggle player
;       - run-game new-game-status