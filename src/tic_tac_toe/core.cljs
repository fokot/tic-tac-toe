(ns tic-tac-toe.core
  (:require (comment [reagent.core :as r] )
            [cljs.reader :as reader]
            ))

(enable-console-print!)

; game state
(defonce default-game-state 
  { :winner nil
    :game-status :in-progress
    :current-player :cross
    :board {}
    })

; vector of keys of all possible solutions
; 3x row
; 3x column
; 2x diagonal
(def solutions 
  (letfn 
    [(solution [x y dx dy] 
      (take 3 (iterate  #(vector (+ dx (get % 0)) (+ dy (get % 1)))  [x y] )))]
    [
      (solution 0 0 0 1)
      (solution 1 0 0 1)
      (solution 2 0 0 1)
      (solution 0 0 1 0)
      (solution 0 1 1 0)
      (solution 0 2 1 0)
      (solution 0 0 1 1)
      (solution 0 2 1 -1)]))

; select the values from map and remove nil values
(defn select-values [m ks] 
        (remove nil? (map m ks)))

; checks if player won the game
; = there is a solution (list positions on the board) for which all 3 values are player's
; #{player} - creates set with player inside. Sets are alo functions returning the element if it's inside
(defn did-player-win? [board player]
  (some #(= 3 (count (filter #{player} (select-values board %) ))) solutions)
)

(defn is-board-full? [board]
  (= 9 (count board))
)

(def board
  {
    [0 0] :cross [0 1] :circle [0 2] :cross
    [1 0] :cross               [1 2] :circle
    [2 0] :cross [2 1] :circle
  }
)

(defn prompt-player [] (
  js/prompt "Make a move" "[0 0]"))


(defn print-game-status [game-state] 
  (print "The game is" 
    (case (:game-status game-state)
      :in-progress "in progress"
      :completed "over")))

(defn print-winner [game-state] 
  (print 
    (case (:winner game-state)
      nil "It's a draw!"
      :cross "Cross won the game!"
      :circle "Circle won the game!"
      )))

(defn test-print-game-status [test-statement game-state new-game-status]
 (do 
  (print test-statement)
  (let [new-game-state (assoc-in game-state [:game-status] new-game-status)] 
    (print-game-status new-game-state))))

(defn run-game [game-state]
  (do
    (print-game-status game-state)
    (if (= :completed (:game-status game-state))
      
      (print-winner game-state)

      (let [turn (prompt-player)
            position (reader/read-string turn)
            current-player (:current-player game-state)
            new-board (assoc-in (:board game-state) [position] current-player)
            current-player-won (did-player-win? new-board current-player)
            is-board-full (is-board-full? new-board)
            new-game-state (-> game-state
              #(if current-player-won (assoc % :winner current-player) %)
              #(if (or current-player-won is-board-full) (assoc % :game-status :completed) %)
              #(update-in % [:current-player] #(if (= :cross %) :circle :cross) )
              #(update-in % [:board] new-board)
            )
           ]

          (run-game new-game-state)
      )
    )
))

    

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