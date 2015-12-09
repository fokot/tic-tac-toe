(ns tic-tac-toe.core
  (:require [reagent.core :as r] 
  	        [cljs.reader :as reader]
            ))

(enable-console-print!)

(defonce default-game-state 
  { :winner :none
    :game-status :in-progress
    :current-player :cross
    :board {}
    })

(defn solution [x y dx dy] 
	(take 3 (iterate  #(vector (+ dx (get % 0)) (+ dy (get % 1)))  [x y] ))
)

(def solutions [
	(solution 0 0 0 1)
	(solution 1 0 0 1)
	(solution 2 0 0 1)
	(solution 0 0 1 0)
	(solution 0 1 1 0)
	(solution 0 2 1 0)
	(solution 0 0 1 1)
	(solution 0 2 1 -1)
])

(defn select-values [m ks] 
         (reduce #(if-let [v (m %2)] (conj %1 v) %1) [] ks))

(defn countWithFilter [predicate sequence]
	(count (filter predicate sequence)
	)
)

(defn did-player-win? [board player]
	(some #(= 3 %) (map (fn [n] (countWithFilter #(= player %) n)) (map #(select-values board %) solutions)))
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
      :none "It's a draw!"
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
			  new-game-state (assoc-in game-state [:board position] current-player)
			  current-player-won (did-player-win? (:board new-game-state) current-player)
			  ]
			  (do
    		 	(print new-game-state)
    		 	(print "current-player-won " current-player-won)

    		 	(run-game (update-in new-game-state [:current-player] #(if (= :cross %) :circle :cross) ))
    		  )
    	)
    )
  	;	(p)
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