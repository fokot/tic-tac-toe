(ns tic-tac-toe.core
  (:require [reagent.core :as r] 
            [cljs.reader :as reader]
            ))

(enable-console-print!)

(def new-game-state
  { 
    :winner nil
    :game-status :in-progress
    :current-player :cross
    :board {}
    })

; game state
(defonce game-state
  (r/atom new-game-state))

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

(def all-board-postions 
  (for [x [0 1 2]
        y [0 1 2]]
       [x y]))

(defn circle-move [board]
  (rand-nth (remove (set (keys board)) all-board-postions)))

(defn run-game [cross-turn]
  (do
    (println "som tu")
    (print-game-status @game-state)
    (println @game-state)
    (if (= :completed (:game-status @game-state))
      
      (print-winner @game-state)

      (let [new-board (assoc (:board @game-state) cross-turn :cross)
           ]
           (if (did-player-win? new-board :cross)
            (do 
              (println "cross won")
              (swap! game-state #(-> % (assoc :board new-board)
                                         (assoc :game-status :completed)

                  ))

              )
            )
            (if (is-board-full? new-board)
              (do 
                (println "board is full")
                (swap! game-state #(-> % (assoc :board new-board)
                                         (assoc :game-status :completed)

                  ))
              )
              (let [new-new-board (assoc new-board (circle-move new-board) :circle)
                    circle-won (did-player-win? new-new-board :circle)
                   ]
                   (do 
                    (swap! game-state assoc :board new-new-board))
                    (if circle-won 
                      (println "cross won")
                      (swap! game-state #(-> % (assoc :board new-board)
                                         (assoc :game-status :completed)

                      ))
                      (if (is-board-full? new-new-board)
                        (println "board is full"))
                    )
                   )
            )
      )
    )
))

(defn cell-component [key] 
  (let [value (get (:board @game-state) key)]
    ^{:key key}[:div.max-size 
      (if (nil? value)
        {:on-click #(run-game key)})
      (case value
        nil ""
        :cross "\u274C"
        :circle "\u2B55"
      )]))



(defn board-component [] 
  [:table
    [:tbody
    (for [x [0 1 2]]
      [:tr
        (for [y [0 1 2]]
          [:td
            [cell-component [x y]]
          ]
        )
      ]
    )
    ]])

(defn start-new-game-component [] 
  [:button {:on-click #(reset! game-state new-game-state)}])

(defn game-component [] [:div
  [board-component]
  [start-new-game-component]])

;; Render the root component
(defn start []
  (r/render-component 
   [game-component]
   (.getElementById js/document "app")))

(start)