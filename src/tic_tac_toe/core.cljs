(ns tic-tac-toe.core
  (:require [reagent.core :as r]))

(enable-console-print!)

(def new-game-state
  { 
    :winner nil
    :game-status :in-progress
    :current-player :cross
    :board {}
    :solutions nil
    })

; game state used by reagent
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
  (seq (apply concat (filter #(= 3 (count (filter #{player} (select-values board %) ))) solutions)))
)

(def all-board-postions 
  (for [x [0 1 2]
        y [0 1 2]]
       [x y]))

(defn computer-move [board]
  (rand-nth (remove (set (keys board)) all-board-postions)))

(defn do-move 
  "Updates board with player move and checks if player did win.
   Returns game-state with new board and game-status set."
  [game-state position player]
    (let [new-game-state (assoc-in game-state [:board position] player)
          player-won (did-player-win? (:board new-game-state) player)]
          (if player-won
            (-> new-game-state
                (assoc :game-status :completed)
                (assoc :winner player)
                (assoc :solutions player-won)
            )
            new-game-state)))

(defn is-board-full
  "Returns game-state with status set to :completed if the board is full"
  [game-state]
  (if (= 9 (count (:board game-state)))
    (assoc game-state :game-status :completed)
    game-state
    )
)

(defn player-clicked
  "Do everything after player clicked:
   * players move
   * check if he won
   * check if board is full
   * computer move
   * check if it won
   * check if board is full
  "
  [game-state position]
      (reduce #(if (= :completed (:game-status %1)) %1 (%2 %1)) 
        [ (do-move game-state position :cross) ;player's turn
          is-board-full ;check if board is full
          (fn [x](do-move x (computer-move (:board x)) :circle)) ;computer's move
          is-board-full ;check if board is full
        ]))

(defn svg-ref 
  "xlink:href will be supported as xlinkHref in react 14, and this project still used react 13"
  [id]
  [:svg
    {:dangerouslySetInnerHTML
      {:__html (str "<use xlink:href=\"#" id "\"></use>")}}])

(defn cell-component [key] 
  (let [value (get (:board @game-state) key)
        solution? (some #{key} (:solutions @game-state))
        can-play (and (nil? value) (= :in-progress (:game-status @game-state)))
        ]
    [:div.cell
      (if can-play
        {:on-click #(swap! game-state player-clicked key)}
        (when solution? {:class "solution"}))
      (case value
        nil ""
        :cross [svg-ref "cross"]
        :circle [svg-ref "circle"])]))

(defn winner-annoucement []
  (when (= :completed (:game-status @game-state))
    [:div.announcement (case (:winner @game-state)
           :cross "You won!!!"
           :circle "Computer won!!!"
           nil "Cat's game!!!")]))

(defn board-component [] 
  [:div {:id "board"}
    (for [x [0 1 2]]
        (for [y [0 1 2]]
            ^{:key (str x y)}[cell-component [x y]]))
    [winner-annoucement]])

(defn restart-game-component [] 
  [:button
    {:on-click #(reset! game-state new-game-state)}
    "New game"])

(defn game-component []   
  [:div {:id "game"}
    [:h1 "Ultimate tic-tac-toe"]
    [board-component]
    [restart-game-component]])

;; Render the root component
(defn start []
  (r/render-component 
   [game-component]
   (.getElementById js/document "app")))

(start)