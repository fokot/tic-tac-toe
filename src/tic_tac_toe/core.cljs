(ns tic-tac-toe.core
  (:require [clojure.string :as string]
            [reagent.core :as r]
            [ajax.core :refer [GET POST]]))

(enable-console-print!)

(def init-recursers [
      {:batch "W1 15" :name "Mary Rose Cook" :image "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/mary_rose_cook_150-bf26e8e8d8a3e268769cc808724c521c8dff11c778bac92f020a64cb50136646.jpg"}
      {:batch "W1 15" :name "Andrew Desharnais" :image "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/andrew_desharnais_150-b58e70c5401570b608e08748b63372ea409cdb0fc8fc02df98d05af8fd33b8a4.jpg"}
      {:batch "F2 15" :name "Salomao Diovanni Montemezzo Becker" :image "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/salomao_diovanni_montemezzo_becker_150-a150572a3231ce4d776f36150eba7bcc4263e233d18f534de915ae2a4db2141c.jpg"}
      {:batch "F2 15" :name "Veronica Hanus" :image "https://d29xw0ra2h4o4u.cloudfront.net/assets/people/veronica_hanus_150-428b56197aa1bf0ef85ed963e311675ef9597cd2ccc6d91474ec47fbc8f3055a.jpg"}
      ])

;; The "database" of your client side UI.
(def app-state
  (r/atom
   {:contacts
    [{:first "Ben" :last "Bitdiddle" :email "benb@mit.edu"}
     {:first "Alyssa" :middle-initial "P" :last "Hacker" :email "aphacker@mit.edu"}
     {:first "Eva" :middle "Lu" :last "Ator" :email "eval@mit.edu"}
     {:first "Louis" :last "Reasoner" :email "prolog@mit.edu"}
     {:first "Cy" :middle-initial "D" :last "Effect" :email "bugs@mit.edu"}
     {:first "Lem" :middle-initial "E" :last "Tweakit" :email "morebugs@mit.edu"}]
    :current-image "http://img08.deviantart.net/aad8/i/2013/176/b/3/fsfdg_by_soldierfurball-d6aom8r.png"
    :recursers init-recursers
    :current-recurser (rand-nth init-recursers)
   }))


(defn handler [response]
  (.log js/console (str response)))

  (GET "https://www.recurse.com/private"
    {:handler handler}
  )

(defn next-rand-rcer [state] (rand-nth (:recursers state)))

(defn update-contacts! [f & args]
  (apply swap! app-state update-in [:contacts] f args)) 

(defn add-contact! [c]
  (update-contacts! conj c))

(defn remove-contact! [c]
  (update-contacts! (fn [cs]
                      (vec (remove #(= % c) cs)))
                    c))

;; The next three fuctions are copy/pasted verbatim from the Om tutorial
(defn middle-name [{:keys [middle middle-initial]}]
  (cond
   middle (str " " middle)
   middle-initial (str " " middle-initial ".")))

(defn display-name [{:keys [first last] :as contact}]
  (str last ", " first (middle-name contact)))

(defn parse-contact [contact-str]
  (let [[first middle last :as parts] (string/split contact-str #"\s+")
        [first last middle] (if (nil? last) [first middle] [first last middle])
        middle (when middle (string/replace middle "." ""))
        c (if middle (count middle) 0)]
    (when (>= (reduce + (map #(if % 1 0) parts)) 2)
      (cond-> {:first first :last last}
        (== c 1) (assoc :middle-initial middle)
        (>= c 2) (assoc :middle middle)))))

;; UI components
(defn contact [c]
  [:li
   [:span (display-name c)]
   [:button {:on-click #(remove-contact! c)} 
    "Delete"]])

(defn new-contact []
  (let [val (r/atom "")]
    (fn []
      [:div
       [:input {:type "text"
                :placeholder "Contact Name"
                :value @val
                :on-change #(reset! val (-> % .-target .-value))}]
       [:button {:on-click #(when-let [c (parse-contact @val)]
                              (add-contact! c)
                              (reset! val ""))}
        "Add"]])))

(defn image []
  [:img {:src (:current-image @app-state)
         :on-click #(swap! app-state 
          (fn [x] (assoc x :current-image "http://mediathek.dfg.de/typo3conf/ext/dreipc_dfg/Resources/Public/Image/DFG_Logo.svg")))
  } 

  ]
  )

(defn current-recurser []
  [:img {:src (:image (:current-recurser @app-state))}

    ]
  )

(defn contact-list []
  [:div
   [:h1 "Contact list"]
   [:ul
    (for [c (:contacts @app-state)]
      [contact c])]
   [new-contact]
   [:div
    [image]]
   [:div
    [current-recurser]]
   ]
   )

;; Render the root component
(defn start []
  (r/render-component 
   [contact-list]
   (.getElementById js/document "app")))

(start)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
