(defn main [db]
  {:show-toast "FIXME"})

;; (.show
;;  (android.widget.Toast/makeText (:application cofx) arg 0))
;; (println "FIXME :: TEST")

(defn make-storage [cofx e]
  (let [db (:db cofx)]
    (if-some [e (:update-db e)]
      (if-some [arg (:add e)]
        (assoc db :items (cons arg (or (:items db) [])))
        (if-some [arg (:delete e)]
          (assoc db :items (filter (fn [x] (not= x arg)) (or (:items db) [])))
          db))
      db)))

(defn local-event-handler [e]
  (case (:action e)
    :add {:update-db {:add (:input e)}}
    :delete {:update-db {:delete (:id e)}}
    {}))

(defn html-to-string [node]
  (if (vector? node)
    (let [tag-name (name (first node))
          attrs (vec (second node))
          attrs-str (reduce (fn [a x] (str a " " (name (first x)) "=\"" (second x) "\"")) "" attrs)
          children (rest (rest node))
          inner-text-attr (:innerText (second node))
          inner-text (reduce (fn [a x] (str a (html-to-string x))) "" children)
          inner-text-result (if (some? inner-text-attr) inner-text-attr inner-text)]
      (str "<" tag-name attrs-str ">" inner-text-result "</" tag-name ">"))
    (str node)))

(defn list-view [items]
  (let [children
        (map (fn [x]
               [:article {}
                [:h3 {} x]
                [:footer {}
                 [:div {:role "button" :hx-post "" :hx-target "body"
                        :hx-vals (str "js:{action:'delete',id:'" x "'}")}
                  "Delete"]]])
             items)]
    (concat [:div {}] children)))

(defn view [db]
  (->>
   [:html {:lang "en" :data-theme "dark"}
    [:head {}
     [:meta {:charset "UTF-8"}]
     [:title {} "Words"]
     [:meta {:name "description" :content "Remember words application"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href "https://unpkg.com/@picocss/pico@1.5.7/css/pico.min.css"}]
     [:script {:src "https://unpkg.com/htmx.org@1.8.5" :defer "" :crossorigin "anonymous" :integrity "sha384-7aHh9lqPYGYZ7sTHvzP1t3BAfLhYSTy9ArHdP3Xsr9/3TlGurYgcPBoFmXX2TX/w"}]]
    [:body {}
     [:main {:class "container"}
      [:label {} "New word"]
      [:input {:name "input" :placeholder "<word> - <translation>"}]
      [:button {:hx-post "" :hx-target "body" :hx-vals "js:{action:'add'}"
                :hx-include "[name='input']"}
       "Add"]
      (list-view (or (:items db) []))]]]
   (html-to-string)
   (str "<!DOCTYPE html>")))

(defn init []
  {:ui {:update (fn [e] (local-event-handler e))
        :restore-state (fn [cofx e] (make-storage cofx e))
        :view (fn [db] (view db))}
   :schedule {:period 1
              :env {:db {}}
              :main (fn [e] (main e))}})
