(comment

  (=
   [{} {:items ["xxxx"]} {:items ["sss" "xxxx"]} {:items ["xxxx"]} {:items []}]
   (->>
    [{} {:action :add :input "xxxx"} {:action :add :input "sss"} {:action :delete :id "sss"} {:action :delete :id "xxxx"}]
    (reductions (fn [a e] (->> (local-event-handler e) (make-storage a))) {}) (rest)))

  (comment))

(defn main [db]
  {:show-toast "FIXME"})

(defn make-storage [db e]
  (if-some [e (:update-db e)]
    (if-some [arg (:add e)]
      (assoc db :items (cons arg (or (:items db) [])))
      (if-some [arg (:delete e)]
        (assoc db :items (filter (fn [x] (not= x arg)) (or (:items db) [])))
        db))
    db))

(defn local-event-handler [e]
  (case (:action e)
    :add {:update-db {:add (:input e)}}
    :delete {:update-db {:delete (:id e)}}
    {}))

(defn html-to-string [node]
  (let [tag-name (name (first node))
        attrs (filter
               (fn [x] (let [k (first x)] (not= :innerText k)))
               (vec (second node)))
        attrs-str (reduce (fn [a x] (str a " " (name (first x)) "=\"" (second x) "\"")) "" attrs)
        children (rest (rest node))
        inner-text-attr (:innerText (second node))
        inner-text (reduce (fn [a x] (str a (html-to-string x))) "" children)
        inner-text-result (if (some? inner-text-attr) inner-text-attr inner-text)]
    (str "<" tag-name attrs-str ">" inner-text-result "</" tag-name ">")))

(defn list-view [items]
  (concat
   [:div {}]
   (map (fn [x]
          [:div {:class "level"}
           [:div {:class "card is-flex-grow-1"}
            [:div {:class "card-content"}
             [:div {:class "content" :innerText x}]]
            [:div {:class "card-footer"}
             [:button {:class "button is-danger"
                       :innerText "Delete"
                       :hx-post ""
                       :hx-target "body"
                       :hx-vals (str "js:{action:'delete',id:'" x "'}")}]]]])
        items)))

(defn view [db]
  (->>
   [:html {:lang "en"}
    [:head {}
     [:meta {:charset "UTF-8"}]
     [:title {:innerText "Words"}]
     [:meta {:name "description" :content "Remember words application"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"}]
     [:link {:rel "stylesheet" :href "https://unpkg.com/bulma-prefers-dark@0.1.0-beta.1"}]
     [:script {:src "https://unpkg.com/htmx.org@1.8.5" :defer "" :crossorigin "anonymous" :integrity "sha384-7aHh9lqPYGYZ7sTHvzP1t3BAfLhYSTy9ArHdP3Xsr9/3TlGurYgcPBoFmXX2TX/w"}]]
    [:body {}
     [:div {:class "container"}
      [:section {:class "section pt-4"}
       [:div {:class "field"}
        [:label {:class "label" :innerText "New word"}]
        [:div {:class "control"}
         [:input {:class "input" :name "input" :placeholder "<word> - <translation>"}]]]
       [:button {:class "button is-primary"
                 :innerText "Add"
                 :hx-post ""
                 :hx-target "body"
                 :hx-vals "js:{action:'add'}"
                 :hx-include "[name='input']"}]]
      [:section {:class "section pt-0"}
       [:h1 {:class "title" :innerText "Words"}]
       (list-view (or (:items db) []))]]]]
   (html-to-string)
   (str "<!DOCTYPE html>")))

(defn init []
  {:ui {:update (fn [e] (local-event-handler e))
        :restore-state (fn [db e] (make-storage db e))
        :view (fn [db] (view db))}
   :schedule {:period 1
              :env {:db {}}
              :main (fn [e] (main e))}})
