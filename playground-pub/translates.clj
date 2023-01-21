(defn main [db]
  {:show-toast "FIXME"})

(defn FIXME [])

(defn local-event-handler [e]
  (let [action (first e) arg (second e)]
    (if (= action :add)
      (FIXME)
      (if (= action :delete)
        (FIXME)
        []))))

(defn html-to-string [node]
  (let [tag-name (name (first node))
        attrs (filter
               (fn [x]
                 (let [k (first x)]
                   (and (not= :onclick k) (not= :innerText k))))
               (second node))
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
          [:div {:class "columns"}
           [:div {:class "column is-four-fifths"}
            [:code {:innerText x}]]
           [:div {:class "column"}
            [:button {:class "button" :innerText "delete" :onclick [:delete x]}]]])
        items)))

(defn view [db]
  (html-to-string
   [:html {}
    [:head {}
     [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"}]
     [:style {:innerText ".form { display: flex; flex-direction: column; max-width: 600px; margin: auto; padding: 16px }; .list > *:not(:last-child) { margin-bottom: 16px; }"}]]
    [:body {:class "form"}
     [:div {:class "columns"}
      [:input {:class "column is-four-fifths"}]
      [:div {:class "column"}
       [:button {:class "button" :innerText "Add" :onclick [:add {}]}]]]
     (list-view db)]]))

(defn init []
  {:ui {:update (fn [e] (local-event-handler e))
        :view (fn [db] (view db))}
   :schedule {:period 1
              :env {:db {}}
              :main (fn [e] (main e))}})