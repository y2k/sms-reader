(defn main [db]
  {:show-toast "FIXME"})

(defn local-event-handler [e]
  (if (= (first e) :form)
    (let [arg (second e)
          action-ext (get arg :action)]
      (if (some? action-ext)
        (let [params (.split (get arg :action) ":")
              action (first params)]
          (if (some? arg)
            (if (= action :add)
              {:insert-db {:add (get arg :input)}}
              (if (= action :delete)
                {:insert-db {:delete (second params)}}
                {:error params}))
            {:error2 params}))
        {:error3 "FIXME"}))
    {:error4 "FIXME"}))

(defn html-to-string [node]
  (let [tag-name (name (first node))
        attrs (filter
               (fn [x] (let [k (first x)] (not= :innerText k)))
               (vec (second node)))
        attrs-str (reduce (fn [a x] (str a " " (name (first x)) "=\"" (second x) "\"")) "" attrs)
        children (rest (rest node))
        inner-text-attr (get (second node) :innerText)
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
             [:button {:class "button is-danger" :name "action" :value (str "delete:" x) :innerText "Delete"}]]]])
        items)))

(defn view [db]
  (html-to-string
   [:html {}
    [:head {}
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:link {:rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css"}]
     [:link {:rel "stylesheet" :href "https://unpkg.com/bulma-prefers-dark@0.1.0-beta.1"}]]
    [:body {}
     [:form {:method "post"}
      [:div {:class "container"}
       [:section {:class "section pt-4"}
        [:div {:class "container"}
         [:div {:class "field"}
          [:label {:class "label" :innerText "New word"}]
          [:div {:class "control"}
           [:input {:class "input" :name "input" :placeholder "<word> - <translation>"}]]]
         [:button {:class "button is-primary" :innerText "Add" :name "action" :value "add"}]]]
       [:section {:class "section py-0"}
        [:h1 {:class "title" :innerText "Words"}]
        (list-view db)]]]]]))

(defn init []
  {:ui {:update (fn [e] (local-event-handler e))
        :view (fn [db] (view db))}
   :schedule {:period 1
              :env {:db {}}
              :main (fn [e] (main e))}})
