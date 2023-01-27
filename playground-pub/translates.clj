(defn main [db]
  {:show-toast "FIXME"})

(defn local-event-handler [e2]
  (if (= (first e2) :web)
    (let [e (second e2)]
      (if (= (get e :action) :add)
        {:insert-db {:add (get e :input)}}
        (if (= (get e :action) :delete)
          {:insert-db {:delete (get e :id)}}
          {:error e})))
    {:error2 e2}))

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
             [:button {:class "button is-danger"
                       :innerText "Delete"
                       :hx-post ""
                       :hx-target "body"
                       :hx-vals (str "js:{action:'delete',id:'" x "'}")}]]]])
        items)))

(defn view [db]
  (html-to-string
   [:html {}
    [:head {}
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
      [:section {:class "section py-0"}
       [:h1 {:class "title" :innerText "Words"}]
       (list-view db)]]]]))

(defn init []
  {:ui {:update (fn [e] (local-event-handler e))
        :view (fn [db] (view db))}
   :schedule {:period 1
              :env {:db {}}
              :main (fn [e] (main e))}})
