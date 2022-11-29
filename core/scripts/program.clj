;;

(def rules [["ch'" "ჭ"] ["ts'" "წ"] ["ch" "ჩ"] ["dz" "ძ"] ["gh" "ღ"] ["k'" "კ"] ["kh" "ხ"] ["p'" "პ"] ["sh" "შ"] ["t'" "ტ"] ["ts" "ც"] ["zh" "ჟ"] ["a" "ა"] ["b" "ბ"] ["c" "ც"] ["d" "დ"] ["e" "ე"] ["f" "ფ"] ["g" "გ"] ["h" "ჰ"] ["i" "ი"] ["j" "ჯ"] ["k" "ქ"] ["l" "ლ"] ["m" "მ"] ["n" "ნ"] ["o" "ო"] ["p" "ფ"] ["q" "ყ"] ["r" "რ"] ["s" "ს"] ["t" "თ"] ["u" "უ"] ["v" "ვ"] ["w" "ვ"] ["x" "ხ"] ["z" "ზ"]])

(defn decode [s]
  (reduce
   (fn [a x]
     (let [from (first x) to (first (rest x))]
       (.replaceAll a from to)))
   s
   rules))

(defn main [env msg]
  (let [decoded (decode (.toLowerCase (:body msg)))]
    {:translate {:body decoded :callback :translate-callback}}))

(defn translate-callback [env resp]
  {:telegram {:token "__TELEGRAM_TOKEN__" :user "__USER_ID__" :message resp}})

#_(comment

    (=
     (:body (:translate (main nil {:body "ojaxshi"})))
     "ოჯახში")

    ())
