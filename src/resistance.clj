(ns resistance)

(defn alphabet []
  { \A ".-"   \B "-..." \C "-.-." \D "-.."
    \E "."    \F "..-." \G "--."  \H "...."
    \I ".."   \J ".---" \K "-.-"  \L ".-.."
    \M "--"   \N "-."   \O "---"  \P ".--."
    \Q "--.-" \R ".-."  \S "..."  \T "-"
    \U "..-"  \V "...-" \W ".--"  \X "-..-"
    \Y "-.--" \Z "--.." }
)

(defn dictionary-morse [dictionary]
  (map (fn [word] (apply str (map #((alphabet) %) word))) dictionary)
)

(def morse-messages
  (memoize
    (fn [morse dictionary]
      (->> (map #(cond (= morse %) 1
                       (.startsWith morse %) (morse-messages (subs morse (count %)) dictionary)
                       :else 0)
              dictionary)
           (reduce +))))
)

(defn number-of-messages
  [morse dictionary]
    (let [dictionary-morse (set (dictionary-morse dictionary))]
      (morse-messages morse dictionary-morse))
)
