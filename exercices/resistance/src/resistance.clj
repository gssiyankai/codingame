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
      (let [matching-word (fn [sentence word] (.startsWith morse word))
            same-size (fn [word1 word2] (= (count word1) (count word2)))]
        (->> (map (fn [[word num]]
                    (if (matching-word morse word)
                      (if (same-size morse word)
                        1
                        (* num (morse-messages (subs morse (count word)) dictionary)))
                        0
                      )
                   )
                  dictionary)
             (reduce +)))))
)

(defn number-of-messages
  [morse dictionary]
    (let [dictionary-morse (frequencies (dictionary-morse dictionary))]
      (morse-messages morse dictionary-morse))
)
