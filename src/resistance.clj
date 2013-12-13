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

(defn number-of-messages
  ([morse dictionary]
    (let [dictionary-morse (set (dictionary-morse dictionary))]
      (number-of-messages morse "" dictionary-morse nil)))
  ([morse preceding-morse dictionary word]
    (let [current-morse (str preceding-morse (first morse))
          rest-morse (rest morse)
          stop (empty? rest-morse)
          words (if (nil? word) dictionary [word])]
      (->> (map #(if (.startsWith % current-morse) % nil) words)
           (filter #(not (nil? %)))
           (map #(if (= current-morse %)
                        (if stop 1 (number-of-messages rest-morse "" dictionary nil))
                        (if stop 0 (number-of-messages rest-morse current-morse dictionary %))))
           (flatten)
           (reduce +))))
)
