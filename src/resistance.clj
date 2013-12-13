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
      (number-of-messages morse "" dictionary-morse)))
  ([morse preceding-morse dictionary]
    (let [current-morse (str preceding-morse (first morse))
          rest-morse (rest morse)
          stop (empty? rest-morse)]
      (->> (map #(if (.startsWith % current-morse) % nil) dictionary)
           (filter #(not (nil? %)))
           (map #(if (= current-morse %)
                        (if stop 1 (inc (number-of-messages rest-morse "" [%])))
                        (if stop 0 (number-of-messages rest-morse current-morse [%]))))
           (flatten)
           (reduce +))))
)
