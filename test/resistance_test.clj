(ns resistance_test
  (:use clojure.test)
  (:use resistance))

(defn read-file [file]
  (clojure.string/split-lines
    (slurp file))
)

(defn test-morse [id]
  (let [output (read-file
                 (str "Test_" id "_output.txt"))
        expected (read-string
                   (first output))
        input (read-file
                 (str "Test_" id "_input.txt"))
        morse (first input)
        dictionary (rest
                     (rest input))]
    (is (= expected (number-of-messages morse dictionary))))
)

(deftest test-dictionary-morse
  (is (= [".-"] (dictionary-morse ["A"])))
  (is (= ["......-...-..---"] (dictionary-morse ["HELLO"])))
  (is (= ["--.-------..", "-----.-.-...-.--."] (dictionary-morse ["GOOD" "MORNING"])))
)

(deftest test-correct-detection-of-a-letter
  (test-morse 1)
)

(deftest test-correct-detection-of-a-word
  (test-morse 2)
)

(deftest test-simple-messages
  (test-morse 3)
)

(deftest test-long-sequence-large-dictionary
  (test-morse 4)
)

(run-tests 'resistance_test)