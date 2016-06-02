(ns alphabet-cipher.coder)

(defn repeat-keyword [n s]
  (take n (cycle s)))

(defn extract-keyword [word]
  (let [length (count word)]
    (loop [i 1]
      (if (or (>= i length)
              (= (repeat-keyword length (take i word)) (seq word)))
        (apply str (take i word))
        (recur (inc i))))))

(def alphabet
  (map char (range (int \a) (inc (int \z)))))

(defn rotate [n s]
  (concat (drop n s) (take n s)))

(defn index [c] (.indexOf alphabet c))

(defn row [c]
  (rotate (index c) alphabet))

(defn decipher-char [m c]
  (nth alphabet (.indexOf (row m) c)))

(defn decipher [cipher message]
  (extract-keyword (map decipher-char message cipher)))

(defn full-keyword [keyword message]
  (apply str (repeat-keyword (count message) keyword)))

(defn encode-char [k m]
  (nth (row m) (index k)))

(defn encode [keyword message]
  (apply str (map encode-char
                  (full-keyword keyword message)
                  message)))

(defn decode [keyword message]
  (apply str (map decipher-char
                  (full-keyword keyword message)
                  message)))
