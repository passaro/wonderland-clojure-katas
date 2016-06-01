(ns alphabet-cipher.coder)

(defn repeatme [n s]
  (take n (flatten (repeat s))))

(defn extract [word]
  (let [length (count word)]
    (loop [i 1]
      (if (or (>= i length)
              (= (repeatme length (take i word)) (seq word)))
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
  (extract (map decipher-char message cipher)))

(defn full-keyword [keyword message]
  (apply str (repeatme (count message) keyword)))

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
