(ns german-trainer.core
  (:gen-class))

(require '[clojure.java.io :as io])
(require '[clojure.string :as string])

(def wordlist-path "resources/wordlist.txt")
(def translations-all-path "resources/translations.txt")
(def translations-tobe-path "resources/translations-tobe.txt")
(def translations-complete-path "resources/translations-complete.txt")
(def minutes 3)
(def translated-times-treshold 5)

(def session-time (* 60 minutes))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn not-duplicate-key? [m k]
  (not (contains? m k)))

(defn prompt 
  "A simple prompt reading standard in. Returns the line that was read"
  []
  (do
    (print ">>> ")
    (flush)
    (string/lower-case (read-line))))

(defn file-to-seq
  "Transforms the contents of a file into a sequence"
  [fname]
  (with-open [rdr (io/reader fname)]
    (doall (line-seq rdr))))

(defn ntranslations-in-file 
  "The number of wordpair translations the program knows"
  [fname]
  (/ (count (file-to-seq fname)) 4))

(defn seq-to-file 
  "Writes a sequence to a file, each element separated by a newline character"
  [sq fname append?]
  (with-open [wtr (io/writer fname :append append?)]
    (doseq [element sq] (.write wtr (str element "\n")))))

(defn but-firstline 
  "Removes the first line of the file under fname"
  [fname]
  (seq-to-file (rest (file-to-seq wordlist-path)) fname false))

(defn seq-into-map 
  "Converts a seq into a map, where elements with even indices becomes the keys 
  and the elements with odd indices becomes the values"
  [lines]
  (zipmap 
    (take-nth 4 lines) 
    (map vector 
         (map (fn [line] (string/split line #",")) (take-nth 4 (rest lines)))
         (map parse-int (take-nth 4 (rest (rest lines))))
         (map parse-int (take-nth 4 (rest (rest (rest lines))))))))

(defn file-into-map 
  "produces a map from the contents of the file specified by fname. Every even
  numbered row will be a key and every odd valued row will be a value in this
  map"
  [fname]
  (seq-into-map (file-to-seq fname)))

(defn filter-often-translated
  "Filters out the words that have alread been translated more than a certain treshold"
  [translationmap]
  (filter 
    (fn 
      [[_ [_ ntimess ntimesf]]] 
      (>= ntimess (+ translated-times-treshold ntimesf)))
    translationmap))


(defn make-translation
  "Given a word and its translation, prompts the uses whether to save it. Assembles
  a map containing the answer as a string, the word and its translation"
  [word translation]
  (println " Keep: " word " -> [" translation "]? ([y]es, [s]kip)")
  {:save? (prompt) :english word :german translation})

(defn prompt-translation-generated 
  "Generates a word to translate, prompts the user for a translation for it, then
  makes a translation"
  []
  (let [[word & _] (file-to-seq wordlist-path) ]
    (println "Translate this word for me:" word) 
    (let [translation (prompt)]
      (make-translation word translation))))

(defn prompt-translation-custom 
  "Prompts the user for a word and its translation, then makes a translation"
  []
  (println " Enter an english word")
  (let [word (prompt)]
    (println " Enter the german translation")
    (let [translation (prompt)]
      (make-translation word translation))))

(defn save-translation 
  "Appends a pair of strings to a file under fname, separated by a newline character"
  [s1 s2]
  (spit translations-tobe-path (str s1 "\n" s2 "\n0\n0\n") :append true)
  (spit translations-all-path (str s1 "\n" s2 "\n0\n0\n") :append true))

(defn try-save-translation 
  "Given a translation and a map of translations, asserts that the translation does
  not already exist in the translationmap. If not, and if a save was requested, saves
  the translation." 
  [translation translationmap]
  (if 
    (and 
        (= (:save? translation) "y") 
        (not-duplicate-key? translationmap (:english translation)))
    (save-translation (:english translation) (:german translation)) 
    (println "Not saving this translation")))

(defn translate-custom
  "Make a user defined translation"
  []
  (let [translation (prompt-translation-custom)
        translations (file-into-map translations-tobe-path)]
    (try-save-translation translation translations)))

(defn translate-generated
  "Make a generated translation"
  []
  (let [translation (prompt-translation-generated)
        translations (file-into-map translations-tobe-path)]
    (try-save-translation translation translations)
    (if (re-matches #"[ys]" (:save? translation))
           (but-firstline wordlist-path))))

(defn lookup-translation
  "Look up a translation"
  []
  (println "Enter the english word to look up translation for")
  (let [word (prompt)
        translationmap (file-into-map translations-all-path)]
    (println "->" (translationmap word))))

(defn do-translation 
  [translations-left all-translations time-left ntranslated nwords]
  (if (and (< time-left 0) (not (empty? translations-left)))
    [ntranslated all-translations]
    (let [word (rand-nth (keys translations-left))
          translation (first (translations-left word))
          times-translated (second (translations-left word))
          times-failed (nth (translations-left word) 2)
          starttime (System/nanoTime)]
      (println (str 
                 "(" nwords ") "
                 "> " 
                 word 
                 " < "
                 "(" times-translated ", " times-failed ")"
                 " time: " 
                 (int time-left)))
      (let [element (some #{(prompt)} translation)
            time-taken (/ (- (System/nanoTime) starttime) 1e9)]
        (if (= element ":q")
          [ntranslated all-translations]
          (do-translation 
            (dissoc translations-left word) 
            (if (some? element) 
              (do (println "Correct!\n")
                  (assoc 
                    all-translations 
                    word 
                    [translation (inc times-translated) times-failed]))
              (do (println "WRONG! Should have been: " translation "\n")
                  (assoc 
                    all-translations 
                    word 
                    [translation times-translated (inc times-failed)])))
            (- time-left time-taken) 
            (if (some? element) (inc ntranslated) ntranslated)
            (inc nwords)))))))



(defn translation-session []
    (let [translationmap (file-into-map translations-tobe-path)
          ntrans-tobe (ntranslations-in-file translations-tobe-path)
          starttime (System/nanoTime)
          time-left session-time
          session (do-translation translationmap translationmap time-left 0 0)
          [ntranslated new-map] session
          done-translations (filter-often-translated new-map)
          done-translation-seq (map (partial string/join ",") 
                                    (map first (vals done-translations)))
          done-ntimes-seq (map second (vals done-translations))
          done-ntimesf-seq (map (fn [[_ _ f]] f) (vals done-translations))
          updated-map (apply dissoc new-map (keys done-translations))
          translation-seq (map (partial string/join ",") 
                               (map first (vals updated-map)))
          ntimes-seq (map second (vals updated-map))
          ntimesf-seq (map (fn [[_ _ f]] f) (vals updated-map))]
      (println " Time is up! You translated" ntranslated "words correctly.")
      (println " " (count done-translations) "words became fully translated in this session")
      (seq-to-file 
        (interleave (keys done-translations) done-translation-seq done-ntimes-seq done-ntimesf-seq)
        translations-complete-path
        true)
      (seq-to-file 
        (interleave (keys updated-map) translation-seq ntimes-seq ntimesf-seq) 
          translations-tobe-path
          false)))

(defn known-words-translation-session []
  (let [translationmap (file-into-map translations-complete-path)]
    (do-translation translationmap translationmap session-time 0 0)))

(defn main []
  (println "\n I am a translation program. I currently know"
           (ntranslations-in-file translations-all-path)
           "english words"
           "\n and their german translations."
           "Of those,"
           (ntranslations-in-file translations-tobe-path) 
           "words remains to be translated 5 times"
           "\nI am done with:" 
           (ntranslations-in-file translations-complete-path) 
           "words"
           "\n A training session is" 
           (/ session-time 60) 
           "minute(s) long"
           "\n You can enter:"
           "\n\n g\t\tfor translating a generated word," 
           "\n e\t\tfor entering a new word to translate,"
           "\n l\t\tfor looking up a translation"
           "\n d\t\tfor the translation challenge,"
           "\n k\t\tfor known-words challenge,"
           "\n set time [Num]\tto set a new translation challange time,"
           "\n q\t\tto quit\n")
  (let [ans (prompt)]
    (case ans
      "g" (translate-generated)
      "e" (translate-custom)
      "l" (lookup-translation)
      "d" (translation-session)
      "k" (known-words-translation-session)
      (if-let [t (re-matches #"set time [0-9]+" ans)] 
        (def session-time (* 60 (parse-int (last (string/split t #" ")))))
        (println " Unknown input")))
    (if (= ans "q") :done (main))))

(defn -main [&args]
  (main)) 

