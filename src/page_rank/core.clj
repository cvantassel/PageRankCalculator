(ns page-rank.core
  (:gen-class))

(require '[clojure.string :as str])
(def incomingLinks (vec (replicate 10000 [])))


; PROCESSING FUNCTIONS

; Credit to: https://www.tutorialspoint.com/clojure/clojure_file_io.htm 
(defn append-to-file
  [file text]
  (with-open [wrtr (clojure.java.io/writer file :append true)]
    (.write wrtr (str text "\n"))))

; Big props to my boy Brian:
; https://stackoverflow.com/a/2640320
(defn addToOutput 
    ([num]
      (def outgoingLinks 
      (conj outgoingLinks 
      (read-string (str "[" 
        (clojure.string/replace num #"^\d* " "")
         "]"))))))

(defn addToPageRank
     ([num]
      (def pageRank 
      (conj pageRank 
      (read-string num )
      ))))

; Credit to: https://stackoverflow.com/a/25950711
(defn process-file-by-lines
  "Process file reading it line-by-line"
  ([file]
   (process-file-by-lines file identity))
  ([file process-fn]
   (process-file-by-lines file process-fn addToOutput))
  ([file process-fn output-fn]
   (with-open [rdr (clojure.java.io/reader file)]
     (doseq [line (line-seq rdr)]
       (output-fn
         (process-fn line ))))))

;Credit to: https://stackoverflow.com/a/18319708
(defn vec-remove
  "remove elem in vect"
  [vect pos]
  (vec (concat (subvec vect 0 pos) [[]] (subvec vect (inc pos)))))

(defn rewriteIncomingandRemoveFromOutgoing
  [link page]
  (def incomingLinks 
    (assoc incomingLinks
    link  
    (conj (incomingLinks link) (.indexOf outgoingLinks page)) )
  )
  (if (and (= link (last page)) (not (nth incomingLinks link)))
    (def outgoingLinks
      (vec-remove outgoingLinks (.indexOf outgoingLinks page))
    )))

(defn generateIncomingLinks
  ([]
  (doseq [page outgoingLinks]
      (doseq [link page]
        (rewriteIncomingandRemoveFromOutgoing link page)
      ))))


; CALCUATION FUNCTIONS

(defn summation
    ([coll]
    (hash-map 
      (keyword (str (.indexOf incomingLinks coll)))
      (+ 0.15 (* 0.85 (reduce + (for [link coll]
        (/ (get pageRank (keyword (str link))) (count (outgoingLinks (.indexOf incomingLinks coll)))) ))))
    )
    )
)


(defn calculatePageRank
  [coll] 
     (apply merge (mapv
      summation coll
      )
  ))


; CONCURRENCY

(defn splitInTwo [numSeq]
  (partition (/ (count numSeq) 2) numSeq))

(defn splitInFour [numSeq]
  (partition (/ (count numSeq) 4) numSeq))

(defn split-8 [numSeq]
  (partition (/ (count numSeq) 8) numSeq))

(defn split-16 [numSeq]
  (partition (/ (count numSeq) 16) numSeq))

(defn split-32 [numSeq]
  (partition (int (/ (count incomingLinks) 32)) numSeq))

(defn split-64 [numSeq]
  (partition (int (/ (count numSeq) 64)) numSeq))


;MAIN

(defn -main
  "Calculates Page Rank and outputs timings and ranks to files."
  [& args]

  ;SETUP 
  ;Read Input File
  (def outgoingLinks [])
  (process-file-by-lines "pages.txt" identity addToOutput)

  ;Generate incomingLinks
  (def incomingLinks (vec (replicate 10000 [])))
  (generateIncomingLinks)

  ;Read Input File (again)
  (def outgoingLinks [])
  (process-file-by-lines "pages.txt" identity addToOutput)

  ;Initialize Page Rank
  (def pageRank (hash-map))
  (doseq [x (range 10000)]
  (def pageRank (assoc pageRank (keyword (str x)) 1)))

  (println "Setup done. Starting calcuations.")

  
;DO CALCULATION AND WRITE TO OUTPUT FILE

  (dotimes [n 2]
    (def pageRank (apply merge (pmap calculatePageRank (split-64 incomingLinks))))
  )

  (spit "time.txt" (.toString pageRank)) 
)
