(ns page-rank.core
  (:gen-class))

(require '[clojure.string :as str])
  (def incomingLinks (vec (replicate 4 [])))

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
      (read-string (str "[" (subs num 2) "]"))))))

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
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) [[]] (subvec coll (inc pos)))))

(defn summation
    ([x]
    (reduce + (for [link x]
         (/ (pageRank link) (count (outgoingLinks (.indexOf incomingLinks x)) ))))))


(defn rewriteIncomingandRemoveFromOutgoing
  [link page]
  (def incomingLinks 
    (assoc incomingLinks
    link  
    (conj (incomingLinks link) (.indexOf outgoingLinks page)) )
  )
  (if (= link (last page))
    (def outgoingLinks
      (vec-remove outgoingLinks (.indexOf outgoingLinks page))
    )))

(defn generateIncomingLinks
  ([]
  (doseq [page outgoingLinks]
      (doseq [link page]
        (rewriteIncomingandRemoveFromOutgoing link page)
      ))))

(defn calculateAndUpdatePageRank
  [] 
  ; (clojure.java.io/delete-file "ranks.txt")
   (spit "ranks.txt" "")
  ;Calculate PageRank and append to file
   (with-open [wrtr (clojure.java.io/writer "ranks.txt" :append true)]
    (doseq [x incomingLinks]
      (.write wrtr (str 
      (+ 0.15 
      (* 0.85 
      (summation x) 
      )) "\n"))
  ))
  ;Fill in new page ranks
  (def pageRank [])
  (process-file-by-lines "ranks.txt" identity addToPageRank))


(defn -main
  "Calculates Page Rank and outputs timings and ranks to files."
  [& args]
  (println "Hello, Caleb!")

;SETUP 
  ;Read Input File
  (def outgoingLinks [])
  (process-file-by-lines "pages.txt" identity addToOutput)

  ;Generate incomingLinks
  (def incomingLinks (vec (replicate 4 [])))
  (generateIncomingLinks)

  ;Read Input File
  (def outgoingLinks [])
  (process-file-by-lines "pages.txt" identity addToOutput)

  (def pageRank (vec (replicate 4 1)))

  ;Fill in new page ranks
  ; (def pageRank [])
  ; (process-file-by-lines "ranks.txt" identity addToPageRank)
  ; (calculateAndUpdatePageRank)
  
;DO CALCULATION AND TIME
  (with-open [wrtr (clojure.java.io/writer "time.txt" :append true)]
    (dotimes [n 3]
      (.write wrtr (str 
      (with-out-str (time (dotimes [n 3] (calculateAndUpdatePageRank))))
      ))))

  

)
