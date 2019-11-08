(ns page-rank.core
  (:gen-class))

(require '[clojure.string :as str])
(def outgoingLinks [])
(def incomingLinks (vec (replicate 4 [])))
; (def pageRank (vec (replicate 10000 1)))

(def pageRank (vec (replicate 4 1)))

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
      (read-string (str "[" num "]"))))))

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
         (process-fn (subs line 2)))))))


(defn vec-element-remove
  "remove elem from vec in coll"
  [coll pos pos2]
  (vec 
  (concat 
    (subvec coll 0 pos) 
    [
    (vec
    (concat 
      (subvec (coll pos) pos2)
      (subvec (coll pos) (inc pos2))
      ))
    ]
    (subvec coll (inc pos)
  ))))

(defn summation
    ([x]
    (for [link x]
        (/ (pageRank link) (count (outgoingLinks (.indexOf incomingLinks x)))))))

;Credit to: https://stackoverflow.com/a/18319708
(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) [[]] (subvec coll (inc pos)))))


(defn generateIncomingLinks
  ([]
  (for [page outgoingLinks]
      (for [link page]
        (rewriteIncomingandRemoveFromOutgoing link page)
      ))))

(defn rewriteIncomingandRemoveFromOutgoing
  [link page]
  (def incomingLinks 
    (assoc incomingLinks
    link  
    (conj (incomingLinks link) (.indexOf outgoingLinks page)) )
  )
  (def outgoingLinks
    (vec-element-remove outgoingLinks (.indexOf outgoingLinks page) link)
  ))


  (def outgoingLinks
(vec-element-remove outgoingLinks (.indexOf outgoingLinks [1 2]) 1))
  (def incomingLinks (vec (replicate 4 [])))
        (assoc incomingLinks
        2
        (conj (incomingLinks 2) (.indexOf outgoingLinks [1 2])) )


(defn addToInput
  ([num]
    (def incomingLinks
    (conj incomingLinks
    (read-string)
    ))

; (def outgoingLinks [[1 2] [0 3] [1 2] [1 2]])
; (def incomingLinks [[1] [0 2 3] [0 2 3] [1]])
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")

  ;Read Input File
  (def outgoingLinks [])
  (process-file-by-lines "pages.txt")

  ;Generate incomingLinks
  (def incomingLinks (vec (replicate 4 [])))
  (generateIncomingLinks)

  ;Calculate PageRank and append to file
  (for [x (mapv summation incomingLinks)]
    (append-to-file "Example.txt" (+ 0.15 (* 0.85 (reduce + x)) ))
  )
  
  )
