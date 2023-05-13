#lang racket
(require racket/hash)
(require racket/file)
(require data/either)

;This part of the program deals with the preprocessing.

;Read a file as one string.
(define (file-contents file)
  (port->string (open-input-file file) #:close? #t))

;Returns a list of the stop words
(define (get-stop-words)
  (string-split (file-contents "stop_words_english.txt")))

;Removes punctuation from the file and converts it to downcase
(define (remove-punctuation file)
 (string-downcase
 (string-replace
  (string-replace
   (string-replace
    (string-replace
     (string-replace
      (string-replace
       (string-replace
        (string-replace
         (string-replace
          (string-replace
           (string-replace
            (string-replace
             (string-replace
              (string-replace (string-replace file ")" " ") "(" " ") "\"" " ") "“" " ") "—" " ") ";" " ") ":" " ") "-" " ") "!" " ") "?" " ") "." " ") "," " ") "”" " ") "\r" " ") "\n" " ")))

#|
This function takes all the files in the "Files" folder and removes the punctuation from them.
(directory-list) function returns a list of all the file paths in the folder.
The (file-contents) function will allow us the read the content of the file allowing for the punctuation to be removed.
In the end, this function will return a list of files.
|#
(define (remove-punctuation-from-files list-of-files)
  (if (empty? list-of-files)
      list-of-files
      (cons (remove-punctuation (file-contents (first list-of-files))) (remove-punctuation-from-files (rest list-of-files)))))



;This function takes the list of files and produces a hash of each word count for each file.
;This funciton will return a list of hashes where each entry in the list is a hash of the word count for each word in that particular file.
(define (create-list-of-hashes list-of-files)

  (define (create-hash hash-table file-to-list-of-words)
    (if (empty? file-to-list-of-words)
        hash-table
        (if (hash-has-key? hash-table (first file-to-list-of-words))
            (create-hash (hash-update hash-table (first file-to-list-of-words) add1) (rest file-to-list-of-words))
            (create-hash (hash-union hash-table (hash (first file-to-list-of-words) 1)) (rest file-to-list-of-words)))))
    
  (if (empty? list-of-files)
      list-of-files
      (cons (create-hash (hash (first (string-split (first list-of-files))) 1) (rest (string-split (first list-of-files)))) (create-list-of-hashes (rest list-of-files)))))


;This function will take the list of hashes and removes all the stop words from them.
;This function will return an updated list of hashes that have the stop words removed from them.
(define (remove-stop-words list-of-hashes list-of-stop-words)

  (define (remove-hashes current-hash list-of-stop-words)
    (if (empty? list-of-stop-words)
        current-hash
        (remove-hashes (hash-remove current-hash (first list-of-stop-words)) (rest list-of-stop-words))))

  (if (empty? list-of-hashes)
      list-of-hashes
      (cons (remove-hashes (first list-of-hashes) list-of-stop-words) (remove-stop-words (rest list-of-hashes) list-of-stop-words))))


;This function takes the list of hashes and calcualtes the relative frequence of each word for each hash.
;This function will return an updated list of hashes that has the relative frequency calculated for each word for each hash.
(define (calculate-rel-freq list-of-hashes)

  (define (add value-list)
    (if (empty? value-list)
      0
      (+ (add (rest value-list)) (first value-list))))

  (define (relative-freq key value )
    (values key(* (log (/ value (add (hash-values (first list-of-hashes))) ) 10) -1)))

  (if (empty? list-of-hashes)
      list-of-hashes
      (cons (hash-map/copy (first list-of-hashes) relative-freq) (calculate-rel-freq (rest list-of-hashes)))))

;This program takes each file name and hashes it to a hash of words.
(define (assign-filename-to-hash list-of-hashes list-of-filenames)

  (if (empty? list-of-hashes)
      list-of-filenames
      (cons (hash (some-system-path->string (first list-of-filenames)) (first list-of-hashes)) (assign-filename-to-hash (rest list-of-hashes) (rest list-of-filenames)))))


;This part of the program deals with user interaction.

;This function goes through all our files counting the number of matches from the search word(s) that the user entered.
;This function returns a list of pairs with the file name being the first value and the match count as the second value in each pair.
(define (find-files-with-matches list-of-hashes  list-of-search-words)

  (define (count-matches current-hash current-filename list-of-search-words match-count)
    (cond
      [(and (empty? list-of-search-words) (> match-count 0))   (success (cons current-filename match-count))]
      [(and (empty? list-of-search-words) (= match-count 0))  (failure empty)]
      [(hash-has-key? current-hash (first list-of-search-words))   (count-matches current-hash current-filename (rest list-of-search-words) (add1 match-count))]
      [else (count-matches current-hash current-filename (rest list-of-search-words) match-count)]))


  (if (empty? list-of-hashes)
      list-of-hashes
      (cons  (from-either (count-matches (first (hash-values (first list-of-hashes))) (first (hash-keys (first list-of-hashes))) list-of-search-words 0)) (find-files-with-matches (rest list-of-hashes) list-of-search-words))))

;This function calculates the total frequency score for each word that appears in each file
(define (add-scores-for-matched-words list-of-matches list-of-hashes list-of-search-words)

  (define (add-scores current-filename current-hash list-of-search-words total-score)
    (if (empty? list-of-search-words)
        (cons current-filename total-score)
        (if (hash-has-key? current-hash (first list-of-search-words))
            (add-scores current-filename current-hash (rest list-of-search-words) (+ (hash-ref current-hash (first list-of-search-words))  total-score))
            (add-scores current-filename current-hash (rest list-of-search-words) total-score))))
    

  (if (empty? list-of-matches)
      empty
      (if (equal? (car (first list-of-matches)) (first (hash-keys (first list-of-hashes))))
          (cons (add-scores (car (first list-of-matches)) (first (hash-values (first list-of-hashes))) list-of-search-words 0) (add-scores-for-matched-words (rest list-of-matches) (rest list-of-hashes) list-of-search-words))
          (add-scores-for-matched-words list-of-matches (rest list-of-hashes) list-of-search-words))))

;This function will display the search results and the first line of each file
(define (display-search-results list-of-files-and-ratings)
  (displayln " ")
  (cond
    [(empty? list-of-files-and-ratings) "End of search results"]
    [else (displayln (car (first list-of-files-and-ratings)))
          (displayln (first (file->lines (string->path (string-append "Files\\" (car (first list-of-files-and-ratings)))))))
          (display-search-results (rest list-of-files-and-ratings))]))
          
              


;Main Program
(displayln "Hash is loading please wait (Takes about 30 seconds - 1 minute) ...")

(define hash-of-files-and-word-counts (assign-filename-to-hash (calculate-rel-freq (remove-stop-words (create-list-of-hashes (remove-punctuation-from-files (directory-list "Files" #:build? #t))) (get-stop-words)))
                                                              (directory-list "Files" #:build? #f)))

;Get user input
(display "Enter a search word or words: ")
(define user-input (map string-downcase (string-split (first (list (read-line (current-input-port) 'any))))))

;List of all the files and how many words that match in each
(define list-of-matches (remove* '( () ) (find-files-with-matches hash-of-files-and-word-counts user-input)))


;List of all the files and their ratings
(define list-of-ratings (add-scores-for-matched-words list-of-matches hash-of-files-and-word-counts user-input))

;Sort the ratings in ascending order
(define sorted-list-of-ratings (sort list-of-ratings (lambda (x y) (< (cdr x) (cdr y)))))

;Get the search results
(displayln "Here are the search results:")

(if (empty? sorted-list-of-ratings)
    (display "No Search Results Found")
    (display-search-results sorted-list-of-ratings))







 

