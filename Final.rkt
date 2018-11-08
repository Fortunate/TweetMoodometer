#lang racket
(require data-science-master)
(require plot)
(require math)
(require json)
(require srfi/19)
(require racket/stream)
(require racket/system)







;;; This function reads line-oriented JSON (as output by massmine),
;;; and packages it into an array. For very large data sets, loading
;;; everything into memory like this is heavy handed. For data this small,
;;; working in memory is simpler

(provide joined-tweets sentiment-analysis)

(define (read-tweets filename)
  (string->jsexpr
                (with-input-from-file filename (λ () (json-lines->json-array)))))

(define (read-tweets-twurl filename)
  (let ([stringout (with-output-to-string (lambda ()(system filename)))])
    ;(display stringout)
    (hash-ref (with-input-from-string stringout (λ () (read-json))) 'results)
;(hash-ref (with-input-from-string stringout (λ () (read-json))) 'statuses)
    
  ))

(define (json-lines->json-array #:head [head #f])
  (let loop ([num 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and head (>= num head)))
        (jsexpr->string json-array)
        (loop (add1 num) (cons record json-array)
              (read-json (current-input-port))))))

;;; Normalize case, remove URLs, remove punctuation, and remove spaces
;;; from each tweet. This function takes a list of words and returns a
;;; preprocessed subset of words/tokens as a list

(define (clean-text x)
   
         (string-normalize-spaces
          (remove-punctuation
           (remove-urls
            (string-downcase x)))))

;;; Read in the entire tweet database (3200 tweets from Trump's timeline)
;(joined-tweets "/usr/local/bin/twurl /1.1/search/tweets.json?q=nasa&result_type=popular")

;; Remove just the tweet text and source from each tweet
;;; hash. Finally, remove retweets.
;;; Remove just the tweet text, source, and timestamp from each tweet
;;; hash. Finally, remove retweets.

(define (analyse-tweets twurl-request)
  (let ([ff (joined-tweets twurl-request)])
  (sentiment-analysis ff))
  )
;"/usr/local/bin/twurl /1.1/tweets/search/30day/dev.json?query=realDonaldTrump search api&place_country=UK&fromDate=201710090000&toDate=201711090000"
(define (tweetlist filename)
  (define tweets (read-tweets-twurl filename))
  (let ([tmp (map (λ (x) (list  (hash-ref x 'text))) tweets)]) ;; improve to use streams
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)
     
    ))

;;t is a list of lists of strings. Tail recursion is used to extract each string and append
;; it into one large string.

(define (joined-tweets filename)
    (local[
           (define (joined1 tlist1 acc)
             (cond [(empty? tlist1) acc]
                   [else (joined1 (rest tlist1) (string-join (list acc "\n " (clean-text (first(first tlist1))))))]
                   )
             )
           ](joined1 (tweetlist filename) "")) )



;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label.

(define (sentiment-analysis tweet-text)
  (let* ([words (document->tokens tweet-text #:sort? #t)]
        [sentiment (list->sentiment words #:lexicon 'nrc)])
;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.     
  (take sentiment 5)
  (let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
 
 ))

;;> (define t (joined-tweets "=trump1_tweets.json"))
;;> (sentiment-analysis t)
;(define rr (joined-tweets "/usr/local/bin/twurl /1.1/search/tweets.json?q=realDonaldTrump&count=700"))
;(sentiment-analyis rr)

;(define out (open-output-file "new.json"))
;(write (read-tweets "/usr/local/bin/twurl /1.1/search/tweets.json?q=nasa&result_type=popular") out);(write "hello world" out)
;(close-output-port out)
