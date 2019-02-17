#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/match
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(cipher-common-words-single utils:cipher-word-list)
;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:

         ;; my-fundoo-analysis
         )

;;sort a list in decreasing order of frequencies
(define (sort-and-set word-list)
  (define (sort-helper w-l s)
    (match w-l
      ['() s]
      [(cons a b) (let ([c (count (lambda(x) (equal? x a)) w-l)]
                        [rest (filter (lambda(x) (not (equal? x a))) w-l)])
                    (sort-helper rest (cons (cons a c) s)))]))
  (sort (sort-helper word-list '()) (lambda(x y) (>= (cdr x) (cdr y)))))

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.


(define (is-character? a)
  (and (> (char->integer a) 96) (< (char->integer a) 123)))

(define (cipher-monograms ciphertext)
  (define (is-char-this a)
    (lambda(x) (equal? a x)))
  (define (insert char cnt l)
    (cond [(null? l) (list (cons char cnt))]
          [(> (cdar l) cnt) (cons (car l) (insert char cnt (cdr l)))]
          [else (cons (cons char cnt) l)]))
        
  (define (monogram-helper filtered)
    (if (null? filtered) '()
        (let* ([that-char (car filtered)]
               [c (count (is-char-this that-char) filtered)]
               [rest (filter (lambda(x) (not ((is-char-this that-char) x))) filtered)])
          (insert that-char c (monogram-helper rest))))) 

  (let ([filtered-cipher (filter is-character? (string->list ciphertext))])
    (map car (monogram-helper filtered-cipher))))
    


;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

(define (cipher-bigrams cipher-word-list)
  (define (word-bigrams w-l acc)
    (match w-l
      [(cons a '()) acc]
      [(cons a (cons b c)) (word-bigrams (cons b c) (cons (list->string (list a b)) acc))]))
  (map car (sort-and-set (append* (map (lambda(word) (word-bigrams (string->list word) '())) cipher-word-list)))))


;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define empty-neighbourhood
  (build-list 26 (lambda(x) (cons (integer->char (+ x utils:CIPHER-BEGIN)) 0))))
(define (complete-neighbourhood l)
  (append l (filter (lambda(x) (not (member (car x) (map car l)))) empty-neighbourhood)))

(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define (predecessor bigram l)
    (cons (list (car (string->list bigram))) l))
  (define (successor bigram l)
    (cons (cdr (string->list bigram)) l))
  (define (both bigram l)
    (cons (remove-duplicates (string->list bigram)) ;(filter (lambda(x)
				;	    (not (or (equal? (string->list bigram) x)
					;		     (equal? (reverse (string->list bigram)) x)))) l)))
	  l))
  (let ([filt (match mode
                ['predecessor predecessor]
                ['successor successor]
                ['both both])])
    (complete-neighbourhood (sort-and-set (append* (remove-duplicates (foldr
                                                    filt
                                                    '()
                                                    cipher-bigrams-list)))))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define (predecessor bigram)
    (list (car (string->list bigram))))
  (define (successor bigram)
    (cdr (string->list bigram)))
  (define (both bigram)
    (string->list bigram))
  (let ([filt (match mode
                ['predecessor predecessor]
                ['successor successor]
                ['both both])])
    (complete-neighbourhood (sort-and-set (append* (map
                                                    filt
                                                    cipher-bigrams-list))))))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

;; not needed
(define (cipher-trigrams cipher-word-list)
  '())

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

;; not needed
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!


(define (cipher-common-words-single cipher-word-list)
  (let ([single-words-list (filter (lambda(x) (= (length (string->list x)) 1)) cipher-word-list)])
    (map car (sort-and-set single-words-list))))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
;; not needed
(define (cipher-common-words-double cipher-word-list)
  (let ([double-words-list (filter (lambda(x) (= (length (string->list x)) 2)) cipher-word-list)])
    (map car (sort-and-set double-words-list))))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.

;; not needed
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.

;; not needed
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!

;; not needed
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!

;; not needed
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!

;; not needed
(define (cipher-common-double-letters cipher-word-list)
  '())
