#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration
         correct-key?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (to-lower-case char)
  (integer->char (+ utils:CIPHER-BEGIN (utils:plain-char-offset char))))

(define (word-to-lower word)
  (list->string (map to-lower-case (string->list word))))


(define (complete? key)
  (if (member #\_ key) #f #t))

(define (candidate? word)
  (define (unique? word)
    (match word
      ['() #t]
      [(cons a b) (and (not (member a b)) (unique? b))]))
  (and (= 6 (length word)) (unique? word)))

(define (six-letter-words dict)
  (foldr (lambda (x y)
           (if (candidate? (string->list x))
               (cons (word-to-lower x) y)
               y))
           '()
           dict))
(define possible-secret-list (six-letter-words utils:dictionary))
(define (correct-key? partial-key complete-key)
  (match (cons partial-key complete-key)
    [(cons '() '()) #t]
    [(cons _ '()) #f]
    [(cons '() _) #f]
    [(cons (cons #\_ rest-part) (cons _ rest-full)) (correct-key? rest-part rest-full)]
    [(cons (cons a rest-part) (cons a rest-full)) (correct-key? rest-part rest-full)]
    [(cons (cons a _) (cons b _)) #f]))

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
  (match (foldr (lambda(x y)
           (let ([key-this-word (utils:encryption-key x)]
                 [nothing-after? (equal? key-after-dictionary-closure y)])
             (cond [(and (correct-key? key-after-dictionary-closure key-this-word) nothing-after?) (begin
                                                                                                     (display x)
                                                                                                     (displayln "?")
                                                                                                     key-this-word)]
                   [(not y) y]
                   [(correct-key? key-after-dictionary-closure key-this-word) (begin
                                                                                (displayln "swe: #f")
                                                                                #f)]
                   [(not nothing-after?) y]
                   [else key-after-dictionary-closure]))) key-after-dictionary-closure possible-secret-list)
    [#f #f]
    [key #:when(complete? key)
         (begin
           (displayln "Just one unique secret word found!")
           (utils:show-key key) key)]
    [key key]))

