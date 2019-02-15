#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         racket/match
         (prefix-in utils: "utils.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
  (define (decrypt-partial key cipher-list)
    (map (lambda(x) (utils:decrypt key x)) cipher-list))
  (define partial-plain-text (decrypt-partial key utils:cipher-word-list))
  (define (find-for-each-word ppt)
    (match ppt
      ['() key]
      [(cons a b)
       #:when (decrypted-word? a)
       (begin
         (display a)
         (displayln " <- skipping this one")
       (find-for-each-word b))] ;; word already decrypted
      [(cons word b)
       (match (completion word)
         [#f #f]; key is incorrect
         [#t (begin
               (display word)
               (displayln " <- multiple matches right now")
               (find-for-each-word b))] ;; multiple matches right now
         [w (match (substitution word w)
              [x (if (utils:is-monoalphabetic? x key)
                     (begin
                       (display word)
                       (display " <--> ")
                       (display w)
                       (displayln "... MATCH!")
                       (dictionary-closure (utils:add-substitution x key))) (begin
                                                                              (display word)
                                                                              (displayln " <- No match found")
                                                                              (displayln "dc* : #f")
                                                                              #f))])])]))
                
       ;(cond [(and (completion word) (utils:is-monoalphabetic? (substitution word (completion word))))
         ;                   (dictionary-closure (utils:add-substitution (substitution word (completion word)) key))] ;;recurse or a new more complete key
          ;                 [(completion word) (find-for-each-word b)] ;; no information to extract
           ;                [(completion word) #f])] ;;this means this key is incorrect
  (find-for-each-word partial-plain-text))

(define (completion word)
  ; returns
  ; 1. a #f if there is no completion
  ; 2. a #t if there are multiple completion
  ; 3. a word from the dictionary if unique
  (foldr (lambda(dict-word rest)
           (match rest
             [#f (if (match-char-list (string->list word) (string->list dict-word)) dict-word #f)]
             [#t #t]
             [w (if (match-char-list (string->list word) (string->list dict-word)) #t w)])) #f utils:dictionary))

(define (substitution partial dict-word) ; a list of pair of subsitution, plaintext char to upper text char
  (remove-duplicates (filter (lambda(p) (lower? (cdr p))) (map (lambda (x y) (cons y x)) (string->list partial) (string->list dict-word)))))



(define (decrypted-word? word)
  (foldr (lambda(x y) (and (not (lower? x)) y)) #t (string->list word)))
(define (lower? char)
  (and (< (char->integer char) 123) (> (char->integer char) 96)))

(define (match-char-list plain-partial dict-word)
  (match (cons plain-partial dict-word)
    [(cons '() '()) #t]
    [(cons '() _) #f]
    [(cons _ '()) #f]
    [(cons (cons a b) (cons c d)) (cond [(lower? a) (match-char-list b d)]
                                        [(equal? a c) (match-char-list b d)]
                                        [else #f])]))