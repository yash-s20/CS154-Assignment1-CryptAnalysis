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
    (map (λ(x) (utils:decrypt key x)) cipher-list))
   ; (if (null? cipher-list) '()
    ;(let ([word (utils:decrypt key (car cipher-list))])
     ; (cons word (closure-helper key (cdr cipher-list))))))
  (define partial-plain-text (decrypt-partial key utils:cipher-word-list))
  (define (find-for-each-word ppt)
    (match ppt
      ['() key]
      [(cons a b)
       #:when (decrypted-word? a)
       (find-for-each-word b)] ;; word already decrypted
      [(cons word b) (cond [(and (unique-completion? word) (utils:is-monoalphabetic? (substitution word (find-match word))))
                            (dictionary-closure (utils:add-substitution (substitution word (find-match word)) key))] ;;recurse or a new more complete key
                           [(multiple-completion? word) (find-for-each-word b)] ;; no information to extract
                           [(no-completion? word) #f])] ;;this means this key is incorrect
      ))
  (find-for-each-word partial-plain-text))
      
(define (substitution partial dict-word) 0)
(define (decrypted-word? word)
  (foldr (λ(x y) (and (not (lower? x)) y)) #t (string->list word)))
(define (lower? char)
  (and (< (char->integer char) 123) (> (char->integer char) 96)))

(define (match-char-list plain-partial dict-word)
  (match (cons plain-partial dict-word)
    [(cons '() '()) #t]
    [(cons '() _) #f]
    [(cons _ '()) #f]
    [(cons (cons a b) (cons c d)) (cond [(lower? a) (match-char-list b d)]
                                        [(eq? a c) (match-char-list b d)]
                                        [else #f])]))