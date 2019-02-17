#lang racket

;; You can require more modules of your choice.
(require racket/list
	 "list-comprehension.rkt"
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define etai-list (list #\E #\T #\A #\I))
(define etia-list (list #\E #\T #\I #\A))

(define (etai-mapping subs)
  (map (lambda(x y) (cons x y)) etai-list subs))

(define (etia-mapping subs)
  (map (lambda(x y) (cons x y)) etia-list subs))


(define (atmost-a-from-l a l)
  (cond [(= a 0) '(())]
	[(>= a (length l)) (list l)]
	[else (append (map (lambda(x) (cons (car l) x)) (atmost-a-from-l (- a 1) (cdr l))) (atmost-a-from-l a (cdr l)))]))

(define (first-b b l)
  (define (f-helper b l acc-f)
    (if (= b 0) (acc-f '())
	(f-helper (- b 1) (cdr l) (lambda (x) (acc-f (cons (car l) x))))))
  (f-helper b l (lambda(x) x)))

(define (a-from-first-b l a b)
  (atmost-a-from-l a (first-b b l)))


(define most-unique-neighbours
  (stats:cipher-unique-neighbourhood
   (stats:cipher-bigrams utils:cipher-word-list)
   'both))

(define (ordering x y)
  (match (cons x y)
    [(cons '() '()) #t]
    [(cons (cons a b) (cons a c)) (ordering b c)]
    [(cons (cons a b) (cons c d)) (< (index-of (map car most-unique-neighbours) a) (index-of (map car most-unique-neighbours) c))]))

(define (permute-singles singles)
  (cond [(null? singles) '()]
	[(= 1 (length singles)) (list singles)]
	[(= 2 (length singles)) 
	 (cons singles (list (reverse singles)))]
	[else (append* (map permutations (a-from-first-b singles 2 (length singles))))]))


(define (combination-et.. monograms singles)
  (a-from-first-b (filter (lambda(x) (not (member x singles))) monograms) (max (- 4 (length singles)) 2) (max (- 6 (length singles)) 4)))

;; permute-et gives the better order of permutation of e and t according to neighbours
(define (permute-et combinations)
  (append* (map (lambda(x) (sort (permutations x) ordering)) combinations)))


(define (etai key)
  (let* ([monos (stats:cipher-monograms utils:ciphertext)]
	 [singles (map (lambda(x) (car (string->list x)))
		       (stats:cipher-common-words-single utils:cipher-word-list))]
	 [comb-et (combination-et.. monos singles)]
	 [perm-ai (permute-singles singles)]
	 [perm-et (permute-et comb-et)])
    (filter (lambda(x) (utils:is-monoalphabetic? x key))
            (cond [(null? perm-ai) (map etai-mapping perm-et)]
                  [(null? (cdr perm-ai)) (append-map (lambda(x y) (list x y)) (lc (etai-mapping (append x y)) : x <- perm-et y <- perm-ai) (lc (etia-mapping (append x y)) : x <- perm-et y <- perm-ai))]
                  [else 
                   (lc (etai-mapping (append x y)) : x <- perm-et y <- perm-ai)]))))
	 
  ;(list (list (cons #\E #\o) (cons #\T #\e) (cons #\A #\w) (cons #\I #\q))))

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list etai))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

