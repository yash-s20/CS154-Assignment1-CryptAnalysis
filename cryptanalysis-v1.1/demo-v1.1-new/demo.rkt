#lang racket

(require (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in strat: "strategies.rkt")
         (prefix-in  algo: "dict-closure.rkt")
         "main.rkt"
         racket/list)

;; To change the ciphertext and cipher-word-list, change the filename in
;; utils.rkt, as you've been doing.

(define wisdom (utils:encryption-key "wisdom"))

(define (fuzz-key iters key)
  (if (= iters 0)
      key
      (fuzz-key (sub1 iters) (list-set key (random 26) #\_))))


(define key (build-list 26 (lambda (_) #\_)))

;; You can experiment with the following functions in here:
;; crack-cipher
;; secret-word-enumeration
;; algo:dictionary-closure

;; strat:etai

;; stats:cipher-monograms
;; stats:cipher-bigrams
;; stats:cipher-neighbourhood
;; stats:cipher-trigrams
;; stats:cipher-quadgrams
;; stats:cipher-common-words-single
;; stats:cipher-common-words-double
;; stats:cipher-common-words-triple
;; stats:cipher-common-words-quadruple
;; stats:cipher-common-initial-letters
;; stats:cipher-common-final-letters
;; stats:cipher-common-double-letters

;; utils:*

;; For example

;; (crack-cipher (list strat:etai) key)
;; (strat:etai key)
;; (stats:cipher-monograms utils:ciphertext)
;; (stats:cipher-bigrams utils:cipher-word-list)

(define fuzzed (fuzz-key 10 wisdom))
(utils:show-key fuzzed)
;; (secret-word-enumeration fuzzed)
;; (algo:dictionary-closure fuzzed utils:cipher-word-list)
