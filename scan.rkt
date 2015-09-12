#lang racket
(require "auto.rkt")
(provide scan-init
	scan
	scan-types
	scan-identify
	scan-types
	scan-3-types)

;; SCAN

(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))
(define (scan-identify population)
  (foldl
   (lambda (au h)
     (hash-update h (identify au) add1 0))
   (hash)
   population))

(define (scan-init population)
  (foldl
   (lambda (au h)
     (hash-update h (automaton-init-claim au) add1 0))
   (hash)
   population))


(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))

(define (scan-types population)
  (let ([type-list (scan-init population)])
    (list
     (hash-ref* type-list 0)
     (hash-ref* type-list 1))))

(define (scan-3-types population)
  (let ([type-list (scan-init population)])
    (list
     (hash-ref* type-list 0)
     (hash-ref* type-list 1)
     (hash-ref* type-list 2))))


