#lang racket

(require "auto.rkt")
(provide 
	create-test-population)

(define (create-test-population high medium low accom)
  (set! series (list (list high medium)))
  (set! N (sum (list high medium low accom)))
  (shuffle
   (append
    (make-list high all-highs)
    (make-list medium all-mediums)
    (make-list low all-lows)
    (make-list accom accommodator))))
