#lang racket

(provide struct-out automaton
	automaton-init-claim
	automaton-hh
	automaton-hm
	automaton-hl
	automaton-mh
	automaton-mm
	automaton-ml
	automaton-lh
	automaton-lm
	automaton-ll
         accommodator
         all-lows
         all-mediums
         all-highs
         identify
         all-lows?
         all-mediums?
         all-highs?
         identify-2-types)




(struct automaton (init-claim hh hm hl mh mm ml lh lm ll) #:transparent)
(define accommodator (automaton 1 0 1 2 0 1 2 0 1 2))
(define all-highs (automaton 2 2 2 2 2 2 2 2 2 2))
(define all-mediums (automaton 1 1 1 1 1 1 1 1 1 1))
(define all-lows (automaton 0 0 0 0 0 0 0 0 0 0))

(define (identify automaton)
  (drop (vector->list (struct->vector automaton)) 1))

(define (all-highs? automaton)
  (equal? automaton all-highs))
(define (all-mediums? automaton)
  (equal? automaton all-mediums))
(define (all-lows? automaton)
  (equal? automaton all-lows))
(define (accommodator? automaton)
  (equal? automaton accommodator))
(define (identify-2-types population)
  (list
   (count all-highs? population)
   (count all-mediums? population)
   ))




