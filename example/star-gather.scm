#!/bin/sh
# -*-scheme-*-

guile -s $0 $@

!#

(use-modules (starparse))

(define (arg n default)
  (or (false-if-exception (list-ref (command-line) n))
      default))

(define input-file (arg 1 "-"))
(define filter-string (arg 2 #f))

(define result '())

(define (append-entry name value)
  (set! result (cons (cons name value) result)))

(star-parse input-file filter-string append-entry)

