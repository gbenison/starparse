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

(define (echo-entry name value)
  (format #t "~a: ~a~%" name value))

(star-parse input-file filter-string echo-entry)

