#!/bin/sh
# -*-scheme-*-

exec guile -s $0 $@

#
# This example gathers chemical shift information by atom from a star file,
# puts into a list of lists called 'result'
#
!#


(use-modules (starparse)
	     (ice-9 regex))

(if (getenv "SLEEP")
    (begin
      (format #t "pid --> ~a~%" (getpid))
      (sleep 10)))

(define (arg n default)
  (or (false-if-exception (list-ref (command-line) n))
      default))

(define input-file (arg 1 "-"))

(define result '())
(define entry '())

(define (is-id-token? sym)
  (let ((name (symbol->string sym)))
    (and (string-match "Atom" name)
	 (string-match "ID" name))))

;; looking for starting token
(define (state-1 name value)
;  (format #t "** state 1: ~a ~a~%" name value)
  (flush-all-ports)
  (if (is-id-token? name)
      (begin (set! entry (assoc-set! entry name value))
	     (set! current-state state-2))))

;; building an entry
(define (state-2 name value)
;  (format #t "** state 2: ~a ~a~%" name value)
  (flush-all-ports)
  (if (is-id-token? name)
      (begin (set! result (cons entry result))
	     (set! entry (list))
	     (set! current-state state-1)
	     (current-state name value))
      (set! entry (assoc-set! entry name value))))

(define current-state state-1)

(star-parse input-file #f (lambda (name value)(current-state name value)))

