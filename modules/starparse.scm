
(define-module (starparse)
  #:export (star-parse read-bmrb-shifts)
  #:use-module (ice-9 regex))

(load-extension "libstarparse" "starparse_init")

(define (read-bmrb-shifts fname)
  (let ((result (list))
	(entry (list)))
    (define (is-id-token? sym)
      (let ((name (symbol->string sym)))
	(and (string-match "Atom" name)
	     (string-match "ID" name))))
    ;; looking for starting token
    (define (state-1 name value)
      (if (is-id-token? name)
	  (begin (set! entry (assoc-set! entry name value))
		 (set! current-state state-2))))
    ;; building an entry
    (define (state-2 name value)
      (if (is-id-token? name)
	  (begin (set! result (cons entry result))
		 (set! entry (list))
		 (set! current-state state-1)
		 (current-state name value))
	  (set! entry (assoc-set! entry name value))))
    (define current-state #f)
    (begin (set! current-state state-1)
	   (star-parse fname #f (lambda (name value)
				  (current-state name value))))
    (reverse result)))



