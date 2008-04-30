
(define-module (starparse)
  #:export (star-parse
	    write-star-loop)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format))

(load-extension "libstarparse" "starparse_init")

(define (all-=? elts)
  (fold (lambda (elt prev)(= elt (car elts))) #t elts))

;
; data1, data2, ... lists of field name followed by values
; for that field; all data(n) must be of equal length
;
(define (write-star-loop . data)
  (if (not (all-=? (map length data)))
      (error "write-star-loop: inconsistent lengths"))
  (display "loop_\n")
  (for-each (lambda(f)(format #t "_~a~%" f)) (map car data))
  (let loop ((data (map cdr data)))
    (if (>= (length (car data)) 1)
	(begin
	  (display "   ")
	  (for-each
	   (lambda (field)(format #t "~8a " field))
	   (map car data))
	  (newline)
	  (loop (map cdr data)))))
  (display "stop_\n"))



