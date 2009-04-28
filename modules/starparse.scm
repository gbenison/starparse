
(define-module (starparse)
  #:export (star-parse
	    write-star-loop)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format))

(load-extension "libstarparse" "starparse_init")

(define (all-=? elts)
  (fold (lambda (elt prev)(= elt (car elts))) #t elts))

;
; Return a procedure that takes a string as an argument
; and returns a string left-padded with whitespace to make
; total length a multiple of 'n' with at least one
; whitespace-padding character
;
(define (tab-expand n)
  (lambda (str)
    (string-pad
     str
     (* n (ceiling (/ (+ 1 (string-length str)) n))))))

;
; Format 'field', any scheme type, as a string suitable for
; inclusion in a star file.
; escape the resulting string to avoid problems with comment characters, whitespace, etc.
;
(define (format-star-field field)
  (define canonicalize
    (let ((cs (char-set-union
	       char-set:letter+digit
	       (string->char-set "$+-."))))
      (lambda (char)
	(if (char-set-contains? cs char) char #\_))))
  (let ((raw-string
	 (cond ((string? field)
		field)
	       ((symbol? field)
		(symbol->string field))
	       ((number? field)
		(number->string field))
	       (else "."))))
    (string-map canonicalize raw-string)))

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
	   display
	   (map (tab-expand 8)
		(map format-star-field (map car data))))
	  (newline)
	  (loop (map cdr data)))))
  (display "stop_\n"))



