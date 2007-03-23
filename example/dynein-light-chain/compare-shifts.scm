; GCB 20mar07
;
; compare list of bmrb files; pick out largest changes in chemical shift

(use-modules (starparse)
	     (srfi srfi-1)
	     (ice-9 regex)
	     (ice-9 format))



(define (atom-match? atom-1 atom-2)
  (false-if-exception
   (and (equal? (assoc 'Residue_seq_code atom-1)
		(assoc 'Residue_seq_code atom-2))
	(equal? (assoc 'Atom_name atom-1)
		(assoc 'Atom_name atom-2)))))

(define (find-matching-atom atom-1 set)
  (find (lambda(x)(atom-match? atom-1 x)) set))

(define (atom:chemical-shift atom)
  (cdr (assoc 'Chem_shift_value atom)))

(define (atom:atom-name atom)
  (cdr (assoc 'Atom_name atom)))

(define (atom:residue atom)
  (cdr (assoc 'Residue_seq_code atom)))

(define (get-chemical-shift-difference atom-1 set)
  (let ((atom-2 (find-matching-atom atom-1 set)))
    (if (and atom-1 atom-2)
	(- (atom:chemical-shift atom-1)
	   (atom:chemical-shift atom-2))
	#f)))

(define star-file-names
  (let ((dir (opendir "."))
	(result (list)))
    (do ((entry (readdir dir) (readdir dir)))
	((eof-object? entry))
      (if (string-match ".*str" entry)
	  (set! result (cons entry result))))
    (closedir dir)
    result))

(define bmrb-entries (map read-bmrb-shifts star-file-names))

(for-each
 (lambda (atom)
   (format #t "~a ~4a " (atom:residue atom)(atom:atom-name atom))
   (for-each (lambda (x)
	       (if (number? x)
		   (format #t "~7,2f" x)
		   (display "       ")))
	     (map (lambda (entry)
		    (get-chemical-shift-difference atom entry)) bmrb-entries))
   (newline))
 (car bmrb-entries))













