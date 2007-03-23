; GCB 20mar07
;
; compare list of bmrb files; pick out largest changes in chemical shift

(use-modules (starparse)
	     (srfi srfi-1)
	     (ice-9 regex)
	     (ice-9 format))

(define (write-line x)
  (display x)
  (newline))

(define test-atom-name "N")

(define fname-1 (cadr (command-line)))
(define fname-2 (caddr (command-line)))

(define bmrb-1 (read-bmrb-shifts fname-1))
(define bmrb-2 (read-bmrb-shifts fname-2))

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

(define (atom-name-predicate name)
  (lambda (atom)
    (equal? name (atom:atom-name atom))))

(define (atom:residue atom)
  (cdr (assoc 'Residue_seq_code atom)))

(define (atom:residue-name atom)
  (cdr (assoc 'Residue_label atom)))

(define (get-chemical-shift-difference atom-1 set)
  (let ((atom-2 (find-matching-atom atom-1 set)))
    (if (and atom-1 atom-2)
	(- (atom:chemical-shift atom-1)
	   (atom:chemical-shift atom-2))
	#f)))

(define bmrb-1-filtered (filter (atom-name-predicate test-atom-name) bmrb-1))
(define bmrb-2-filtered (map (lambda(x)(find-matching-atom x bmrb-2)) bmrb-1-filtered))

(for-each
 (lambda (atom-1 atom-2)
   (false-if-exception
    (format #t "~a ~a~%"
	    (atom:residue atom-1)
	    (abs (- (atom:chemical-shift atom-1)
		    (atom:chemical-shift atom-2))))))
 bmrb-1-filtered
 bmrb-2-filtered)














