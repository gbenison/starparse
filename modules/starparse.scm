
(define-module (starparse)
  #:export (star-parse
	    read-bmrb-shifts
	    bmrb->hash
	    make-assignment-set
	    hash->bmrb)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format))
		     

(load-extension "libstarparse" "starparse_init")

(define (bmrb->hash fname)
  (let ((residues (make-hash-table 31))
	(this-residue #f)
	(atom-value #f)
	(atom-name #f))
    (define (ensure-hash resid)
      (let ((result (hash-ref residues resid)))
	(if result
	    result
	    (hash-set! residues resid (make-hash-table 5)))))
    (star-parse fname #f
		(lambda (name value)
		  (case name
		    ((Residue_seq_code Atom_chem_shift.Auth_seq_ID)
		     (set! this-residue (ensure-hash value)))
		    ((Atom_name Atom_chem_shift.Atom_ID Atom_chem_shift.Auth_atom_ID)
		     (set! atom-name (string->symbol value)))
		    ((Chem_shift_value Atom_chem_shift.Val)
		     (set! atom-value value)))
		  (if (and this-residue atom-name atom-value)
		      (begin (hash-set! this-residue atom-name atom-value)
			     (set! this-residue #f)
			     (set! atom-name #f)
			     (set! atom-value #f)))))
    residues))

(define (even-members lst)
  (if (null? lst)
      (list)
      (let ((rest (if (>= (length lst) 2)
		      (cddr lst)
		      '())))
	(cons (car lst)
	      (even-members rest)))))

(define (odd-members lst)
  (even-members (cdr lst)))

(define (write-all-bmrb-fields element fields)
  (if (null? fields)
      (newline)
      (let ((op  (car fields))
	    (rest (cdr fields)))
	(format #t "~8a " (op element))
	(write-all-bmrb-fields element rest))))

(define (write-bmrb-loop data . fields)
  (let ((tags (even-members fields))
	(ops (odd-members fields)))
    ;; write header
    (display "loop_")
    (newline)
    (for-each (lambda(f)
		(display "_")
		(display f)
		(newline))
	      tags)
    ;; write body
    (for-each
     (lambda (elem)
       (write-all-bmrb-fields elem ops))
     data)
    (display "stop_")
    (newline)))

(define (atom-name->type name)
  (case name
    ((N)        'N)
    ((CA CB CG) 'C)
    ((H)        'H)
    (else       '.)))

(define (atom-name->error name)
  (case (atom-name->type name)
    ((H)     0.02)
    (else    0.05)))

;; assignment-set is a list of (residue . atom) pairs
(define (hash->bmrb hash assignment-set)
  (define (unknown x) ".")
  (define (asg->chemical-shift asg)
    (hash-ref (hash-ref hash (car asg))(cdr asg)))
  (define (format-if-number x)
    (if (number? x)
	(format "~,2f" x)
	"."))
  (display "data_assignments\n")
  (write-bmrb-loop
   (filter (lambda (x)(false-if-exception (asg->chemical-shift x))) assignment-set)
   'Atom_chem_shift.ID               (let ((n 0))(lambda (x)(set! n (+ 1 n)) n))
   'Atom_chem_shift.Auth_seq_ID      car
   'Atom_chem_shift.Auth_comp_ID     unknown
   'Atom_chem_shift.Auth_atom_ID     cdr
   'Atom_chem_shift.Atom_type        (lambda (x)(atom-name->type (cdr x)))
   'Atom_chem_shift.Val              (lambda (x)(format-if-number (asg->chemical-shift x)))
   'Atom_chem_shift.Val_err          (lambda (x)(atom-name->error (cdr x)))
   'Atom_chem_shift.Ambiguity_code   (lambda (x) 1)
   'Atom_chem_shift.Occupancy        (lambda (x) 1)))

(define (make-assignment-set start stop . atoms)
  (let ((residues
	 (map (lambda (x)
		(+ x start))
	      (iota (+ (- stop start) 1)))))
    (apply append
	   (map (lambda(resid)(map (lambda(atom)(cons resid atom)) atoms)) residues))))

;; deprecated in favor of bmrb->hash
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



