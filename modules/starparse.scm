
(define-module (starparse)
  #:export (star-parse
	    bmrb->hash
	    bmrb->alist
	    make-assignment-set
	    alist:write-bmrb-block
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

;; Names from the BMRB definition of v.3:
; Atom_chem_shift.ID
; Atom_chem_shift.Assembly_atom_ID
; Atom_chem_shift.Entity_assembly_ID
; Atom_chem_shift.Entity_ID
; Atom_chem_shift.Comp_index_ID
; Atom_chem_shift.Seq_ID  	
; Atom_chem_shift.Comp_ID  	
; Atom_chem_shift.Atom_ID  	
; Atom_chem_shift.Atom_type  	
; Atom_chem_shift.Atom_isotope_number
; Atom_chem_shift.Val  
; Atom_chem_shift.Val_err
; Atom_chem_shift.Assign_fig_of_merit
; Atom_chem_shift.Ambiguity_code  	
; Atom_chem_shift.Occupancy  	
; Atom_chem_shift.Resonance_ID  	
; Atom_chem_shift.Auth_entity_assembly_ID 
; Atom_chem_shift.Auth_seq_ID  	
; Atom_chem_shift.Auth_comp_ID  	
; Atom_chem_shift.Auth_atom_ID  	
; Atom_chem_shift.Details  	
; Atom_chem_shift.Sf_ID  	
; Atom_chem_shift.Entry_ID
; Atom_chem_shift.Assigned_chem_shift_list_ID

;; map NMR-Star v.2 names to standard NMR-Star v.3 names
(define (ensure-star-v3 name)
  (case name
    ((Residue_seq_code Atom_chem_shift.Auth_seq_ID)
     'Atom_chem_shift.Seq_ID)
    ((Atom_name Atom_chem_shift.Atom_ID Atom_chem_shift.Auth_atom_ID)
     'Atom_chem_shift.Atom_ID)
    ((Chem_shift_value Atom_chem_shift.Val)
     'Atom_chem_shift.Val)
    (else name)))

(define (entry-cleanup entry)
  (let ((key (car entry))
	(value (cdr entry)))
    (define new-value
      (if (number? value)
	  (if (integer? value)
	      (inexact->exact value)
	      value)
	  (string->symbol value)))
    (cons (ensure-star-v3 key)
	  new-value)))

; Partition 'my-alist' into sublists such that
; each sublist has no redundant keys.
; i.e. group raw bmrb alist into per-assignment groups
(define (group-assignments my-alist)
  (let loop ((result (list))
	     (current (list))
	     (rest my-alist))
    (define (entry-present? name)
      (assoc name current))
    (if (null? rest)
	(if (null? current)
	    result
	    (cons current result))
	(if (entry-present? (caar rest))
	    (loop (cons current result)
		  (list)
		  rest)
	    (loop result
		  (cons (car rest) current)
		  (cdr rest))))))

(define (bmrb->alist fname)
  (define raw-alist
    (let ((result '()))
      (star-parse fname #f
		  (lambda (name value)
		    (set! result
			  (cons (cons name value) result))))
      result))
  (group-assignments (map entry-cleanup raw-alist)))

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

(define (exact-integer? x)
  (and (integer? x)
       (exact? x)))

;
; Format 'datum' for inclusion in a star file:
; column width of 8 characters;
; default entry of '.' if datum is false
;
(define (format-star-datum datum)
  (define format-string
    (cond ((exact-integer? datum) "~8d ")
	  ((number? datum) "~8f ")
	  (else "~8a ")))
  (if datum
      (format #f format-string datum)
      ".        "))
;
; Print on the default output stream the elements of
; the list of alists 'data' in Star format as a loop
; with tags given by 'fields'
;
(define (write-bmrb-loop data fields)
  ;; write header
  (display "loop_")
  (newline)
  (for-each (lambda(f)
	      (display "_")
	      (display f)
	      (newline))
	    fields)
  ;; write body
  (for-each
   (lambda (datum)
     (for-each
      (lambda (field)
	(display (format-star-datum (assoc-ref datum field))))
      fields)
     (newline))
   data)
  (display "stop_")
  (newline))

;
; Write a full bmrb data block, using the keys from
; the first entry in my-alist as the loop elements
;
(define (alist:write-bmrb-block name my-alist)
  (let ((fields (map car (car my-alist))))
    (format #t "data_~a~%" name)
    (write-bmrb-loop my-alist fields)))

