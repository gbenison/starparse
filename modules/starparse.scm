
(define-module (starparse)
  #:export (star-parse
	    read-bmrb-shifts
	    bmrb->hash)
  #:use-module (ice-9 regex))

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



