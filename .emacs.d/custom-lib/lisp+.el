(defun thread:nest-first (expr forms)
  (pcase forms
    ('() expr)
    (`((,(and op (guard (not (eq op 'lambda)))) . ,args) . ,rest)
     (thread:nest-first `(,op ,expr ,@args) rest))
    (`(,op . ,rest) (thread:nest-first `(,op ,expr) rest))))
(defmacro -> (expr &rest forms)
  (thread:nest-first expr forms))

(defun str:join (sep strs) ;from s.el
  (mapconcat 'identity strs sep))

;;; LINKED LISTS with links represented as chained cons-cells
'cons ;; CONStructs a new link, AKA conj, en, add, insert-front
(defalias '1st 'car) ;; peek
(defalias 'rest 'cdr) ;; next, after-1st
(defalias 'empty-p 'null) ;; tests whether its argument is empty
(defalias 'nonempty-p 'consp)
(defun 2nd (lst) (1st (rest lst)))
(defun 1p (lst) (and (nonempty-p lst) (empty-p (rest lst)))) ;; singleton
(defalias 'list:join 'append) ;; concatenate
(defun list:append (&rest args) (list:join (1st (last args 1)) (butlast args)))

;;; generic SEQUENCE operations
;; sequences - ordered collections

(defun fill (n x)
  (cl-loop repeat n collect x))

;; generates the sequence of consecutive integers in a given inclusive range lo..hi
(defun range (lo hi)
  (cl-loop for i from lo to hi collect i))

;; joins each ith element of n lists in turn from left to right and returns a list of the corresponding elements until one list ends
;; seqs - a seq of seqs, which are all assumed to have the same number of elements
(defun zip (seqs)
  (if (seq-contains seqs ())
      ()
     ;combine the of the first element from each sequence with
     (cons (mapq 1st seqs)
	   ;the result of combining the list of all elements to the right
	   (zip (mapq rest seqs)))))

;; lists all consecutive pairs
(defun adj (seq)
  (zip (list seq (next seq))))

;; unnests a nested seq by 1 level
(defun flatten (seq)
  (seq-reduce list:join seq ()))

;;; ITERATION

(defmacro mapq (fn seq) `(mapcar ',fn ,seq))

; TODO: additional clauses
; - where
; - except
; - <-, after
; - where, such that
; - indexed
; - binding
; - skip
; - step <var> <init> <test> <next>, by
; - collection types - last, do, max, sum
; - default clauses
(defun collect:isolate-first-clause (clauses)
  (pcase clauses
      ('() (error "Empty comprehension: missing first 'for' clause"))
      (`(for ,<var> ,<rel> ,<src> . ,rest)
       `((for ,<var> ,<rel> ,<src>) ,rest))
      (_ ;the first clause will always be a for-like (generating) clause
       (error "First 'for' clause missing in comprehension"))))
(defun collect:isolate-next-clause (clauses)
  (pcase clauses
    ('() '(end () ()))
    (`(for ,<var> ,<rel> ,<src> . ,rest)
     ;makes the variable take on successive values by iterating as indicated
     `(for  (for ,<var> ,<rel> ,<src>) ,rest))
    (`(and ,<var> ,<rel> ,<src> . ,rest) ;simultaneous iteration
     `(and  (for ,<var> ,<rel> ,<src>) ,rest))
    (`(with ,<var> = ,<val> . ,rest) ;fix a temporary variable
     `(with  (for ,<var> = ,<val>) ,rest))
    (`(when <test> . ,rest) ;filter, select only
     `(when  (when ,<test>) ,rest))
    (`(until <test> . ,rest) ;early termination checks
     `(until  (until ,<test>) ,rest))
    (_ (error "Expecting 'and', 'for', 'when', or 'until' in comprehension"))))
(defun collect:build-loop (expr clauses)
  (seq-let (kind clause rest) (collect:isolate-next-clause clauses)
    (pcase kind
      ('end                         `(collect ,expr))
      ((or 'and 'with 'until 'when) (list:join clause (collect:build-loop expr rest)))
      ('for ;destructively appends the inner loop for the nested case
       `(nconc (cl-loop ,@clause ,@(collect:build-loop expr rest)))))))
(defmacro collect (expr &rest clauses)
  (seq-let (clause rest) (collect:isolate-first-clause clauses)
     `(cl-loop ,@clause ,@(collect:build-loop expr rest))))

;;; in-place storage MODIFICATION

;; set a variable to the result of computing some function on its current value
(defmacro apply! (place function &args)
  `(setq ,place (funcall ,function ,place ,@args)))

;; swap the place back into the first argument
;; like Clojure's anonymous function syntax,
;; uses magic symbol '% as placeholder for argument
(defmacro aswap! (place access-expr)
  `(let ((% ,place))
     (setq ,place ,access-expr)))
