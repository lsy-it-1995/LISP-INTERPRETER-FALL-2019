;; You need to write this one.
(defun my-assoc (v alist)
    (cond ( (null alist) nil)
        ((eq (caar alist) v)
            (car alist)
        )
        (T(my-assoc v (cdr alist)))
    )
)


;; This one is done
(defun my-eval (e alist)
    (cond ((atom e) (my-eval-atom e alist))
          (t (my-apply (car e) (cdr e) alist))
    )
)

;; You need to write this one.
(defun my-eval-atom (e alist)
    (cond
        ((null e) nil)
	((eq e T) T)
        ((symbolp e)
            (cdr (my-assoc e alist))
        )
        (t e)
    )
)

;; This one is done, but you must write the functions it calls
(defun my-apply (fn args alist)
    (cond ((atom fn) (my-apply-atom fn args alist))
          ( t (my-apply-lambda fn args alist)))
)

;; You need to write this one.
;; Utility function for eval-cond and apply-lambda.  Evaluates each expression
;; in l and returns the value of the last expression
(defun my-eval-list (l alist)
	(cond
		((null l) nil)
		((null (cdr l)) 
			(my-eval (car l) alist))
		(t(my-eval (car l) alist) (my-eval-list (cdr l) alist))
	)
)

;; You need to write this one.
(defun my-apply-lambda (fn args alist)
;; bind the formals to the evaluated actuals then evaluate the body in that
;; new scoping context (i.e., that becomes the new alist for recursive
;; evaluation of the function body.  Return the value of the last 
;; expression in the body (using eval-list).
)

;; You need to write this one.
(defun my-bind-formals (formals actuals alist)
;; This takes a list of formals and unevaluated actuals.  It should evaluate
;; each actual and bind it to its corresponding formal placing them all on
;; the front of the alist.  It should return the alist with the new bindings
;; on the front.  This will be used to evaluate calls to functions defined
;; via defun.
;; e.g., (my-bind-formals '(a) '((add 1 b)) '((b . 10)))
;; will return ((a . 11) (b . 10))
;; Note there will be one actual parameter for each formal parameter.

)

;; You need to write this one.  Handle the primitives as special cases, then
;; handle user defined functions (defined via defun) in the default case.
;; These are the only functions we handle: eq, car, cdr, cons, quote, cond,
;; defun, eval, setq, and user defined functions (defined via defun) that
;; we have evaluated.  You can add more built-ins (like plus, times, atom,
;; listp) as you like for testing.

(defun my-apply-atom (fn args alist)
    (cond ((eq fn 'eq)
           (eq (my-eval (car args) alist) (my-eval (cadr args) alist)))
          ;; I wrote the first one, eq, for you, you write the rest
          ((eq fn 'car)
		(car (my-eval (car args) alist))
          )
          ((eq fn 'cdr)
		(cdr (my-eval (car args) alist))
          )
          ((eq fn 'cons)
		(cons (my-eval (car args) alist) (my-eval (cadr args) alist))
          )
          ((eq fn 'quote)
		(car args)
          )
          ((eq fn 'setq) (my-eval-setq (car args) (my-eval (cadr args) alist)))
          ;; these are (nearly) done, but you must write the sub-functions
          ((eq fn 'cond) (my-eval-cond args alist))
          ((eq fn 'defun) (my-eval-defun args alist))
          ((eq fn 'eval) (my-eval (my-eval (car args) alist) alist))
          (T (my-apply ;; get the lambda from the alist, 
                       args alist))
    )
)

(setq global-alist nil)
(defun my-eval-setq (var val)
	(setq global-alist (cons (cons var val) global-alist))
)

;; You need to write this one.  You should know how cond works at this point.
(defun my-eval-cond (clauses alist)
(write-line "my-eval-cond")
(write clauses)
(write alist)
	(cond
		((null clauses)nil)
		(car clauses)
		(T(cdr clauses) alist)
	)
)

;; You need to write this one.
(defun my-eval-defun (body alist)
;; just push the function body onto the global alist.  It is already an
;; association, e.g., (equal (L1 L2) (cond (...))) and (assoc 'equal in
;; the global alist will return this.  etq global-alist nil)
;; have a list containing the formal parameters and the expressions in
;; the function body.
	(setq global-alist (cons body global-alist))
)

(defun my-top ()
    (prog ()
        top (print (my-eval (read) global-alist))
            (terpri) ;; prints a newline
            (go top) ;; loops forever
    )
)
