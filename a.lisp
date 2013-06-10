;;insert a pair into a list,and (car pair) as a key;
;;example:(1 3) ((1 2) (2 3))
;;return:((1 2) (2 3))
(defparameter *lst* '((1 2) (2 3)))

(defun insert (pair lst)
	(cond ((null lst) (cons pair ()))
		((= (car pair) (car (car lst))) lst)
		((< (car pair) (car (car lst))) (cons pair lst))
		((t (cons (car lst) (insert pair (cdr lst)))))
	)
)

(insert '(3 4) *lst*)
;;return ((1 2) (2 3) (3 4))