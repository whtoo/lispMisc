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

;;search a symbol in a intented list.
;;if list is sperated recusively to a symbol,just use symbolp.
;;else recusive work.
(defun searchSymbol (symbolVar expression)
	(cond ((null expression) nil)
		((symbolp expression) (eq expression symbolVar))
		(t (or (searchSymbol symbolVar (first expression))
				(searchSymbol symbolVar (rest expression)))
		)
	)
)

;;find-if  x equal first of one pair in pairs then return the pair.
;;else-if x is less than one of pairs then return the two pairs from the bigger pair and its former pair.
(defun nearPair (x pairs)
	   (cond ((null pairs) nil)
		 ((eq x (first (first pairs))) (list (first pairs)))
		 ((or (< x (first (first pairs)))
		     (null (rest pairs))) ())
		 ((< x (first (second pairs)))
		  (list (first pairs) (second pairs))
		  )
		 (t (nearPair x (rest pairs)))))