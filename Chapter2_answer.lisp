(defparameter forward nil)
;;2.5 answer on ai:theory and pratice
(defun iter (pair lst)
	   (cond ((eq (car lst) (car pair)) (append forward (car (rest pair)) (rest lst)))
		 (t (if (null lst) nil
				(progn 
				  (let ((forward (append forward (list (car lst)))))
					(format t "~{ ~a ~}" forward)
					(iter pair (rest lst))
					)
				)
			)
		)
	)
)