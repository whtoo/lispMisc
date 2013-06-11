;;init a tree instance symbol
(defparameter tree nil)
;;return a tree instance
(defun make-tree (labelname val children)
	(list 'tree labelname val children)
)
;;get methods for tree instance
(defun tree-label (tree) (second tree))
(defun tree-value (tree) (third tree))
(defun tree-children (tree) (fourth tree))
(defun tree-print (tree) (princ (tree-label tree)))
;;init a test instance
(setq tree
	(make-tree 'a 6 (list (make-tree 'b 3
							(list (make-tree 'd 5 nil)
								(make-tree 'e 4 nil)))
						(make-tree 'c 1
							(list (make-tree 'f 2 nil)
								(make-tree 'g 0 nil)))))
)
;;dfs method
(defun dfs (nodes goalp next)
	(cond ((null nodes) nil)
	;;return the first node if it is a goal node
		((funcall goalp (first nodes)) (first nodes))
		(t (progn 
			(format t "~{ ~a ~% ~}" (append (funcall next (first nodes))
							(rest nodes)))
			(dfs (append (funcall next (first nodes))
							(rest nodes))
				goalp
				next)
		))))
;;test dfs
(dfs (list tree)
	#'(lambda (x) (eq 'g (tree-label x)))
	#'tree-children)
	