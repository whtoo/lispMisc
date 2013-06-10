(defun make-tree (label left right)
	(list 'label-binary-tree label left right)
)

(defun is-tree (x)
	(and (listp x) (eq (first x) 'label-binary-tree))
)

(defun tree-label (tree) (second tree))
(defun tree-left (tree) (third tree))
(defun tree-right (tree) (fourth tree))

(defun set-tree-label (tree value)
	(setf (second tree) value)
)

(defun tree-sub (tree old new)
	(if (is-tree tree)
		(progn (if (eq old (tree-label tree))
			(set-tree-label tree new))
			(tree-sub (tree-left tree) old new)
			(tree-sub (tree-right tree) old new)
		)
		)
	)
)