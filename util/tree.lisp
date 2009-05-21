;;;; http://nostdal.org/Aromyxo/ ;;;;

(in-package :AmUtil)


(defun treeWalker (tree handler)
  (funcall handler tree)
  (when (listp tree)
    (dolist (node tree)
      (treeWalker node handler))))
(export 'treeWalker)
