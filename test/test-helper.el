(load-file "org-query.el")
(load-file "org-query-gtd.el")
(require 'xtest)

(defmacro org-query-test-do (with-seq-todo body)
  `(lambda (_)
     (if ,with-seq-todo
         (save-excursion
           (goto-char (point-min))
           ;; Without this in the buffer org-mode will not recognize NEXT as a todo state.
           (insert "#+SEQ_TODO: TODO NEXT | CANCELLED DONE\n")))
     (org-mode)
     (setq org-query-test-result (,@body))
     org-query-test-result))
