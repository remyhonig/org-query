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

(defmacro org-query-element-test-map (with-seq-todo body)
  `(lambda (_)
     (if ,with-seq-todo
         (save-excursion
           (goto-char (point-min))
           ;; Without this in the buffer org-mode will not recognize NEXT as a todo state.
           (insert "#+SEQ_TODO: TODO NEXT | CANCELLED DONE\n")))
     (org-mode)
     (setq --map (org-element-parse-buffer))
     (message "%S" --map)
     (setq org-query-test-result (org-query-tree-map ,body --map))
     (message "%S" org-query-test-result)
     org-query-test-result))
