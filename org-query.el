(defun org-query-if-ancestor-p (func)
  "Is any ancestor t for FUNC."
  (let ((result nil))
    (save-excursion
      (while (and (not result) (org-up-heading-safe))
        (setq result (funcall func))))
    result))

(defun org-query-if-headline-matches-p (match)
  "Is headline at point a MATCH."
  (not (null (string-match match (nth 4 (org-heading-components))))))

(defun org-query-is-parent-headline-p (match)
  "Does parent headline value MATCH."
  (org-query-if-ancestor-p (apply-partially 'org-query-if-headline-matches-p match)))

(defun org-query-if-child-p (func)
  "Is any child satisfying FUNC."
  (let ((satisfied)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (forward-line 1)
      (while (and (not satisfied)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (not (null (funcall func)))
          (setq satisfied t))))
    satisfied))

(defun org-query-if-todo-p (&optional todo-states)
  "Is headline a task and is it's todo kwd one of TODO-STATES?"
  (setq match-states (cond (todo-states todo-states) (t org-todo-keywords-1)))
                                        ;(message "if-todo-p match %S on %S" match-states (org-heading-components))
  (not (null (member (org-get-todo-state) match-states))))

(defun org-query-if-project-p (&optional todo-states)
  "Is headline a project and is it's todo kwd one of TODO-STATES?"
  (setq match-states (cond (todo-states todo-states) (t org-todo-keywords-1)))
                                        ;(message "if-project-p match %S on %S" match-states (org-heading-components))
  (and
                                        ; is headline a task
   (org-query-if-todo-p match-states)
                                        ; does this headline have any tasks in any state
   (org-query-if-child-p 'org-query-if-todo-p)))

(defun org-query-if-project-task-p (&optional project-todo-states task-todo-states)
  "Is headline a task in a project?
PROJECT-TODO-STATES optional list of todo states the project should be in
TASK-TODO-STATES optional list of todo states the task should be in"
  (setq match-project-states (cond (project-todo-states project-todo-states) (t org-todo-keywords-1)))
  (setq match-task-states (cond (task-todo-states task-todo-states) (t org-todo-keywords-1)))
  (and
                                        ; headline should be a todo 
   (org-query-if-todo-p match-task-states)
                                        ; should not have a child; otherwise i'd be a project
   (not (org-query-if-child-p 'org-query-if-todo-p))
                                        ; is one it's ancestors a task
   (org-query-if-ancestor-p (apply-partially 'org-query-if-todo-p match-project-states))))

(defun org-query-skip-headline (func)
  "Return marker to next headline if FUNC satisfies"
  (save-restriction
    (widen)
    (let ((marker (save-excursion (or (outline-next-heading) (point-max)))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defun org-query-skip-tree (func)
  "Return marker to next headline if FUNC satisfies"
  (save-restriction
    (widen)
    (let ((marker (save-excursion (org-end-of-subtree t))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defmacro org-query-select (resolution body)
  "Create skip-function with condition in FUNC."
  ()
  (let ((skipfunc (intern (concat "org-query-skip-" resolution))))
    `(lambda ()
       (,skipfunc
        (lambda () (not (,@body)))))))

(provide 'org-query)
