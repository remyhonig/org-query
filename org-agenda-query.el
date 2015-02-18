(defun rmh/if-ancestor-p (func)
  "Is any ancestor t for FUNC."
  (let ((result nil))
    (save-excursion
      (while (and (not result) (org-up-heading-safe))
        (setq result (funcall func))))
    result))

(defun rmh/if-headline-matches-p (match)
  "Is headline at point a MATCH."
  (not (null (string-match match (nth 4 (org-heading-components))))))

(defun rmh/is-parent-headline-p (match)
  "Does parent headline value MATCH."
  (rmh/if-ancestor-p (apply-partially 'rmh/if-headline-matches-p match)))

(defun rmh/if-child-p (func)
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

(defun rmh/if-todo-p (&optional todo-states)
  "Is headline a task and is it's todo kwd one of TODO-STATES?"
  (setq match-states (cond (todo-states todo-states) (t org-todo-keywords-1)))
  ;(message "if-todo-p match %S on %S" match-states (org-heading-components))
  (not (null (member (org-get-todo-state) match-states))))

(defun rmh/if-project-p (&optional todo-states)
  "Is headline a project and is it's todo kwd one of TODO-STATES?"
  (setq match-states (cond (todo-states todo-states) (t org-todo-keywords-1)))
  ;(message "if-project-p match %S on %S" match-states (org-heading-components))
  (and
   ; is headline a task
   (rmh/if-todo-p match-states)
   ; does this headline have any tasks in any state
   (rmh/if-child-p 'rmh/if-todo-p)))

(defun rmh/if-project-task-p (&optional project-todo-states task-todo-states)
  "Is headline a task in a project?
PROJECT-TODO-STATES optional list of todo states the project should be in
TASK-TODO-STATES optional list of todo states the task should be in"
  (setq match-project-states (cond (project-todo-states project-todo-states) (t org-todo-keywords-1)))
  (setq match-task-states (cond (task-todo-states task-todo-states) (t org-todo-keywords-1)))
  (and
   ; headline should be a todo 
   (rmh/if-todo-p match-task-states)
   ; should not have a child; otherwise i'd be a project
   (not (rmh/if-child-p 'rmh/if-todo-p))
   ; is one it's ancestors a task
   (rmh/if-ancestor-p (apply-partially 'rmh/if-todo-p match-project-states))))

(defun rmh/skip-headline (func)
  "Return marker to next headline if FUNC satisfies"
  (save-restriction
    (widen)
    (let ((marker (save-excursion (or (outline-next-heading) (point-max)))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defun rmh/skip-tree (func)
  "Return marker to next headline if FUNC satisfies"
  (save-restriction
    (widen)
    (let ((marker (save-excursion (org-end-of-subtree t))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defmacro rmh/select (resolution body)
  "Create skip-function with condition in FUNC."
  ()
  (let ((skipfunc (intern (concat "rmh/skip-" resolution))))
    `(lambda ()
       (,skipfunc
        (lambda () (not (,@body)))))))

(require 'xtest)

(xt-deftest rmh-test/if-is-backlog-task-in-active-project
  (xt-note "A TODO task in an active project. Subprojects are not tasks.")
  (xtd-return= (lambda (_) (org-mode)
                 (setq org-todo-keywords-1 '("TODO" "NEXT"))
                 (rmh/if-project-task-p '("NEXT") '("TODO")))

               ("* NEXT A\n** TODO B-!-\n" t)
               ;; Subprojects are not tasks so don't match
               ("* NEXT A\n** TODO B-!-\n*** TODO C\n" nil)
               ("* TODO A\n** TODO B-!-\n" nil)))

(xt-deftest rmh-test/if-ancestor-p
  (xt-note "Any ancestor of the current headline should match the condition.")
  (xtd-return= (lambda (_) (org-mode)
                 (rmh/if-ancestor-p (apply-partially 'rmh/if-headline-matches-p "A")))
              
               ("* A\n** C-!-\n" t)
               ("* B\n** C-!-\n" nil)
               ("* A\n** Bn*** C-!-\n" t)))

(xt-deftest rmh-test/inbox
  (xt-note "Any headline in [Inbox] should match wether it is a task or not.")
  (xtd-return= (lambda (_) (org-mode)
                 (rmh/if-ancestor-p (apply-partially 'rmh/if-headline-matches-p "-Inbox-")))
              
               ("* -Inbox-\n** TODO new-!-\n" t)
               ("* Other\n** TODO new-!-\n" nil)
               ("* -Inbox-\n** not a todo-!-\n" t)))

(xt-deftest rmh-test/if-is-project
  (xt-note "A project needs to have subtasks and a todo state of NEXT or TODO.")
  (xtd-return= (lambda (_) (org-mode)
                 (setq org-todo-keywords-1 '("TODO"))
                 (rmh/if-project-p))
              
               ("* TODO A-!-\n** TODO B\n" t)
               ("* TODO A-!-\n** B\n" nil)
               ("* A-!-\n** B\n" nil)))

(xt-deftest rmh-test/if-is-inactive-project
  (xt-note "An inactive project has a TODO todo state.")
  (xtd-return= (lambda (_) (org-mode)
                 (setq org-todo-keywords-1 '("TODO" "NEXT"))
                 (rmh/if-project-p '("TODO")))
              
               ("* NEXT A-!-\n** TODO B\n" nil)
               ("* TODO A-!-\n** TODO B\n" t)))

(xt-deftest rmh-test/if-is-active-project
  (xt-note "An active project has a NEXT todo state.")
  (xtd-return= (lambda (_) (org-mode)
                 (setq org-todo-keywords-1 '("TODO" "NEXT"))
                 (rmh/if-project-p '("NEXT")))
              
               ("* NEXT A-!-\n** TODO B\n" t)
               ("* TODO A-!-\n** TODO B\n" nil)))

(xt-deftest rmh-test/if-child-is-task
  (xt-note "Does any child of the current headline satisfy the condition.")
  (xtd-return= (lambda (_) (org-mode)
                 (setq org-todo-keywords-1 '("TODO"))
                 (rmh/if-child-p (apply-partially 'rmh/if-headline-matches-p "Hoi")))
              
               ("* TODO A-!-\n** Hoi\n" t)
               ("* TODO A-!-\n** B\n" nil)))

(xt-deftest rmh-test/org-end-of-subtree
  (xt-note "")
  (xtd-setup= (lambda (_) (org-mode) (org-end-of-subtree))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n** B-!-\n")))

(xt-deftest rmh-test/org-next-headline
  (xt-note "")
  (xtd-setup= (lambda (_) (org-mode) (outline-next-heading))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n-!-** B\n")))

(xt-deftest rmh-test/skip-headline
  (xt-note "")
  (xtd-setup= (lambda (_) (org-mode)
                (let ((marker (rmh/skip-headline 'rmh/if-todo-p)))
                  (if marker (goto-char marker))))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n-!-** B\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))

(xt-deftest rmh-test/skip-tree
  (xt-note "")
  (xtd-setup= (lambda (_) (org-mode)
                (let ((marker (rmh/skip-tree 'rmh/if-todo-p)))
                  (if marker (goto-char marker))))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n** B-!-\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))

(provide 'rmh-agenda-functions)
