(require 'org-query)
(require 'xtest)

(defmacro org-query-test-do (with-seq-todo body)
  `(lambda (_)
     (if ,with-seq-todo
         (save-excursion
           (goto-char (point-min))
           ;; Without this in the buffer org-mode will not recognize NEXT as a todo state.
           (insert "#+SEQ_TODO: TODO NEXT | DONE\n")))
     (org-mode)
     (setq org-query-test-result (,@body))
     org-query-test-result))

(defvar org-query-test-kwds "#+SEQ_TODO: TODO NEXT | DONE")

(xt-deftest org-query-test-if-is-backlog-task-in-active-project
  (xt-note "A TODO task in an active project. Subprojects are not tasks.")
  (xtd-return= (org-query-test-do 't
                (org-query-if-project-task-p '("NEXT") '("TODO")))
               ("* NEXT A\n** TODO B-!-\n" t)
               ;; Subprojects are not tasks so don't match
               ("* NEXT A\n** TODO B-!-\n*** TODO C\n" nil)
               ("* TODO A\n** TODO B-!-\n" nil)))

(xt-deftest org-query-test-if-ancestor-p
  (xt-note "Any ancestor of the current headline should match the condition.")
  (xtd-return= (org-query-test-do 't
                (org-query-if-ancestor-p (apply-partially 'org-query-if-headline-matches-p "A")))
               ("* A\n** C-!-\n" t)
               ("* B\n** C-!-\n" nil)
               ("* A\n** Bn*** C-!-\n" t)))

(xt-deftest org-query-test-inbox
  (xt-note "Any headline in [Inbox] should match wether it is a task or not.")
  (xtd-return= (org-query-test-do 't
                (org-query-if-ancestor-p (apply-partially 'org-query-if-headline-matches-p "-Inbox-")))
               ("* -Inbox-\n** TODO new-!-\n" t)
               ("* Other\n** TODO new-!-\n" nil)
               ("* -Inbox-\n** not a todo-!-\n" t)))

(xt-deftest org-query-test-if-is-project
  (xt-note "A project needs to have subtasks and a todo state of NEXT or TODO.")
  (xtd-return= (org-query-test-do 't (org-query-if-project-p))
               ("* TODO A-!-\n** TODO B\n" t)
               ("* TODO A-!-\n** B\n" nil)
               ("* A-!-\n** B\n" nil)))

(xt-deftest org-query-test-if-is-inactive-project
  (xt-note "An inactive project has a TODO todo state.")
  (xtd-return= (org-query-test-do 't (org-query-if-project-p '("TODO")))
               ("* NEXT A-!-\n** TODO B\n" nil)
               ("* TODO A-!-\n** TODO B\n" t)))

(xt-deftest org-query-test-if-is-active-project
  (xt-note "An active project has a NEXT todo state.")
  (xtd-return= (org-query-test-do 't (org-query-if-project-p '("NEXT")))
               ("* -!-NEXT A\n** TODO B\n" t)
               ("* TODO A-!-\n** TODO B\n" nil)))

(xt-deftest org-query-test-if-child-is-task
  (xt-note "Does any child of the current headline satisfy the condition.")
  (xtd-return= (org-query-test-do 't
                 (org-query-if-child-p (apply-partially 'org-query-if-headline-matches-p "Hoi")))
               ("* TODO A-!-\n** Hoi\n" t)
               ("* TODO A-!-\n** B\n" nil)))

(xt-deftest org-query-test-org-end-of-subtree
  (xt-note "")
  (xtd-setup= (org-query-test-do nil (org-end-of-subtree))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n** B-!-\n")))

(xt-deftest org-query-test-org-next-headline
  (xt-note "")
  (xtd-setup= (org-query-test-do nil (outline-next-heading))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n-!-** B\n")))

(xt-deftest org-query-test-skip-headline
  (xt-note "A succesful selection will move the cursor to the end of the current headline")
  (xtd-setup= (org-query-test-do nil
               (let ((marker (org-query-skip-headline 'org-query-if-todo-p)))
                  (if marker (goto-char marker))))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n-!-** B\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))

(xt-deftest org-query-test-skip-tree
  (xt-note "A succesful selection will move the cursor to the end of the current tree")
  (xtd-setup= (org-query-test-do nil
               (let ((marker (org-query-skip-tree 'org-query-if-todo-p)))
                  (if marker (goto-char marker))))
              ("* TODO A-!-\n** B\n"
               "* TODO A\n** B-!-\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))

(xt-deftest org-query-test-select-headline
  (xt-note "A succesful selection will not advance the cursor")
  (xtd-setup= (org-query-test-do nil
               (let* ((func (org-query-select "headline" (org-query-if-todo-p)))
                      (marker (funcall func)))
                 (if marker (goto-char marker))))
              ("-!-* TODO A\n** B\n"
               "-!-* TODO A\n** B\n")
              ("* A-!-\n** B\n* C\n"
               "* A\n-!-** B\n* C\n")))
