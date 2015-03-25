(xt-deftest org-query-test-gtd-project
  (xt-note "A project needs to have subtasks and a todo state of NEXT or TODO.")
  (xtd-return= (org-query-test-do t (org-query-gtd-project))
               ("* TODO A-!-\n** TODO B\n" t)
               ("* NEXT A-!-\n** TODO B\n" t)
               ("* TODO A-!-\n** B\n" nil)
               ("* A-!-\n** B\n" nil)))


(xt-deftest org-query-test-gtd-active-project
  (xt-note "An active project has a NEXT todo state.")
  (xtd-return= (org-query-test-do t (org-query-gtd-active-project))
               ("* -!-NEXT A\n** TODO B\n" t)
               ("* TODO A-!-\n** TODO B\n" nil)))


(xt-deftest org-query-test-gtd-backlog-task
  (xt-note "An active project has a NEXT todo state.")
  (xtd-return= (org-query-test-do t (org-query-gtd-backlog-task))
               ("* NEXT A\n** CANCELLED B\n*** TODO C-!-\n" nil)
               ("* NEXT A\n** TODO B-!-\n" t)
               ("* TODO A\n** TODO B-!-\n" nil)))


