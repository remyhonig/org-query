;; Headline matching

(xt-deftest org-query-test-stringmatch
  (xtd-return= (org-query-test-do 't (org-query-stringmatch "-Inbox-"))
               
               ("* -Inbox--!-\n" t)
               ("* TODO new-!-\n" nil)))


(xt-deftest org-query-test-todo
  (xtd-return= (org-query-test-do 't (org-query-todo))
               
               ("* -Inbox--!-\n" nil)
               ("* TODO new-!-\n" t)))


;; Structure walking

(xt-deftest org-query-test-parent
  (xtd-return= (org-query-test-do 't (org-query-parent (org-query-stringmatch "A")))
               
               ("* A\n** C-!-\n" t)
               ("* B\n** C-!-\n" nil)
               ("* A\n** Bn*** C-!-\n" t)))


(xt-deftest org-query-test-child
  (xtd-return= (org-query-test-do 't (org-query-child (org-query-todo)))
               
               ("* A-!-\n** TODO Hoi\n" t)
               ("* A-!-\n** B\n** TODO Hoi\n" t)
               ("* A-!-\n** B\n" nil)))


;; Skipping functions

(xt-deftest org-query-test-skip-headline
  (xt-note "A succesful selection will move the cursor to the end of the current headline")
  (xtd-setup= (org-query-test-do nil
                                 (let ((marker (org-query-skip-headline 'org-query-todo)))
                                   (if marker (goto-char marker))))
              
              ("* TODO A-!-\n** B\n"
               "* TODO A\n-!-** B\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))


(xt-deftest org-query-test-skip-tree
  (xt-note "A succesful selection will move the cursor to the end of the current tree")
  (xtd-setup= (org-query-test-do nil
                                 (let ((marker (org-query-skip-tree 'org-query-todo)))
                                   (if marker (goto-char marker))))
              
              ("* TODO A-!-\n** B\n"
               "* TODO A\n** B-!-\n")
              ("* A-!-\n** B\n* C\n"
               "* A-!-\n** B\n* C\n")))

;; Selection macro

(xt-deftest org-query-test-select-headline
  (xt-note "A succesful selection will not advance the cursor")
  (xtd-setup= (org-query-test-do nil
                                 (let* ((func (org-query-select "headline" (org-query-todo)))
                                        (marker (funcall func)))
                                   (if marker (goto-char marker))))
              
              ("-!-* TODO A\n** B\n"
               "-!-* TODO A\n** B\n")
              ("* A-!-\n** B\n* C\n"
               "* A\n-!-** B\n* C\n")))
