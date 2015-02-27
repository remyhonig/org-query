;; Test prereqtuisites for correct functioning of this package

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
