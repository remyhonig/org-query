
;; (org-data nil
;;           (headline (
;;                      :raw-value "-Inbox-"
;;                      :begin 1
;;                      :end 11
;;                      :pre-blank 0
;;                      :hiddenp nil
;;                      :contents-begin nil
;;                      :contents-end nil
;;                      :level 1
;;                      :priority nil
;;                      :tags nil
;;                      :todo-keyword nil
;;                      :todo-type nil
;;                      :post-blank 0
;;                      :footnote-section-p nil
;;                      :archivedp nil
;;                      :commentedp nil
;;                      :quotedp nil
;;                      :CATEGORY nil
;;                      :title (#("-Inbox-" 0 7 (:parent #1)))
;;                      :parent #0)))

(defun org-query-tree-map-visit-headline (evaluator headline ancestor-flags)
  (let* ((fn-flag-self (plist-get algo :self))
           (fn-parent-evaluation (plist-get algo :parents))
           (fn-flag-children (lambda (child) (plist-get child :flags)))
           (fn-child-flags (plist-get algo :children))
           (fn-final-evaluation (plist-get algo :when))
           (algo-name (plist-get algo :what))

           ;; flags collected by looking at the current element in the tree
           (flags (funcall fn-flag-self tree))
           ;; all flags collected from all ancestors
           (current-ancestor-flags (-distinct (-flatten (-concat ancestor-flags flags))))
           ;; the function that recursively traverses all children
           (fn-map-children (lambda (x) (org-query-tree-map algo x current-ancestor-flags)))

           ;; children of the current headline
           (headline-children (nthcdr 2 tree))
           ;; the result of the tree emap for the children
           (children (mapcar fn-map-children headline-children))
           ;; the collected flags of ALL children
           (result-children-flags (-distinct (-flatten (mapcar fn-flag-children children))))
           
           ;; the flags that are set on the current result depending on the children
           (children-flags (funcall fn-child-flags result-children-flags))

           ;; an early escape evaluation function (not used yet)
           (is-ancestor-flags-satisfied (funcall fn-parent-evaluation (-flatten ancestor-flags)))

           ;; the structure available to the final evaluation function
           (self `(
                   :raw-value
                   ,(org-element-property :raw-value tree)
                   :todo-keyword
                   ,(org-element-property :todo-keyword tree)
                   :children-flags
                   ,children-flags
                   :flags
                   ,flags
                   :ancestor-flags
                   ,(-flatten ancestor-flags)
                   :is-ancestor-flags-satisfied
                   ,is-ancestor-flags-satisfied))

           ;; does the current result satisfy the final evaluation?
           (satisfied (consp (funcall fn-final-evaluation self)))

           ;; create the result with name of matched algorithm
           (result (plist-put self :satisfies (when satisfied algo-name))))

      ;; return the children within the current result to create a tree structure
      (append result children)))

(defun org-query-tree-map (algo tree &rest ancestor-flags)
  "Apply FN to each element of TREE while preserving the tree structure."
  ;;(message "ancestor-flags: %S" ancestor-flags)
  (cond
   ((not tree) nil)
   ((nlistp tree) nil)
   ((and (listp tree) (equal (symbol-name (car tree)) "org-data"))
    (mapcar (lambda (x) (org-query-tree-map algo x)) (nthcdr 3 tree)))
   ((and (listp tree) (equal (symbol-name (car tree)) "headline"))
    (org-query-tree-map-visit-headline algo tree ancestor-flags))
   (t nil)))


(setq active-project

      '(:self ;; return flags
        (lambda (headline)
          (cond
           ((member (org-element-property :todo-keyword headline) '("NEXT")) '(is-next))
           ((member (org-element-property :todo-keyword headline) '("TODO")) '(is-todo))))
        
        :parents ;; return boolean
        (lambda (flags)
          (message "flags: %S" flags)
          (or (consp (member 'is-todo flags)) (consp (member 'is-next flags))))
        
        :children ;; return flags
        (lambda (flags)
          (message "children flags: %S" flags)
          (when (member 'is-next flags) '(has-next-task)))
        
        :when ;; return boolean
        (lambda (result) (message "result %S" result)
          (and
           (member 'has-next-task (plist-get result :children-flags))
           (member 'is-next (plist-get result :flags))))

        :what "active project"))

(xt-deftest org-query-test-analyse
            (xtd-return=
             (org-query-element-test-map t active-project)
               
             ("#+STARTUP: align\n* TODO A\n** TODO B\n** NEXT C\n*** NEXT D\n"
              
              '((:raw-value "A" :todo-keyword "TODO" :children-flags
                            (has-next-task)
                            :flags
                            (is-todo)
                            :ancestor-flags nil :is-ancestor-flags-satisfied nil :satisfies nil
                            (:raw-value "B" :todo-keyword "TODO" :children-flags nil :flags
                                        (is-todo)
                                        :ancestor-flags
                                        (is-todo)
                                        :is-ancestor-flags-satisfied t :satisfies nil)
                            (:raw-value "C" :todo-keyword "NEXT" :children-flags
                                        (has-next-task)
                                        :flags
                                        (is-next)
                                        :ancestor-flags
                                        (is-todo)
                                        :is-ancestor-flags-satisfied t :satisfies "active project"
                                        (:raw-value "D" :todo-keyword "NEXT" :children-flags nil :flags
                                                    (is-next)
                                                    :ancestor-flags
                                                    (is-todo is-next)
                                                    :is-ancestor-flags-satisfied t :satisfies nil))))
              )))
