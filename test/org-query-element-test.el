
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


(defun org-query-tree-map-visit-headline-self (algo headline)
  (let* ((fn-flag-self (plist-get algo :self)))
    (funcall fn-flag-self headline)))


(defun org-query-tree-map-visit-headline-children (algo children flags ancestor-flags)
  (let* ((current-ancestor-flags (-distinct (-flatten (-concat ancestor-flags flags))))
         (fn-map-children (lambda (x) (org-query-tree-map algo x current-ancestor-flags))))
    (mapcar fn-map-children children)))


(defun org-query-tree-map-visit-headline-children-flags (algo children-results)
  (let* ((fn-flag-children (lambda (child) (plist-get child :flags)))
         (fn-child-flags (plist-get algo :children))
         (result-children-flags (-distinct (-flatten (mapcar fn-flag-children children-results)))))
    (funcall fn-child-flags result-children-flags)))


(defun org-query-tree-map-visit-headline-eval-parent (algo ancestor-flags)
  (let* ((fn-parent-evaluation (plist-get algo :parents)))
    (funcall fn-parent-evaluation (-flatten ancestor-flags))))


(defun org-query-tree-map-visit-headline-eval-final (algo self)
  (let* ((fn-final-evaluation (plist-get algo :eval)))
    (funcall fn-final-evaluation self)))


(defun org-query-tree-map-visit-headline (evaluator headline children ancestor-flags)
  (let* (;; flags collected by looking at the current element in the tree
         (flags (org-query-tree-map-visit-headline-self algo tree))

         ;; an early escape evaluation function (not used yet)
         (is-ancestor-flags-satisfied (org-query-tree-map-visit-headline-eval-parent algo ancestor-flags))

         ;; the result of the tree emap for the children
         (children-results (org-query-tree-map-visit-headline-children algo children flags ancestor-flags))

         ;; the flags that are set on the current result depending on the children
         (children-flags (org-query-tree-map-visit-headline-children-flags algo children-results))

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
         (tag (org-query-tree-map-visit-headline-eval-final algo self))

         ;; create the result with name of matched algorithm
         (result (plist-put self :satisfies tag)))

      ;; return the children within the current result to create a tree structure
      (append result children-results)))


(defun org-query-tree-map (algo tree &rest ancestor-flags)
  "Apply FN to each element of TREE while preserving the tree structure."
  ;;(message "ancestor-flags: %S" ancestor-flags)
  (cond
   ((not tree) nil)
   ((nlistp tree) nil)
   ((and (listp tree) (equal (symbol-name (car tree)) "org-data"))
    (mapcar (lambda (x) (org-query-tree-map algo x)) (nthcdr 3 tree)))
   ((and (listp tree) (equal (symbol-name (car tree)) "headline"))
    (let* ((children (nthcdr 2 tree))
           (result (org-query-tree-map-visit-headline algo tree children ancestor-flags)))
      result))
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
        
        :eval ;; return boolean
        (lambda (result) (message "result %S" result)
          (when (and
                 (member 'has-next-task (plist-get result :children-flags))
                 (member 'is-next (plist-get result :flags)))
            "active project"))))


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
