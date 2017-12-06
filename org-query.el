;;; org-query.el --- Create complex but readable skip-functions for org-agenda

;; Copyright (C) 2015  Remy Honig

;; Author           : Remy Honig <remyhonig@gmail.com>
;; Package-Requires : ((org "8.2.7"))
;; URL              : https://github.com/remyhonig/org-query
;; Version          : 20150219.1
;; Keywords         : calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; See https://github.com/remyhonig/org-query for usage.

;;; Code:

(require 'org)


;;;;;;;;;;;;;;;;;;;;;;;
;; structure walking ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defsubst org-query-struct-ancestor (pred)
  "Return non-nil if PRED is non-nil for any ancestor."
  (save-excursion
    (cl-loop while (org-up-heading-safe)
             thereis (funcall pred))))

(defsubst org-query-struct-child (pred)
  "Return non-nil if PRED is non-nil for any child of current heading."
  (save-excursion
    (cl-loop with end = (save-excursion
                          (org-end-of-subtree 'invisible-ok))
             while (and (outline-next-heading)
                        (< (point) end))
             thereis (funcall pred))))

(defsubst org-query-struct-headline (pred)
  "Return non-nil if PRED is non-nil for current heading.
This function is mainly used as a single point to print debug
messages."
  (funcall pred))


;;;;;;;;;;;;;;;;;;;;;;;
;; headline matching ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defsubst org-query-stringmatch (match)
  "Is headline at point a MATCH."
  (not (null (string-match match (nth 4 (org-heading-components))))))

(defsubst org-query-todo (&optional todo-states)
  "Is headline a task and is it's todo kwd one of TODO-STATES?"
  (let ((match-states (cond (todo-states todo-states) (t (mapcar 'car org-todo-kwd-alist)))))
    ;; (message "if-todo-p match %S on %S %S" match-states (org-heading-components) (org-get-todo-state))
    (not (null (member (org-get-todo-state) match-states)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; skipping functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defsubst org-query-next-when (pred &key (next-fn #'outline-next-heading))
  "Return position of next item if PRED is non-nil for current entry.
NEXT-FN is called to find the next item,
e.g. `outline-next-heading' or `org-end-of-subtree'."
  (save-excursion
    (save-restriction
      ;; NOTE: Possibly unnecessary to widen.  Tests pass without it,
      ;; and it should be faster without it:
      ;; (widen)
      (when (funcall pred)
        (funcall next-fn)))))

(defsubst org-query-skip-headline (pred)
  "Return position of next heading if PRED is non-nil for current heading."
  (org-query-next-when pred))

(defsubst org-query-skip-tree (pred)
  "Return position of next subtree if PRED is non-nil for current heading."
  (org-query-next-when pred :next-fn (lambda ()
                                       (org-end-of-subtree t))))


;;;;;;;;;
;; DSL ;;
;;;;;;;;;

(defmacro org-query-select (resolution body)
  "Skip one headline or the whole tree (depending on RESOLUTION) if it satisfies the condition in BODY.
Argument RESOLUTION \"headline\" or \"tree\"
Argument BODY should return t or nil"
  ()
  (let ((skipfunc (intern (concat "org-query-skip-" resolution))))
    `(lambda ()
       (,skipfunc
        (lambda () (not (,@body)))))))

(defmacro org-query-parent (body)
  "Just a macro to run BODY in a lambda"
  ()
  `(org-query-struct-ancestor (lambda () (,@body))))

(defmacro org-query-child (body)
  "Just a macro to run BODY in a lambda"
  ()
  `(org-query-struct-child (lambda () (,@body))))

(defmacro org-query-headline (body)
  "Just a macro to run BODY in a lambda"
  ()
  `(org-query-struct-headline (lambda () (,@body))))

(provide 'org-query)

;;; org-query.el ends here
