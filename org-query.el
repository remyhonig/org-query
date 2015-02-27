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

(defun org-query-struct-ancestor (func)
  "Is any ancestor t for FUNC.
Return t if it does or nil if it does not."
  (let ((result nil))
    (save-excursion
      (while (and (not result) (org-up-heading-safe))
        (setq result (funcall func))))
    result))

(defun org-query-struct-child (func)
  "Is any child satisfying FUNC.
Return t if it does or nil if it does not."
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

(defun org-query-struct-headline (func)
  "Does the headline at point satisfy FUNC.
Return t if it does or nil if it does not.
This function is mainly used as a single point to print debug messages."
  (funcall func))


;;;;;;;;;;;;;;;;;;;;;;;
;; headline matching ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun org-query-stringmatch (match)
  "Is headline at point a MATCH."
  (not (null (string-match match (nth 4 (org-heading-components))))))

(defun org-query-todo (&optional todo-states)
  "Is headline a task and is it's todo kwd one of TODO-STATES?"
  (let ((match-states (cond (todo-states todo-states) (t (mapcar 'car org-todo-kwd-alist)))))
    ;; (message "if-todo-p match %S on %S %S" match-states (org-heading-components) (org-get-todo-state))
    (not (null (member (org-get-todo-state) match-states)))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; skipping functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-query-skip-headline (func)
  "Return marker to next headline if FUNC satisfies."
  (save-restriction
    (widen)
    (let ((marker (save-excursion (or (outline-next-heading) (point-max))))
          (satisfied nil))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defun org-query-skip-tree (func)
  "Return marker to next headline if FUNC satisfies."
  (save-restriction
    (widen)
    (let ((marker (save-excursion (org-end-of-subtree t)))
          (satisfied nil))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))


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
