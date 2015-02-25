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
  (let ((match-states (cond (todo-states todo-states) (t (mapcar 'car org-todo-kwd-alist)))))
    ;; (message "if-todo-p match %S on %S %S" match-states (org-heading-components) (org-get-todo-state))
    (not (null (member (org-get-todo-state) match-states)))))

(defun org-query-if-project-p (&optional todo-states)
  "Is headline a project and is it's todo kwd one of TODO-STATES?"
  (let ((match-states (cond (todo-states todo-states) (t (mapcar 'car org-todo-kwd-alist)))))
    ;; (message "if-project-p match %S on %S with %S and %S" match-states (org-heading-components) org-todo-keywords (mapcar 'car org-todo-kwd-alist))
    (and
     ;; is headline a task
     (org-query-if-todo-p match-states)
     ;; does this headline have any tasks in any state
     (org-query-if-child-p 'org-query-if-todo-p))))

(defun org-query-if-project-task-p (&optional project-todo-states task-todo-states)
  "Is headline a task in a project?
PROJECT-TODO-STATES optional list of todo states the project should be in
TASK-TODO-STATES optional list of todo states the task should be in"
  (setq match-project-states (cond (project-todo-states project-todo-states) (t (mapcar 'car org-todo-kwd-alist))))
  (setq match-task-states (cond (task-todo-states task-todo-states) (t (mapcar 'car org-todo-kwd-alist))))
  (and
   ;; headline should be a todo 
   (org-query-if-todo-p match-task-states)
   ;; should not have a child;; otherwise i'd be a project
   (not (org-query-if-child-p 'org-query-if-todo-p))
   ;; is one it's ancestors a task
   (org-query-if-ancestor-p (apply-partially 'org-query-if-todo-p match-project-states))))

(defun org-query-skip-headline (func)
  "Return marker to next headline if FUNC satisfies."
  (save-restriction
    (widen)
    (let ((marker (save-excursion (or (outline-next-heading) (point-max)))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defun org-query-skip-tree (func)
  "Return marker to next headline if FUNC satisfies."
  (save-restriction
    (widen)
    (let ((marker (save-excursion (org-end-of-subtree t))))
      (setq satisfied (funcall func))
      (if satisfied marker nil))))

(defmacro org-query-select (resolution body)
  "Skip one headline or the whole tree (depending on RESOLUTION) if it satisfies the condition in BODY.
Argument RESOLUTION \"headline\" or \"tree\"
Argument BODY should return t or nil"
  ()
  (let ((skipfunc (intern (concat "org-query-skip-" resolution))))
    `(lambda ()
       (,skipfunc
        (lambda () (not (,@body)))))))

(provide 'org-query)

;;; org-query.el ends here
