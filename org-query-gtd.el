;;; org-query-gtd.el --- Use the agenda for Getting Things Done

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


(require 'org-query)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convenience functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-query-is-parent-headline-p (match)
  "Does parent headline value MATCH."
  (org-query-struct-ancestor (apply-partially 'org-query-stringmatch match)))

(defun org-query-if-project-p (&optional todo-states)
  "Is headline a project and is it's todo kwd one of TODO-STATES?"
  (let ((match-states (cond (todo-states todo-states) (t (mapcar 'car org-todo-kwd-alist)))))
    ;; (message "if-project-p match %S on %S with %S and %S" match-states (org-heading-components) org-todo-keywords (mapcar 'car org-todo-kwd-alist))
    (and
     ;; is headline a task
     (org-query-todo match-states)
     ;; does any child have a todo state
     (org-query-child (org-query-todo)))))

(defun org-query-if-project-task-p (&optional project-todo-states task-todo-states)
  "Is headline a task in a project?
PROJECT-TODO-STATES optional list of todo states the project should be in
TASK-TODO-STATES optional list of todo states the task should be in"
  (setq match-project-states (cond (project-todo-states project-todo-states) (t (mapcar 'car org-todo-kwd-alist))))
  (setq match-task-states (cond (task-todo-states task-todo-states) (t (mapcar 'car org-todo-kwd-alist))))
  (and
   ;; headline should be a todo 
   (org-query-todo match-task-states)
   ;; should not have a child;; otherwise i'd be a project
   (not (org-query-child (org-query-todo)))
   ;; is one it's ancestors a task
   (org-query-parent (org-query-todo match-project-states))))

(provide 'org-query-gtd)
