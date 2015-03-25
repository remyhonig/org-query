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


;; Projects

(defun org-query-gtd-active-project () 
  "Is the headline at point an active project"
  (and
   (not (org-query-parent (org-query-stringmatch "^Someday / Maybe")))
   (org-query-child (org-query-todo))
   (org-query-todo '("NEXT"))))


(defun org-query-gtd-project () 
  "Is the headline at point a waiting project"
  (and
   (not (org-query-parent (org-query-stringmatch "^Someday / Maybe")))
   (org-query-child (org-query-todo))))


(defun org-query-gtd-someday-project () 
  "Is the headline at point a waiting project"
  (and
   (org-query-parent (org-query-stringmatch "^Someday / Maybe"))
   (org-query-child (org-query-todo))))


(defun org-query-gtd-active-project-armed ()
  "Active project with a NEXT state child"
  (and (org-query-gtd-active-project)
       (org-query-child (org-query-todo '("NEXT" "WAITING")))))


(defun org-query-gtd-active-project-stuck ()
  "Active project with a NEXT state child"
  (and (org-query-gtd-active-project)
       (not (org-query-child (org-query-todo '("NEXT" "WAITING"))))))


(defun org-query-gtd-refile ()
  "Tasks to refile"
  (org-query-parent (org-query-stringmatch "^Inbox")))


(defun org-query-gtd-active-project-next-task ()
  "Is the headline a next action in an active project."
  (and
   (org-query-parent (org-query-gtd-active-project))
   (org-query-todo '("NEXT"))))


(defun org-query-gtd-loose-task ()
  "Tasks that do not belong to any project"
  (not (or
        (org-query-parent (or
                           (org-query-todo)
                           (org-query-stringmatch "\\(^Inbox\\|^Someday / Maybe\\)")))
        (org-query-child (org-query-todo)))))

(defun org-query-gtd-someday-loose-task ()
  "Tasks that do not belong to any project"
  (and
   (org-query-parent (org-query-stringmatch "\\(^Inbox\\|^Someday / Maybe\\)"))
   (and
       (not (org-query-parent (org-query-todo)))
       (not (org-query-child (org-query-todo))))))

(defun org-query-gtd-backlog-task ()
  "Tasks in active project with a TODO state"
  (and (org-query-parent (org-query-gtd-active-project))
       (not (org-query-parent (org-query-todo '("CANCELLED" "DONE"))))
       (not (org-query-parent (org-query-stringmatch "^Someday / Maybe")))))


(provide 'org-query-gtd)
