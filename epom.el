;;; epom.el --- Pomodoro time management in emacs

;; Copyright (C) 2010  Austin Frank

;; Author: Austin Frank <austin.frank@gmail.com>
;; Keywords: tools

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

;;

;;; Code:

;; utility functions for utility tracking
;; eventual goal is to interact with org mode

(defgroup epom nil
  "Customization options for epom, Pomodoro time management in emacs.")

(defcustom epom-work-duration "10 sec"
  "Duration of the work portion of a pomodoro.
Defined using the relative timer specification for `run-at-time'.  Default is
\"25 min\"."
  :group 'epom
  :type 'string)

(defcustom epom-break-duration "10 sec"
  "Duration of the break portion of a pomodoro.
Defined using the relative timer specification for `run-at-time'.  Default is \"5 min\"."
  :group 'epom
  :type 'string)

(defcustom epom-start-pom-hook
  '((epom-display-time-message "Pomodoro started at"))
  "Hook that is run after a new pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-start-work-hook
  '((epom-display-time-message "Work started at"))
  "Hook that is run after the work portion of a pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-start-break-hook
  '((epom-display-time-message "Break started at"))
  "Hook that is run after the break portion of a pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-end-pom-hook
  '((epom-display-time-message "Pomodoro completed at"))
  "Hook that is run after a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-end-work-hook
  '((epom-display-time-message "Work completed at"))
  "Hook that is run after the work portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-end-break-hook
  '((epom-display-time-message "Break completed at"))
  "Hook that is run after the break portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defun epom-display-time-message (msg)
  "Display MSG in the echo area, followed by the current time in HH:MM format."
  (message-or-box "%s %s"
                  msg
                  (substring (current-time-string) 11 16)))

(defun epom-start-pom ()
  "Start a new pomodoro (work then break)."
  (interactive)
  (run-hooks 'epom-start-pom-hook)
  (epom-start-work))

(defun epom-start-work ()
  "Start the work portion of a pomodoro."
  (run-hooks 'epom-start-work-hook)
  (run-at-time epom-work-duration nil 'epom-end-work))

(defun epom-start-break ()
  "Start the break portion of a pomodoro."
  (run-hooks 'epom-start-break-hook)
  (run-at-time epom-break-duration nil 'epom-end-break))

(defun epom-end-pom ()
  "Complete a pomodoro (work and break)."
  (run-hooks 'epom-end-pom-hook))

(defun epom-end-work ()
  "End the work portion of a pomodoro."
  (run-hooks 'epom-end-work-hook)
  (epom-start-break))

(defun epom-end-break ()
  "End the work portion of a pomodoro."
  (run-hooks 'epom-end-break-hook)
  (epom-end-pom))

(provide 'epom)
;;; epom.el ends here
