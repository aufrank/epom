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
  "Customization options for epom, Pomodoro time management in emacs."
  :group 'applications)

(defcustom epom-work-duration "25 min"
  "*Duration of the work portion of a pomodoro.
Defined using the relative timer specification for `run-at-time'.  Default is
\"25 min\"."
  :group 'epom
  :type 'string)

(defcustom epom-break-duration "5 min"
  "*Duration of the break portion of a pomodoro.
Defined using the relative timer specification for `run-at-time'.  Default is \"5 min\"."
  :group 'epom
  :type 'string)

(defcustom epom-start-pom-message "Pomodoro started at"
  "*Message displayed after a new pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-start-work-message "Work started at"
  "*Message displayed after the work section of a pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-start-break-message "Break started at"
  "*Message displayed after the break section of a pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-end-pom-message "Pomodoro completed at"
  "*Message displayed after a new pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-end-work-message "Work completed at"
  "*Message displayed after the work section of a pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-end-break-message "Break completed at"
  "*Message displayed after the break section of a pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-start-pom-hook 'epom-display-start-pom-message
  "*Hook that is run after a new pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-start-work-hook 'epom-display-start-work-message
  "*Hook that is run after the work portion of a pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-start-break-hook 'epom-display-start-break-message
  "*Hook that is run after the break portion of a pomodoro is started."
  :group 'epom
  :type 'hook)

(defcustom epom-end-pom-hook 'epom-display-end-pom-message
  "*Hook that is run after a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-end-work-hook 'epom-display-end-work-message
  "*Hook that is run after the work portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-end-break-hook 'epom-display-end-break-message
  "*Hook that is run after the break portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defun epom-display-time-message (msg)
  "Display MSG in the echo area, followed by the current time in HH:MM format."
  (message-or-box "%s %s"
                  msg
                  (substring (current-time-string) 11 16)))

(defun epom-display-start-pom-message ()
  "Displays the string in `epom-start-pom-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-pom-message))

(defun epom-display-start-work-message ()
  "Displays the string in `epom-start-work-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-work-message))

(defun epom-display-start-break-message ()
  "Displays the string in `epom-start-break-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-break-message))

(defun epom-display-end-pom-message ()
  "Displays the string in `epom-end-pom-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-end-pom-message))

(defun epom-display-end-work-message ()
  "Displays the string in `epom-end-work-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-end-work-message))

(defun epom-display-end-break-message ()
  "Displays the string in `epom-end-break-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-end-break-message))

(defun epom-start-pom ()
  "Start a new pomodoro (work then break)."
  (interactive)
  (run-hooks 'epom-start-pom-hook)
  (epom-start-work))

(defun epom-start-work ()
  "Start the work portion of a pomodoro."
  (run-hooks 'epom-start-work-hook)
  (run-at-time epom-work-duration () 'epom-end-work))

(defun epom-start-break ()
  "Start the break portion of a pomodoro."
  (run-hooks 'epom-start-break-hook)
  (run-at-time epom-break-duration () 'epom-end-break))

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
