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

;; Utility functions for tracking workflow in the Pomodoro time
;; management system.  Eventual goal is to interact with org mode,
;; todochiku, and magit

;;; Code:

;; customization options
(defgroup epom nil
  "Customization options for epom, Pomodoro time management in emacs."
  :group 'applications)

;; duration of work/break cycles
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

;; text of status messages
(defcustom epom-start-pom-message "Pomodoro started."
  "*Message displayed after a new pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-start-work-message "Work started."
  "*Message displayed after the work section of a pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-start-break-message "Break started."
  "*Message displayed after the break section of a pomodoro is started.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-stop-pom-message "Pomodoro completed."
  "*Message displayed after a new pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-stop-work-message "Work completed."
  "*Message displayed after the work section of a pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

(defcustom epom-stop-break-message "Break completed."
  "*Message displayed after the break section of a pomodoro is completed.
This message is followed by the time in HH:MM format."
  :group 'epom
  :type 'string)

;; hooks to run functions before and after each step of the cycle
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

(defcustom epom-stop-pom-hook 'epom-display-stop-pom-message
  "*Hook that is run after a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-stop-work-hook 'epom-display-stop-work-message
  "*Hook that is run after the work portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-stop-break-hook 'epom-display-stop-break-message
  "*Hook that is run after the break portion of a pomodoro is completed."
  :group 'epom
  :type 'hook)

(defcustom epom-cancel-pom-hook nil
  "*Hook that is run after a pomodoro is canceled."
  :group 'epom
  :type 'hook)

(defcustom epom-restart-pom-hook nil
  "*Hook that is run after a pomodoro is restarted."
  :group 'epom
  :type 'hook)

;; utility function for printing message followed by time
(defun epom-display-time-message (message &optional icon)
  "Display MSG, followed by the current time in HH:MM format.
If available, use todochiku for notifications.  Otherwise, use heuristic to decide between the echo area or a message box.  See `message-or-box' for details."
  (let ((msg (format "%s  %s"
                     message
                     (substring (current-time-string) 11 16))))
    (if (featurep 'todochiku)
        (todochiku-message "epom"
                           msg
                           (todochiku-icon (or icon 'default)))
      (message-or-box msg))))

;; 0-ary functions for displaying messages with timestamps, default
;; action in corresponding hooks
(defun epom-display-start-pom-message ()
  "Displays the string in `epom-start-pom-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-pom-message))

(defun epom-display-start-work-message ()
  "Displays the string in `epom-start-work-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-work-message))

(defun epom-display-start-break-message ()
  "Displays the string in `epom-start-break-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-start-break-message))

(defun epom-display-stop-pom-message ()
  "Displays the string in `epom-stop-pom-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-stop-pom-message))

(defun epom-display-stop-work-message ()
  "Displays the string in `epom-stop-work-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-stop-work-message))

(defun epom-display-stop-break-message ()
  "Displays the string in `epom-stop-break-message' followed by the time in HH:MM format."
  (epom-display-time-message epom-stop-break-message))

;; user functions for starting, stopping, and restarting pomodoro
;; cycles
(defun epom-start-pom ()
  "Start a new pomodoro (work then break)."
  (interactive)
  (run-hooks 'epom-start-pom-hook)
  (epom-start-work))

(defun epom-cancel-pom ()
  "Cancel the current pomodoro."
  (interactive)
  (run-hooks 'epom-cancel-pom-hook)
  (epom-stop-pom))

(defun epom-restart-pom ()
  "Cancel the current pomodoro and start another."
  (interactive)
  (run-hooks 'epom-restart-pom-hook)
  (epom-cancel-pom)
  (epom-start-pom))

;; functions that implement the Pomodoro work/break cycle
(defun epom-start-work ()
  "Start the work portion of a pomodoro."
  (run-hooks 'epom-start-work-hook)
  (run-at-time epom-work-duration () 'epom-stop-work))

(defun epom-start-break ()
  "Start the break portion of a pomodoro."
  (run-hooks 'epom-start-break-hook)
  (run-at-time epom-break-duration () 'epom-stop-break))

(defun epom-stop-pom ()
  "Stop a pomodoro (work and break)."
  (run-hooks 'epom-stop-pom-hook))

(defun epom-stop-work ()
  "Stop the work portion of a pomodoro."
  (run-hooks 'epom-stop-work-hook)
  (epom-start-break))

(defun epom-stop-break ()
  "Stop the work portion of a pomodoro."
  (run-hooks 'epom-stop-break-hook)
  (epom-stop-pom))

(provide 'epom)
;;; epom.el ends here
