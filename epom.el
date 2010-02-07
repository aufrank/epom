;; epom.el --- Pomodoro time management in emacs

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
;; variables for internal use, store and manage cycles and states
(defvar epom-state nil
  "The state corresponding to `epom-timer'.")

(defvar epom-timer nil
  "The timer correspinding to `epom-state'.")

(defvar epom-completed-states ()
  "States that have been completed.")

;; customization options
(defgroup epom nil
  "Customization options for epom, Pomodoro time management in emacs."
  :group 'applications)

;; definition of cycles
(defcustom epom-states '((work .5)
                         (break .25))
  "List of states and their durations.
The name of the state is a symbol, the duration is specified in
seconds."
  :group 'epom
  :type '(alist :key-type symbol :value-type (group number)))

;; hooks to run at the beginning and end of each cycle
(defcustom epom-state-start-hook ()
  "Functions to run after a state starts."
  :group 'epom
  :type 'hook)

(defcustom epom-state-stop-hook ()
  "Functions to run when a state ends."
  :group 'epom
  :type 'hook)

(defcustom epom-state-restart-hook ()
  "Functions to run when a state is restarted."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-start-hook ()
  "Extra functions to run after a cycle is started."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-stop-hook ()
  "Extra functions to run after a cycle is stopped."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-restart-hook ()
  "Extra functions to run after a cycle is restarted."
  :group 'epom
  :type 'hook)

;; functions for starting, stopping, and manipulating states
(defun epom-get-next-state nil
  "Pop the first element of `epom-states' into `epom-state'."
  (setq epom-state (pop epom-states)))

(defun epom-complete-state nil
  "Put `epom-state' into `epom-completed-states'."
  (setq epom-completed-states
        (append epom-completed-states (list epom-state))
        epom-state nil))

(defun epom-reset-states nil
  (setq epom-states (append epom-completed-states epom-states)
        epom-completed-states ()))

;; user functions for starting, stopping, and restarting cycles
(defun epom-start-cycle ()
  "Start a new cycle."
  (interactive)
  (epom-reset-states)
  (epom-advance-state)
  (run-hooks 'epom-cycle-start-hook))

(defun epom-stop-cycle ()
  "Stop the current cycle."
  (interactive)
  (epom-stop-state)
  (run-hooks 'epom-cycle-stop-hook))

(defun epom-resume-cycle ()
  "Resume the current-cycle, starting with `epom-state' (restarts `epom-timer').
Usually used after `epom-stop-cycle'."
  (interactive)
  (epom-start-state)
  (run-hooks 'epom-cycle-resume-hook))

(defun epom-restart-cycle ()
  "Stop the current cycle and start another."
  (interactive)
  (epom-stop-cycle)
  (epom-start-cycle)
  (run-hooks 'epom-cycle-restart-hook))

(defun epom-cancel-cycle ()
  "Stop the current cycle and reset `epom-states'."
  (interactive)
  (epom-stop-cycle)
  (epom-reset-states)
  (run-hooks 'epom-cycle-cancel-hook))

;; user functions for advancing and restarting pomodoro states
(defun epom-start-state nil
  "Start `epom-state', stopping an existing timer if necessary."
  (interactive)
  ;; if we're already running a state, stop it
  (if epom-timer
      (epom-stop-state))
  (epom-display-state-start)
  (epom-start-timer)
  (run-hooks 'epom-state-start-hook))

(defun epom-stop-state nil
  "Stop `epom-timer', ending `epom-state'."
  (interactive)
  (if epom-timer
      (epom-stop-timer))
  (epom-display-state-stop)
  (epom-complete-state)
  (epom-advance-state)
  (run-hooks 'epom-state-stop-hook))

(defun epom-restart-state nil
  "Restart `epom-timer', corresponding to `epom-state'."
  (interactive)
  (epom-stop-state)
  (epom-start-state)
  (run-hooks 'epom-state-restart-hook))

(defun epom-advance-state nil
  "Stop `epom-timer', ending `epom-state'; move to the next element of `epom-states'."
  (interactive)
  (if epom-states
      (progn (epom-get-next-state)
             (epom-start-state))
    (epom-reset-states)))

;; functions for starting and stopping timers
(defun epom-start-timer nil
  "Start a timer ending based on `epom-state', setting `epom-timer'."
  (let ((state (car epom-state))
        (duration (nth 1 epom-state)))
    (setq epom-timer
          (run-at-time (* 60 duration) nil
                       'epom-stop-state))))

(defun epom-stop-timer nil
  "Stop `epom-timer' and reset it to nil."
  (cancel-timer epom-timer)
  (setq epom-timer nil))

;; functions for user notifications
(defun epom-display-state-start nil
  "Display a message when `epom-state' starts."
  (let ((state (car epom-state)))
    (epom-display-time-message
     (format "%s %s." state "starts")
     'alarm)))

(defun epom-display-state-stop nil
  "Display a message when `epom-state' ends."
  (let ((state (car epom-state)))
    (epom-display-time-message
     (format "%s %s." state "ends"))
    'check))

(defun epom-display-time-message (message &optional icon)
  "Display MESSAGE, followed by the current time in HH:MM format.
If available, use todochiku for notifications (with appropriate
ICON).  Otherwise, use heuristic to decide between the echo area
or a message box.  See `message-or-box' for details."
  (let ((msg (format "%s  %s"
                     message
                     (substring (current-time-string) 11 16))))
    (if (featurep 'todochiku)
        (todochiku-message "epom"
                           msg
                           (todochiku-icon (or icon 'default)))
      (message-or-box msg))))

(provide 'epom)
;;; epom.el ends here
