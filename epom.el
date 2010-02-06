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

;; definition of cycles
(defcustom epom-states '((work .5)
                         (break .25))
  "List of states and their durations.
The name of the state is a symbol, the duration is specified in
seconds."
  :group 'epom
  :type '(alist :key-type symbol :value-type (group number)))

(defvar epom-current-state nil
  "The state corresponding to `epom-current-timer'.")

(defvar epom-current-timer nil
  "The timer correspinding to `epom-current-state'.")

(defvar epom-completed-states ()
  "States that have been completed.")

;; hooks to run at the beginning and end of each cycle
(defcustom epom-state-start-hook
  '(epom-set-current-state
    epom-start-timer
    epom-display-state-start)
  "Functions to run when a state starts."
  :group 'epom
  :type 'hook)

(defcustom epom-state-stop-hook
  '(epom-display-state-stop
    epom-complete-state
    epom-maybe-complete-cycle)
  "Functions to run when a state ends."
  :group 'epom
  :type 'hook)

(defcustom epom-state-restart-hook
  '(epom-stop-current-timer
    epom-start-timer
    epom-display-state-start)
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

(defun epom-add-hook (hook function)
  "Like `add-hook', except always append new FUNCTION to HOOK."
  (add-hook hook function t))

(defun epom-set-current-state nil
  "Put the first element in `epom-states' into `epom-current-state'."
  (setq epom-current-state (pop epom-states)))

(defun epom-complete-state nil
  "Put `epom-current-state' into `epom-completed-states'."
  (setq epom-completed-states
        (append epom-completed-states (list epom-current-state)))
  (setq epom-current-state nil))

(defun epom-maybe-complete-cycle nil
  "Refill `epom-states' from `epom-completed-states' if necessary."
  (if epom-states t (setq epom-states epom-completed-states
                          epom-completed-states ())))

(defun epom-start-timer nil
  "Start a timer ending based on `epom-current-state', setting `epom-current-timer'."
  (let ((state (car epom-current-state))
        (duration (nth 1 epom-current-state)))
    (setq epom-current-timer
          (run-at-time (* 60 duration) nil
                       'run-hooks 'epom-state-stop-hook))))

(defun epom-stop-current-timer nil
  "Stop `org-current-timer' and reset it to nil."
  (cancel-timer epom-current-timer)
  (setq epom-current-timer-nil))

(defun epom-display-state-start nil
  "Display a message when `epom-current-state' starts."
  (let ((state (car epom-current-state)))
    (epom-display-time-message
     (format "%s %s." state "starts")
     'alarm)))

(defun epom-display-state-stop nil
  "Display a message when `epom-current-state' ends."
  (let ((state (car epom-current-state)))
    (epom-display-time-message
     (format "%s %s." state "ends"))
    'check))

;; utility function for printing message followed by time
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

;; user functions for starting, stopping, and restarting pomodoro
;; cycles
(defun epom-start-cycle ()
  "Start a new pomodoro (work then break)."
  (interactive)
  (run-hooks 'epom-start-cycle-hook)
  (epom-start-work))

(defun epom-stop-cycle ()
  "Stop the current pomodoro."
  (interactive)
  (run-hooks 'epom-stop-cycle-hook)
  (epom-stop-cycle))

(defun epom-restart-cycle ()
  "Stop the current pomodoro and start another."
  (interactive)
  (run-hooks 'epom-restart-cycle-hook)
  (epom-stop-cycle)
  (epom-start-cycle))

;; user functions for advancing and restarting pomodoro states
(defun epom-next-state nil
  "Stop `epom-current-timer', ending `epom-current-state'; move to the next element of `epom-states'."
  (interactive)
  (cancel-timer epom-current-timer)
  (run-hooks 'epom-state-stop-hook)
  (run-hooks 'epom-state-start-hook))

(defun epom-restart-state nil
  "Restart `epom-current-state'."
  (interactive)
  (cancel-timer epom-current-timer)
  (run-hooks 'epom-state-restart-hook))

(provide 'epom)
;;; epom.el ends here
