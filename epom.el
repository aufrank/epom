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
;; variables for internal use, store and manage cycles and steps
(defvar epom-step nil
  "The step corresponding to `epom-timer'.")

(defvar epom-timer nil
  "The timer correspinding to `epom-step'.")

(defvar epom-completed-steps ()
  "Steps that have been completed.")

;; customization options
(defgroup epom nil
  "Customization options for epom, Pomodoro time management in emacs."
  :group 'applications)

;; default format of notifications
(defcustom epom-message-format "%s %s.  %s."
  "Default format of notifications.
A format string as described in `format'.  Three arguments can be
interpolated into the string:  the name of the step, a message about the event, and the time of the notification."
  :group 'epom
  :type 'string)

(defcustom epom-step-start-message "starts"
  "String to interpolate into `epom-message-format' as the second argument when a step starts."
  :group 'epom
  :type 'string)

(defcustom epom-step-stop-message "ends"
  "String to interpolate into `epom-message-format' as the second argument when a step stops."
  :group 'epom
  :type 'string)

;; definition of cycles
(defcustom epom-cycle '((work 25 "")
                        (break 5 ""))
  "An alist of steps, their durations, and notification messsages.
The name of the step is a symbol.
The duration is specified as a number of minutes.
The message format is a format string.  See `epom-message-format' for more information."
  :group 'epom
  :type '(alist :key-type symbol
                :value-type (list (number :tag "Duration")
                                  (string :tag "Message format"))))

(defvar epom-remaining-cycle epom-cycle
  "Temporary variable to hold the current instantiation of `epom-cycle'.")

;; hooks to run at the beginning and end of each cycle
(defcustom epom-step-start-hook ()
  "Functions to run after a step starts."
  :group 'epom
  :type 'hook)

(defcustom epom-step-stop-hook ()
  "Functions to run before a step ends."
  :group 'epom
  :type 'hook)

(defcustom epom-step-restart-hook ()
  "Functions to run after a step is restarted."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-start-hook ()
  "Extra functions to run after a cycle is started."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-stop-hook ()
  "Extra functions to run before a cycle is stopped."
  :group 'epom
  :type 'hook)

(defcustom epom-cycle-restart-hook ()
  "Extra functions to run after a cycle is restarted."
  :group 'epom
  :type 'hook)

;; functions for starting, stopping, and manipulating steps
(defun epom-get-next-step nil
  "Pop the first element of `epom-remaining-cycle' into `epom-step'."
  (setq epom-step (pop epom-remaining-cycle)))

(defun epom-complete-step nil
  "Put `epom-step' into `epom-completed-steps'."
  (setq epom-completed-steps
        (append epom-completed-steps (list epom-step))
        epom-step nil))

(defun epom-reset-cycle nil
  "Reset `epom-remaining-cycle' to the original value of `epom-cycle'; unset `org-step'."
  (setq epom-remaining-cycle epom-cycle
        epom-step nil
        epom-completed-steps '()))

;; user functions for starting, stopping, and restarting cycles
(defun epom-start-cycle ()
  "Start a new cycle."
  (interactive)
  (epom-reset-cycle)
  (epom-advance-step)
  (run-hooks 'epom-cycle-start-hook))

(defun epom-stop-cycle ()
  "Stop the current cycle."
  (interactive)
  (run-hooks 'epom-cycle-stop-hook)
  (epom-stop-step))

(defun epom-resume-cycle ()
  "Resume the current-cycle, starting with `epom-step' (restarts `epom-timer').
Usually used after `epom-stop-cycle'."
  (interactive)
  (epom-start-step)
  (run-hooks 'epom-cycle-resume-hook))

(defun epom-restart-cycle ()
  "Stop the current cycle and start another."
  (interactive)
  (epom-stop-cycle)
  (epom-start-cycle)
  (run-hooks 'epom-cycle-restart-hook))

(defun epom-cancel-cycle ()
  "Stop the current cycle and reset `epom-cycle'."
  (interactive)
  (epom-stop-cycle)
  (epom-reset-cycle)
  (run-hooks 'epom-cycle-cancel-hook))

;; user functions for advancing and restarting pomodoro steps
(defun epom-start-step nil
  "Start `epom-step', stopping an existing timer if necessary."
  (interactive)
  ;; if we're already running a step, stop it
  (if epom-timer
      (epom-stop-step))
  (epom-display-step-message epom-step-start-message)
  (epom-start-timer)
  (run-hooks 'epom-step-start-hook))

(defun epom-stop-step nil
  "Stop `epom-timer', ending `epom-step'."
  (interactive)
  (if epom-timer
      (epom-stop-timer))
  (epom-display-step-message epom-step-stop-message)
  (epom-complete-step)
  (epom-advance-step)
  (run-hooks 'epom-step-stop-hook))

(defun epom-restart-step nil
  "Restart `epom-timer', corresponding to `epom-step'."
  (interactive)
  (epom-stop-step)
  (epom-start-step)
  (run-hooks 'epom-step-restart-hook))

(defun epom-advance-step nil
  "Stop `epom-timer', ending `epom-step'; move to the next element of `epom-remaining-cycle'."
  (interactive)
  (if epom-remaining-cycle
      (progn (epom-get-next-step)
             (epom-start-step))
    (epom-reset-cycle)))

;; functions for starting and stopping timers
(defun epom-start-timer nil
  "Start a timer ending based on `epom-step', setting `epom-timer'."
  (let ((step (car epom-step))
        (duration (nth 1 epom-step)))
    (setq epom-timer
          (run-at-time (* 60 duration) nil
                       'epom-stop-step))))

(defun epom-stop-timer nil
  "Stop `epom-timer' and reset it to nil."
  (cancel-timer epom-timer)
  (setq epom-timer nil))

;; functions for user notifications
(defun epom-display-step-message (event-msg)
  "Display `epom-step', EVENT-MSG, and `current-time-string', using `epom-message-format'."
  (let* ((step (car epom-step))
         (time (current-time-string))
         (step-msg (car (last epom-step)))
         (msg-format (if (not (string= "" step-msg))
                         step-msg
                       epom-message-format))
         (msg (format msg-format step event-msg time)))
    (cond ((fboundp 'org-notify)
           (org-notify msg))
          ((fboundp 'todochiku-message)
           (todochiku-message "epom" msg (todochiku-icon 'check)))          
          (t (message-or-box msg)))))

(provide 'epom)
;;; epom.el ends here
