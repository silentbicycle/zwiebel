;;; zwiebel.el --- Work/break interval timer
;;
;; This file is not part of Emacs. In fact, it's BSD licensed.
;;
;; Copyright (c) 2010, Scott Vokes <vokes.s@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;     * Neither the name of the <ORGANIZATION> nor the names of its
;;       contributors may be used to endorse or promote products
;;       derived from this software without specific prior written
;;       permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;; Commentary:
;;    Work on something. Focus, get it done. Clock's ticking.
;;
;;    This is a set of hooks wrapped around a simple state machine and timer.
;;    The minutes remaining for the current task (or break) appear on
;;    the modeline.
;;
;;    For typical usage, just run zwiebel-dwim (which will run zwiebel-start,
;;    zwiebel-interrupt, or zwiebel-break, depending on the current state).
;;    You'll probably want to give it a global keybinding, such as
;;        (global-set-key (kbd "C-c z") 'zwiebel-dwim) .
;;
;;    Register events with the hooks for notifications, logging, etc.
;;        (add-hook 'zwiebel-complete-hook
;;           (message (format "Done: %s" *zwiebel-task*)))
;;
;;    I'd name this for the Tomato-in-Italian Technique, but it's trademarked.
;;
;;    For an autoload, use (autoload 'zwiebel-dwim "zwiebel" "" t).
;;
;; TODO:
;;    * Set up custom stuff. I never use custom, but hey.
;;    * Is there a better way to get the seconds remaining from a timer?

(defconst zwiebel-version 0.1 "Version.")

;; Configuration
(defvar zwiebel-work-minutes 25 "The length of a work session, in minutes.")
(defvar zwiebel-break-minutes 5 "The length of a break, in minutes.")
(defvar zwiebel-long-break-minutes 30 "The length of a long break, in minutes.")
(defvar zwiebel-show-seconds nil "Show seconds remaining on timer?")
(defvar zwiebel-timer-on-modeline t
  "Whether to add the current timer info to the mode-line.")
(defvar zwiebel-ask-for-task t "Whether to ask for a task description")

;; Hooks
(defvar zwiebel-start-hook nil "Task start hook.")
(defvar zwiebel-interrupt-hook nil "Task interruption hook.")
(defvar zwiebel-complete-hook nil "Task completion hook.")
(defvar zwiebel-break-hook nil "Break start hook.")
(defvar zwiebel-break-done-hook nil "Break end hook.")

;; State variables, mutated during operation.
(defvar *zwiebel-timer* nil "The timer.")
(defvar *zwiebel-state* 'idle "The current timer state: idle, work, overtime, break")
(defvar *zwiebel-task* nil "Description of the current task.")
(defvar *zwiebel-last-complete* nil "The last completion time.")
(defvar *zwiebel-time-string* "" "The string to add to the mode-line.")
(defvar *zwiebel-completed* 0 "The number of completed work sessions.")
(defvar *zwiebel-interrupted* 0 "The number of interrupted work sessions.")


(defun zwiebel-start (uarg)
  "Start a new task. If `zwiebel-ask-for-task' is not nil, prompt
for a task description, unless run with universal argument."
  (interactive "p")
  (when (and zwiebel-ask-for-task (or (not (= uarg 4)) (null *zwiebel-task*)))
    (setq *zwiebel-task* (read-string "Task? ")))
  (assert (or (eq *zwiebel-state* 'idle) (eq *zwiebel-state* 'break)))
  (setq *zwiebel-timer*
        (run-at-time (* 60 zwiebel-work-minutes)
                     nil
                     (lambda ()
                       (run-hooks 'zwiebel-complete-hook)
                       (setq *zwiebel-completed* (1+ *zwiebel-completed*)
                             *zwiebel-last-complete* (current-time)
                             *zwiebel-state* 'overtime
                             *zwiebel-timer* nil)))
        *zwiebel-state* 'work)
  (zwiebel-update-time-string))

(defun zwiebel-task ()
  "Print the current task."
  (interactive)
  (message *zwiebel-task*))

(defun zwiebel-interrupt ()
  "Interrupt the current task or break."
  (interactive)
  (cond ((eq *zwiebel-state* 'work)
	 (progn
	   (cancel-timer *zwiebel-timer*)
	   (setq *zwiebel-interrupted* (1+ *zwiebel-interrupted*)
		 *zwiebel-timer* nil
		 *zwiebel-state* 'idle)
	   (run-hooks 'zwiebel-interrupt-hook)
	   (zwiebel-update-time-string)))
	((eq *zwiebel-state* 'break)
	 (progn
	   (cancel-timer *zwiebel-timer*)
           (run-hooks 'zwiebel-break-done-hook)
	   (setq *zwiebel-state* 'idle
		 *zwiebel-timer* nil)
	   (zwiebel-update-time-string)))))

(defun zwiebel-get-timer-remaining ()
  "Get the remaining time from a timer."
  ; Yuck. There has to be a better way to get the seconds remaining.
  (when (vectorp *zwiebel-timer*)
    (let ((ct (current-time)))
      (mapcar (lambda (x) (elt *zwiebel-timer* x)) '(1 2 3)))))

(defun zwiebel-seconds-to-m-s-pair (secs)
  (cons (/ secs 60)
        (mod secs 60)))

(defun zwiebel-countdown ()
  "Return remaining (minutes . seconds) on *zwiebel-timer*."
  (let ((ct (current-time))
        (rem (zwiebel-get-timer-remaining)))
    (if (and rem (time-less-p ct rem))
        (let ((diff (time-subtract rem ct)))
          (zwiebel-seconds-to-m-s-pair (cadr diff)))
        '(0 . 0))))            ; (zwiebel-countdown)

(defun zwiebel-countdown-str (m-s &optional force-seconds)
  "Convert minute-second pair to time string."
  (let ((m (number-to-string (car m-s)))
        (s (number-to-string (cdr m-s))))
    (if (or zwiebel-show-seconds force-seconds)
        (concat m ":" s)
        m)))

(defun zwiebel-overtime-str ()
  "Return string for how long work has gone into overtime."
  (let ((diff (if *zwiebel-last-complete*
                  (time-subtract (current-time) *zwiebel-last-complete*)
                  nil)))
    (if diff
        (zwiebel-countdown-str
         (zwiebel-seconds-to-m-s-pair (cadr diff)) t)
        "0")))

(defun zwiebel-break (uarg)
  (interactive "p")
  (if (or (eq *zwiebel-state* 'overtime) (eq *zwiebel-state* 'idle))
      (progn
        (run-hooks 'zwiebel-break-hook)
        (setq *zwiebel-state* 'break
              *zwiebel-timer*
              (run-at-time (* 60 (if current-prefix-arg
                                     zwiebel-long-break-minutes
                                   zwiebel-break-minutes))
                           nil
                           (lambda ()
                             (run-hooks 'zwiebel-break-done-hook)
                             (setq *zwiebel-state* 'idle
                                   *zwiebel-timer* nil))))
		(zwiebel-update-time-string))
      (error "Not complete")))

(defun zwiebel-modeline ()
  "Look up the current time string."
  *zwiebel-time-string*)

(defun zwiebel-update-time-string ()
  "Update the current time string."
  (setq *zwiebel-time-string*
        (let ((s *zwiebel-state*)
              (tc (zwiebel-countdown-str (zwiebel-countdown)))
              (ot (zwiebel-overtime-str)))
          (cond ((eq s 'idle) "")
                ((eq s 'work) (concat "<W " tc "> "))
                ((eq s 'overtime) (concat "<O +" ot "> "))
                ((eq s 'break) (concat "<B " tc "> "))
                (t (error "Match failed"))))))

(defun zwiebel-tick ()
  "Callback for functions to be run once per second."
  (zwiebel-update-time-string)
  (force-mode-line-update))

(defun zwiebel-dwim (uarg)
  "Start task, interrupt, break, or end break, depending on current state."
  (interactive "p")
  (let ((s *zwiebel-state*))
    (cond ((eq s 'idle) (zwiebel-start uarg))
          ((eq s 'work) (zwiebel-interrupt))
          ((eq s 'overtime) (zwiebel-break uarg))
          ((eq s 'break) (zwiebel-interrupt))
          (t (error "Match failed")))))

;; Add countdown to the mode-line.
(when zwiebel-timer-on-modeline
  (let ((str '(:eval (zwiebel-modeline))))
    (unless (member str global-mode-string)
      (push str global-mode-string))))

;; Start repeating callback to update timer.
(run-at-time 1 1 'zwiebel-tick)

(provide 'zwiebel)
