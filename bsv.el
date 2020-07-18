;;; bsv.el --- Extended bs.el to show the buffer names vertically -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Takaaki ISHIKAWA

;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Keywords: convenience
;; Version: 0.9.5
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; URL: https://github.com/takaxp/bsv
;; Package-Requires: ((emacs "25.1"))
;; Twitter: @takaxp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package extends bs.el with displaying the buffer list vertically.
;; Also enable to select a buffer by selecting a list number.
;;
;; Install:
;;  - Get bsv.el from GitHub.
;;
;; Setup:
;;  - (with-eval-after-load "bs"
;;      (global-set-key (kbd "M-]") 'bs-cycle-next)
;;      (global-set-key (kbd "M-[") 'bs-cycle-previous)
;;      (require 'bsv))
;;
;; Keybindings:
;;  - The moom-mode-map is automatically configured.
;;  - To see more details and examples, go https://github.com/takaxp/bsv.
;;
;; Example:
;;  - After calling `bs-cycle-next' then buffers will be displayed vertically.
;;  - It is possible to select a buffer by pressing a list number.
;;    - If you type "3", then "c.txt" will be displayed.
;;
;;   |                           |         |                           |
;;   |---------------------------|         |---------------------------|
;;   | Next buffers:             |         | Previous buffers:         |
;;   | 1. a.txt                  |         | 5.     a.txt              |
;;   | 2.  b.txt                 |         | 4.    b.txt               |
;;   | 3.   c.txt                |         | 3.   c.txt                |
;;   | 4.    d.txt               |         | 2.  d.txt                 |
;;   | 5.     e.txt              |         | 1. e.txt                  |
;;

;;; Change Log:

;;; Code:

(eval-when-compile
  (require 'bs))

(defcustom bsv-lighter "Bsv"
  "Package name in mode line."
  :type 'string
  :group 'bsv)

(defcustom bsv-message-timeout 5
  "Clear echo area after this period."
  :type 'integer
  :group 'bsv)

(defcustom bsv-slant t
  "Display the buffer list with slant."
  :type 'boolean
  :group 'bsv)

(defcustom bsv-max-height 9
  "Limit the number of listing buffers.
If specifying more than 10, corrected to 9 automatically."
  :type 'integer
  :group 'bsv)

(defcustom bsv-switch-by-key 'number
  "Select a buffer by pressing a key in a range from \"1\" to \"10\".

To disable this functionality:
  (with-eval-after-load \"bs\"
    (setq bsv-switch-by-key nil))
OR
  (custom-set-variables
   '(bsv-switch-by-key nil))"
  :type '(choice (const :tag "1,2,...,9" :value number)
                 (const :tag "f1, f2,...,f9" :value function)
                 (const :tag "Disable" :value nil))
  :group 'bsv)

(defcustom bsv-show-countdown t
  "If Non-nil, remaining time to deactivate the minor mode is shown in mode-line."
  :type 'boolean
  :group 'bsv)

(defvar bsv-mode-map
  (let ((map (make-sparse-keymap)))
    (when (memq bsv-switch-by-key '(number function))
      (let ((key 1))
        (while (< key 10)
          (define-key map (kbd (if (eq bsv-switch-by-key 'number)
                                   (number-to-string key)
                                 (concat "<f" (number-to-string key) ">")))
            `(lambda () (interactive)
               (let ((buffer (nth (1- ,key) bsv-cycle-list)))
                 (when buffer
                   (switch-to-buffer buffer))
                 (message "%s" buffer))))
          (setq key (1+ key)))))
    map)
  "The keymap for `bsv' to switch to a buffer.
Assign key in a range from \"1\" to \"9\".")

(defvar bsv-cycle-list nil)
(defvar bsv-separater nil)
(defvar bsv--timer nil)
(defvar bsv--disable-features '(flyspell mic-paren))
(defvar bsv--count-down nil)
(defvar bsv--remaining 0)

(defun bsv--lighter ()
  "Lighter."
  (when bsv-lighter
    (concat " " bsv-lighter
            (when (and bsv-show-countdown
                       (>= bsv--remaining 0)
                       (eq (current-buffer) (car (buffer-list))))
              (format ":%d" bsv--remaining)))))

(defun bsv--cycle-list (&optional reverse)
  "Create cycle list to show vertically in echo area.
If REVERSE is non-nil, the list will be reversed."
  (if (cdr bs--cycle-list)
      (let* ((bs-list (if reverse
                          (reverse (cdr bs--cycle-list))
                        (cdr bs--cycle-list)))
             (len (min (if (> bsv-max-height 9) 9 bsv-max-height)
                       (length bs-list)
                       (/ (frame-height) 6))) ;; FIXME
             (bs-cycle-list nil)
             (str nil))
        (setq bsv-cycle-list bs-list)
        (while (> len 0)
          (push (format "%d. %s%s"
                        (1+ (length bs-cycle-list))
                        ;; (if bsv-slant (make-string (1- len) ?\s) "")
                        (if bsv-slant (make-string
                                       (length bs-cycle-list) ?\s) "")
                        (car bs-list))
                bs-cycle-list)
          (setq bs-list (cdr bs-list))
          (setq len (1- len)))
        (unless reverse
          (setq bs-cycle-list (reverse bs-cycle-list)))
        (setq str (format "%s" (mapcar
                                (lambda (buffer)
                                  (format "\n%s" buffer))
                                bs-cycle-list)))
        (substring str 1 (1- (length str))))
    nil))

(defun bsv--cycle-next ()
  "Select next buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (bsv-mode 1)
  (let ((bs--buffer-coming-from (current-buffer))
        (bs-dont-show-regexp   bs-dont-show-regexp)
        (bs-must-show-regexp   bs-must-show-regexp)
        (bs-dont-show-function bs-dont-show-function)
        (bs-must-show-function bs-must-show-function)
        (bs--show-all          nil))
    (bs-set-configuration
     (or bs-cycle-configuration-name bs-default-configuration))
    (let ((bs-buffer-sort-function nil)
          (bs--current-sort-function nil))
      (let* ((tupel (bs-next-buffer (if (or (eq last-command
                                                'bs-cycle-next)
                                            (eq last-command
                                                'bs-cycle-previous))
                                        bs--cycle-list)))
             (next (car tupel))
             (cycle-list (cdr tupel)))
        ;; We don't want the frame iconified if the only window in the frame
        ;; happens to be dedicated.
        (bury-buffer (current-buffer))
        (switch-to-buffer next nil t)
        (setq bs--cycle-list (append (cdr cycle-list)
                                     (list (car cycle-list))))
        (bs-message-without-log
         (concat
          (propertize (concat
                       (when bsv-separater
                         (concat (make-string (frame-width) ?_) "\n"))
                       "Next buffers: ")
                      'face 'minibuffer-prompt)
          (or (bsv--cycle-list) "this buffer")))))))

(defun bsv--cycle-previous ()
  "Select previous buffer defined by buffer cycling.
The buffers taking part in buffer cycling are defined
by buffer configuration `bs-cycle-configuration-name'."
  (interactive)
  (bsv-mode 1)
  (let ((bs--buffer-coming-from (current-buffer))
        (bs-dont-show-regexp   bs-dont-show-regexp)
        (bs-must-show-regexp   bs-must-show-regexp)
        (bs-dont-show-function bs-dont-show-function)
        (bs-must-show-function bs-must-show-function)
        (bs--show-all          nil))
    (bs-set-configuration
     (or bs-cycle-configuration-name bs-default-configuration))
    (let ((bs-buffer-sort-function nil)
          (bs--current-sort-function nil))
      (let* ((tupel (bs-previous-buffer (if (or (eq last-command
                                                    'bs-cycle-next)
                                                (eq last-command
                                                    'bs-cycle-previous))
                                            bs--cycle-list)))
             (prev-buffer (car tupel))
             (cycle-list (cdr tupel)))
        (switch-to-buffer prev-buffer nil t)
        (setq bs--cycle-list (append (last cycle-list)
                                     (reverse (cdr (reverse cycle-list)))))
        (bs-message-without-log
         (concat
          (propertize (concat
                       (when bsv-separater
                         (concat (make-string (frame-width) ?_) "\n"))
                       "Previous buffers: ")
                      'face 'minibuffer-prompt)
          (or (bsv--cycle-list t) "this buffer")))))))

(defun bsv--message-without-log (&rest args)
  "Like `message' but don't log it on the message log.
All arguments ARGS are transferred to function `message'."
  (let ((message-log-max nil)
        (minibuffer-message-timeout bsv-message-timeout))
    (apply 'minibuffer-message args)))

(defun bsv--deactivate ()
  "Disable function `bsv-mode' when the list is cleared from echo area."
  (unless (memq this-command '(bs-cycle-next bs-cycle-previous))
    (bsv-mode -1)))

(defun bsv--cancel-timers ()
  "Cancel existing timers."
  (when bsv--timer
    (cancel-timer bsv--timer)
    (setq bsv--timer nil))
  (when bsv--count-down
    (cancel-timer bsv--count-down)
    (setq bsv--count-down nil
          bsv--remaining 0)))

(defun bsv--count-down ()
  "Decrement `bsv--remaining'."
  (setq bsv--remaining (1- bsv--remaining)))

(defun bsv--setup ()
  "Setup."
  (when (memq 'mic-paren bsv--disable-features)
    (paren-deactivate))
  (when (memq 'flyspell bsv--disable-features)
    (flyspell-mode -1))
  (when bsv--timer
    (cancel-timer bsv--timer))
  (setq bsv--timer (run-with-idle-timer 0 nil #'bsv--deactivate))
  (bsv--cancel-timers)
  (setq bsv--timer (run-with-idle-timer 0 nil #'bsv--deactivate))
  (when (and bsv-message-timeout
             (> bsv-message-timeout 0))
    (setq bsv--remaining bsv-message-timeout)
    (setq bsv--count-down (run-with-timer 0.5 1 #'bsv--count-down))))

(defun bsv--abort ()
  "Abort."
  (when (memq 'mic-paren bsv--disable-features)
    (paren-activate))
  (when (memq 'flyspell bsv--disable-features)
    (flyspell-mode 1))
  (bsv--cancel-timers))

(defun bsv-disable-advices ()
  "Remove all advice functions."
  (interactive)
  (advice-remove 'bs-cycle-next #'bsv--cycle-next)
  (advice-remove 'bs-cycle-previous #'bsv--cycle-previous)
  (advice-remove 'bs-message-without-log #'bsv--message-without-log))

(defun bsv-enable-advices ()
  "Enable all advice functions."
  (interactive)
  (advice-add 'bs-cycle-next :override #'bsv--cycle-next)
  (advice-add 'bs-cycle-previous :override #'bsv--cycle-previous)
  (advice-add 'bs-message-without-log :override #'bsv--message-without-log))

;;;###autoload
(define-minor-mode bsv-mode
  "Extended bs.el for displaying buffers vertically."
  :init-value nil
  :lighter (:eval (bsv--lighter))
  :keymap bsv-mode-map
  :global t
  :require 'bsv
  :group 'bsv
  (if bsv-mode
      (bsv--setup)
    (bsv--abort)))

;; init
(when (require 'bs)
  (bsv-enable-advices))

(provide 'bsv)

;;; bsv.el ends here
