;;; shiftless.el --- Insert uppercase without shift

;; Copyright (c) 2019, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/shiftless.el
;; Version: 0.0.1
;; Package-Requires: ((seq "2.16"))
;; Keywords: Shift, Uppercase

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Detecting your key holding, automatically convert a series of lowercase characters to a single uppercase.
;;
;; When you holding a key, there's a ~delay~ before the repeating start, and a ~interval~ between each repeat.
;;
;; For example, I use
;;
;; #+BEGIN_SRC shell
;; xset r rate 180 45
;; #+END_SRC
;;
;; * Limitations
;;
;; Not work with kmacro, since we know nothing with time in kmacro.
;;
;; You need a high keyboard repeat rate, at least higher than your finger can do.
;;
;; So in my case, key repeat ~delay~ is 180ms, and key repeat ~interval~ is 1s / 45 = 22ms.
;;
;; Now I can enable the shiftless with:
;; #+BEGIN_SRC emacs-lisp
;; (setq shiftless-delay 0.2) ;; larger than 0.18
;; (setq shiftless-interval 0.04) ;; larger than 0.022
;; (shiftless-mode 1)
;; #+END_SRC
;;
;; * License
;; Licensed under the GPLv3.

;;; Code:

(defcustom shiftless-delay 0.200
  "The delay before key repeat start when holding a key."
  :type '(number :tag "Seconds")
  :group 'shiftless)

(defcustom shiftless-interval 0.05
  "The interval between each repeat input when holding a key."
  :type '(number :tag "Seconds")
  :group 'shiftless)

(defcustom shiftless-upper-rules
  nil
  "The uppercase mapping for non-alphabet characters."
  :type '(list :tag "Rules")
  :group 'shiftless)

(defvar shiftless--last-insert-char nil)

(defvar shiftless--last-insert-time nil)

(defvar shiftless--count-repeat-key 1)

(defvar shiftless--trigger-count 3)

(defvar shiftless--cursor-count 1)

(defvar shiftless--after-upcase nil)

(defun shiftless--cancel-input ()
  "Cancel this input.
For most case, just delete the previous char is enough.
Some mode need more work to be compatible."
  (delete-char -1))

(defun shiftless--should-process-p ()
  (and (numberp last-input-event)
       (or
        (seq-find (lambda (x) (equal (car x) last-input-event)) shiftless-upper-rules)
        (<= ?a last-input-event ?z))))

(defun shiftless--upcase-previous-char ()
  (let ((ch (char-before)))
    (let ((pair (seq-find (lambda (x) (equal (car x) ch))
                          shiftless-upper-rules)))
      (if pair
          (let ((x (cdr pair)))
            (delete-char -1)
            (if (stringp x)
                (insert x)
              (call-interactively x)))
        (when (<= ?a ch ?z)
          (upcase-char -1))))))

(defun shiftless--get-cursor-count ()
  "Return the count of cursors.

To be compatible with multiple cursors, we have to save the result of mc/num-cursors, because it doesn't always return the correct number."
  (when (fboundp 'mc/num-cursors)
      (when (< shiftless--cursor-count (mc/num-cursors))
        (setq shiftless--cursor-count (mc/num-cursors))))
  shiftless--cursor-count)

(defun shiftless--holding-in-time-p ()
  (time-less-p (current-time)
               (time-add shiftless--last-insert-time
                         (if (> shiftless--count-repeat-key
                                shiftless--cursor-count)
                             shiftless-interval
                           shiftless-delay))))

(defun shiftless--after-self-insert ()
  (let ((cursor-count (shiftless--get-cursor-count)))
    (if (and shiftless--last-insert-char
             shiftless--last-insert-time
             (shiftless--should-process-p)
             (equal shiftless--last-insert-char last-input-event)
             (shiftless--holding-in-time-p))
        (let* ((cnt-max (* cursor-count shiftless--trigger-count))
               (cnt-min (- cnt-max cursor-count)))
          (setq shiftless--count-repeat-key (1+ shiftless--count-repeat-key))
          (cond
           ((> shiftless--count-repeat-key cnt-max)
            (shiftless--cancel-input)
            (setq shiftless--after-upcase t))
           ((and
             (> shiftless--count-repeat-key cnt-min)
             (<= shiftless--count-repeat-key cnt-max))
            (backward-delete-char (1- shiftless--trigger-count))
            (shiftless--upcase-previous-char))))
      (progn
        (setq shiftless--count-repeat-key 1
              shiftless--cursor-count 1
              shiftless--after-upcase nil))))
  (setq shiftless--last-insert-char last-input-event
        shiftless--last-insert-time (current-time)))

(defun shiftless-use-layout-dvp ()
  (setq shiftless-upper-rules
        '((?@ . "^")
          (?/ . "?")
          (92 . "|")
          (?# . "`")
          (?, . "<")
          (?. . ">")
          (59 . ":")
          (?~ . "~")
          (?& . "%")
          (?- . "_")
          (?$ . "~"))))

(defun shiftless-use-layout-qwerty ()
  (setq shiftless-upper-rules
        '((?` . "~")
          (?/ . "?")
          (92 . "|")
          (?1 . "!")
          (?2 . "@")
          (?3 . "#")
          (?4 . "$")
          (?5 . "%")
          (?6 . "^")
          (?7 . "&")
          (?8 . "*")
          (?9 . "(")
          (?0 . ")")
          (?- . "_")
          (?= . "+")
          (?, . "<")
          (?. . ">")
          (59 . ":")
          (?' . "\""))))

(defun shiftless--prevent-advice (func &rest args)
  (unless shiftless--after-upcase
    (apply func args)))

(defun shiftless--enable ()
  (add-hook 'post-self-insert-hook 'shiftless--after-self-insert))

(defun shiftless--disable ()
  (remove-hook 'post-self-insert-hook 'shiftless--after-self-insert))

(shiftless-use-layout-qwerty)

;;;###autoload
(define-minor-mode shiftless-mode
  "A minor help you insert uppercase by holding the key."
  nil
  "shiftless"
  nil
  (if shiftless-mode
      (shiftless--enable)
    (shiftless--disable)))

(provide 'shiftless)

;;; shiftless.el ends here
