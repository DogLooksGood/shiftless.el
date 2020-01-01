;;; shiftless.el --- Insert uppercase without shift

;; Copyright (c) 2019, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/shiftless.el
;; Version: 0.0.1
;; Package-Requires: ()
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
;; So in my case, key repeat ~delay~ is 180ms, and key repeat ~interval~ is 45ms.

;; Now I can enable the shiftless with:
;; #+BEGIN_SRC emacs-lisp
;; (setq shiftless-delay 0.2) ;; larger than 0.18
;; (setq shiftless-interval 0.05) ;; larger than 0.045
;; (shiftless-mode 1)
;; #+END_SRC

;; * License
;; Licensed under the GPLv3.

;;; Code:


(defcustom shiftless-delay 0.200
  "The delay before key repeat start when holding a key.")

(defcustom shiftless-interval 0.05
  "The interval between each repeat input when holding a key.")

(defcustom shiftless-upper-rules
  '((?` . "~")
    (?/ . "?")
    (92 . "|")
    (49 . "!")
    (50 . "@")
    (51 . "#")
    (52 . "$")
    (53 . "%")
    (54 . "^")
    (55 . "&")
    (57 . "*")
    (58 . "(")
    (59 . ")")
    (?- . "_")
    (?= . "+")
    (?, . "<")
    (?. . ">")
    (59 . ":")
    (?' . "\""))
  "The uppercase mapping for non-alphabet characters.")

(defvar shiftless--last-insert-char nil)

(defvar shiftless--last-insert-time nil)

(defvar shiftless--count-repeat-key 1)

(defvar shiftless--trigger-count 3)

(defvar shiftless--cursor-count 1)

(defun shiftless--upcase-previous-char ()
  (let ((ch (char-before)))
    (when ch
      (let ((pair (find-if (lambda (x) (equal (car x) ch))
                           shiftless-upper-rules)))
        (if pair
            (let ((x (cdr pair)))
              (delete-char -1)
              (if (stringp x)
                  (insert x)
                (call-interactively x)))
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
             (not (or (equal shiftless--last-insert-char 'return)
                      (equal shiftless--last-insert-char 32)))
             (equal shiftless--last-insert-char last-input-event)
             (shiftless--holding-in-time-p))
        (let* ((cnt-max (* cursor-count shiftless--trigger-count))
               (cnt-min (- cnt-max cursor-count)))
          (setq shiftless--count-repeat-key (1+ shiftless--count-repeat-key))
          (cond
           ((> shiftless--count-repeat-key cnt-max)
            (delete-char -1))

           ((and
             (> shiftless--count-repeat-key cnt-min)
             (<= shiftless--count-repeat-key cnt-max))
            (backward-delete-char (1- shiftless--trigger-count))
            (shiftless--upcase-previous-char))))

      (progn
        (setq shiftless--count-repeat-key 1
              shiftless--cursor-count 1))))

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
          (49 . "!")
          (50 . "@")
          (51 . "#")
          (52 . "$")
          (53 . "%")
          (54 . "^")
          (55 . "&")
          (57 . "*")
          (58 . "(")
          (59 . ")")
          (?- . "_")
          (?= . "+")
          (?, . "<")
          (?. . ">")
          (59 . ":")
          (?' . "\""))))

(defun shiftless--enable ()
  (add-hook 'post-self-insert-hook 'shiftless--after-self-insert))

(defun shiftless--disable ()
  (remove-hook 'post-self-insert-hook 'shiftless--after-self-insert))

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
