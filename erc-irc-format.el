;;; erc-irc-format.el --- Transient menus for IRC formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2024,2025 Alcor

;; Author: Alcor <alcor@tilde.club>
;; URL: https://github.com/fmqa/erc-irc-format
;; Keywords: erc irc
;; Version: 0.4
;; Package-Requires: ((emacs "29.1") (erc "5.6") (transient "0.4.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To bind `C-c q' (recommended keybinding), add the following to your init.el:

;; (require 'erc-irc-format)
;; (define-key erc-mode-map (kbd "C-c q") #'erc-irc-format)

;; Or, using `use-package':
;; (use-package erc
;;   :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this.

(require 'transient)
(require 'erc)
(require 'erc-goodies)
(require 'text-property-search)

(defvar-local erc-irc-format-color nil "Currently selected color.")

(defun erc-irc-format--describe-color (s color)
  "Returns the given string s, colorized with the given color."
  (propertize
   s
   'face
   (append
    (list (erc-get-fg-color-face (or erc-irc-format-color color)))
    (when (and color erc-irc-format-color) (list (erc-get-bg-color-face (or color erc-irc-format-color)))))))

(defun erc-irc-format--describe-italic ()
  "Returns a sample italicized string."
  (propertize "italic" 'face '(erc-italic-face)))

(defun erc-irc-format--describe-bold ()
  "Returns a sample bold string."
  (propertize "bold" 'face '(erc-bold-face)))

(defun erc-irc-format--describe-underline ()
  "Returns a sample underlined string."
  (propertize "underline" 'face '(erc-underline-face)))

(defun erc-irc-format--color-code (color)
  "Inserts IRC color codes."
  (if (and color erc-irc-format-color) (format "\03%d,%d" erc-irc-format-color color) (format "\03%d" (or color erc-irc-format-color))))

(defun erc-irc-format--enclose-with (open close)
  "Encloses a marked region with the given open/close characters."
  (let ((from (region-beginning))
        (to (region-end)))
    (save-excursion
      (goto-char to)
      (insert close)
      (goto-char from)
      (insert open))))

(defun erc-irc-format--insert-or-enclose (open &optional close)
  "Inserts (or encloses a marked region with) the given character(s)."
  (if (region-active-p)
      (erc-irc-format--enclose-with open (or close open))
    (insert open)))

(defun erc-irc-format--insert-color (&optional fg)
  "Inserts IRC color codes."
  (erc-irc-format--insert-or-enclose (erc-irc-format--color-code fg) "\03"))

;;;###autoload
(defun erc-irc-format-boldify ()
  "Inserts a ^B control code, or encloses the active region with it"
  (interactive)
  (erc-irc-format--insert-or-enclose "\02"))

;;;###autoload
(defun erc-irc-format-italicize ()
  "Inserts an ^I control code, or encloses the active region with it"
  (interactive)
  (erc-irc-format--insert-or-enclose "\035"))

;;;###autoload
(defun erc-irc-format-underline ()
  "Inserts a ^_ control code, or encloses the active region with it"
  (interactive)
  (erc-irc-format--insert-or-enclose "\037"))

(defun erc-irc-format--dispatch (color)
  "Dispatches to the transient or to the insertion function."
  (if erc-irc-format-color
      (erc-irc-format--insert-color color)
    (erc-irc-format color)))

(defun erc-irc-format--color-p ()
  "Returns the value of the currently selected color."
  erc-irc-format-color)

;; Main transient menu
;;;###autoload (autoload 'erc-irc-format "erc-irc-format" nil t)
(transient-define-prefix erc-irc-format (&optional n)
  "Apply IRC formatting codes."
  [:description (lambda () (if erc-irc-format-color "Background" "Foreground"))
   [("00" (lambda () (erc-irc-format--describe-color "white"       0)) (lambda () (interactive) (erc-irc-format--dispatch  0)))
    ("01" (lambda () (erc-irc-format--describe-color "black"       1)) (lambda () (interactive) (erc-irc-format--dispatch  1)))
    ("02" (lambda () (erc-irc-format--describe-color "blue"        2)) (lambda () (interactive) (erc-irc-format--dispatch  2)))
    ("03" (lambda () (erc-irc-format--describe-color "green"       3)) (lambda () (interactive) (erc-irc-format--dispatch  3)))]
   [("04" (lambda () (erc-irc-format--describe-color "red"         4)) (lambda () (interactive) (erc-irc-format--dispatch  4)))
    ("05" (lambda () (erc-irc-format--describe-color "brown"       5)) (lambda () (interactive) (erc-irc-format--dispatch  5)))
    ("06" (lambda () (erc-irc-format--describe-color "magenta"     6)) (lambda () (interactive) (erc-irc-format--dispatch  6)))
    ("07" (lambda () (erc-irc-format--describe-color "orange"      7)) (lambda () (interactive) (erc-irc-format--dispatch  7)))]
   [("08" (lambda () (erc-irc-format--describe-color "yellow"      8)) (lambda () (interactive) (erc-irc-format--dispatch  8)))
    ("09" (lambda () (erc-irc-format--describe-color "lightgreen"  9)) (lambda () (interactive) (erc-irc-format--dispatch  9)))
    ("10" (lambda () (erc-irc-format--describe-color "cyan"       10)) (lambda () (interactive) (erc-irc-format--dispatch 10)))
    ("11" (lambda () (erc-irc-format--describe-color "lightcyan"  11)) (lambda () (interactive) (erc-irc-format--dispatch 11)))]
   [("12" (lambda () (erc-irc-format--describe-color "lightblue"  12)) (lambda () (interactive) (erc-irc-format--dispatch 12)))
    ("13" (lambda () (erc-irc-format--describe-color "pink"       13)) (lambda () (interactive) (erc-irc-format--dispatch 13)))
    ("14" (lambda () (erc-irc-format--describe-color "grey"       14)) (lambda () (interactive) (erc-irc-format--dispatch 14)))
    ("15" (lambda () (erc-irc-format--describe-color "lightgrey"  15)) (lambda () (interactive) (erc-irc-format--dispatch 15)))]]
  [:class transient-row :if erc-irc-format--color-p
   ("s" "spoilers" (lambda () (interactive) (erc-irc-format--insert-color erc-irc-format-color)))
   ("RET" (lambda () (propertize "default" 'face (list (erc-get-fg-color-face erc-irc-format-color)))) (lambda () (interactive) (erc-irc-format--insert-color)))]
  [:class transient-row :description "Effects" :if-not erc-irc-format--color-p
   ("i" erc-irc-format--describe-italic    erc-irc-format-italicize)
   ("b" erc-irc-format--describe-bold      erc-irc-format-boldify)
   ("u" erc-irc-format--describe-underline erc-irc-format-underline)]
  (interactive)
  (setq erc-irc-format-color n)
  (transient-setup 'erc-irc-format))

;; Erc module to normalize text inserted at the prompt.
;; Author: Alcor <alcor@tilde.club>

(defvar erc-normalize-copy-cc t "Copy control codes")

(defmacro erc-normalize-color-mapper (n pred get-color-face value)
  "Generates face mapper for color codes [0;n] where value is checked
against get-color-face via pred."
  `(cond
    ,@(let (clauses)
        (while (>= n 0)
          (push `((funcall ,pred (funcall ,get-color-face ,n) ,value) ,n) clauses)
          (setq n (1- n)))
        clauses)))

(defun erc-normalize-get-color (pred get-color-face value)
  "Returns the color code for face VALUE given the inverse function GET-COLOR-FACE
and the given equality predicate."
  (erc-normalize-color-mapper 98 pred get-color-face value))

(defun erc-normalize-text-enclose-cc (start end fg bg bold italic underline)
  "Given formatting flags FG BG BOLD ITALIC UNDERLINE, enclose the string
between START and END with the corresponding IRC formatting codes."
  (let (open close)
    (when bold (push "\02" open) (push "\02" close))
    (when italic (push "\035" open) (push "\035" close))
    (when underline (push "\037" open) (push "\037" close))
    (when (or fg bg)
      (push (if bg (format "\03%d,%d" (or fg 99) bg) (format "\03%d" fg)) open)
      (push "\03" close))
    (when (and open close)
      (save-excursion
        (goto-char end)
        (insert (string-join close))
        (goto-char start)
        (insert (string-join open))))))

(defun erc-normalize-text (value start end)
  "Given a formatted string VALUE between START and END, enclose the
string the appropriate formatting codes."
  (let* ((pred (if (listp value) #'member #'equal))
         (fg (erc-normalize-get-color pred #'erc-get-fg-color-face value))
         (bg (erc-normalize-get-color pred #'erc-get-bg-color-face value))
         (bold (funcall pred 'erc-bold-face value))
         (italic (funcall pred 'erc-italic-face value))
         (underline (funcall pred 'erc-underline-face value)))
    (erc-normalize-text-enclose-cc start end fg bg bold italic underline)))

(defun erc-normalize-cc-substitute (str)
  "Given a formatted string STR, normalize it for sending over IRC,
substituting ERC faces with corresponding control characters if
ERC-NORMALIZE-COPY-CC is T."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (when erc-normalize-copy-cc
      (let ((match nil))
        (while (setq match (text-property-search-forward 'font-lock-face nil nil))
          (let ((start (prop-match-beginning match))
                (end (prop-match-end match))
                (value (prop-match-value match)))
            (erc-normalize-text value start end)))))
    (set-text-properties (point-min) (point-max) nil)
    (buffer-string)))

(defun erc-normalize-setup ()
  (add-hook 'yank-transform-functions #'erc-normalize-cc-substitute 70 t))

;;;###autoload (autoload 'erc-normalize-mode "erc-irc-format" nil t)
(define-erc-module normalize nil
  "This mode removes properties from text inserted at the prompt,
possibly inserting corresponding control characters."
  ((add-hook 'erc-mode-hook #'erc-normalize-setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-normalize-setup)))
  ((remove-hook 'erc-mode-hook #'erc-normalize-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'yank-transform-functions #'erc-normalize-cc-substitute t)))))

(provide 'erc-irc-format)
;;; erc-irc-format.el ends here
