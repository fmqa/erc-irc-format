;;; erc-irc-format.el --- Transient menus for IRC formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2024,2025 Alcor

;; Author: Alcor <alcor@tilde.club>
;; URL: https://github.com/fmqa/erc-irc-format
;; Keywords: erc irc
;; Version: 0.9
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

;; Installation via `use-package':

;; (use-package erc-irc-format
;;   :defer t
;;   :vc (:url "https://github.com/fmqa/erc-irc-format.git" :branch "main")
;;   :bind (:map erc-mode-map ("C-c q" . erc-irc-format)))

;; If you are using Emacs ≤ 30.0, you will need to update the built-in package
;; `transient'. By default, `package.el' will not upgrade a built-in package.
;; Set the customizable variable `package-install-upgrade-built-in' to `t' to
;; override this.

(require 'transient)
(require 'erc)
(require 'erc-goodies)

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
   ("i" erc-irc-format-italicize :description erc-irc-format--describe-italic)
   ("b" erc-irc-format-boldify   :description erc-irc-format--describe-bold)
   ("u" erc-irc-format-underline :description erc-irc-format--describe-underline)]
  (interactive)
  (setq erc-irc-format-color n)
  (transient-setup 'erc-irc-format))

;; Erc module to normalize text inserted at the prompt.
;; Author: Alcor <alcor@tilde.club>

(defun erc-irc-format-normalize (str)
  "Given a formatted text string STR, remove text properties."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (set-text-properties (point-min) (point-max) nil)
    (buffer-string)))

(defun erc-irc-format-normalize-setup ()
  (add-hook 'yank-transform-functions #'erc-irc-format-normalize 70 t))

;;;###autoload (autoload 'erc-irc-format-normalize-mode "erc-irc-format" nil t)
(define-erc-module irc-format-normalize nil
  "This mode removes properties from text inserted at the Erc prompt."
  ((add-hook 'erc-mode-hook #'erc-irc-format-normalize-setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-irc-format-normalize-setup)))
  ((remove-hook 'erc-mode-hook #'erc-irc-format-normalize-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'yank-transform-functions #'erc-irc-format-normalize t)))))

(provide 'erc-irc-format)
;;; erc-irc-format.el ends here
