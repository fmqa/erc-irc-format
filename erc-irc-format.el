;;; erc-irc-format.el --- Transient menus for IRC formatting -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alcor

;; Author: Alcor <alcor@tilde.club>
;; URL: https://github.com/fmqa/elisp-snippets
;; Keywords: erc irc
;; Version: 0.1
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

(defmacro erc-irc-format--define-color-describer (color n)
  `(defun ,(intern (concat "erc-irc-format--describe-color-" color)) (&optional fg)
     (propertize ,color 'face (append (list (erc-get-fg-color-face (or fg ,n)))
                                      (when fg (list (erc-get-bg-color-face ,n)))))))

;; Color formatters
(erc-irc-format--define-color-describer "white"      0)
(erc-irc-format--define-color-describer "black"      1)
(erc-irc-format--define-color-describer "blue"       2)
(erc-irc-format--define-color-describer "green"      3)
(erc-irc-format--define-color-describer "red"        4)
(erc-irc-format--define-color-describer "brown"      5)
(erc-irc-format--define-color-describer "magenta"    6)
(erc-irc-format--define-color-describer "orange"     7)
(erc-irc-format--define-color-describer "yellow"     8)
(erc-irc-format--define-color-describer "lightgreen" 9)
(erc-irc-format--define-color-describer "cyan"       10)
(erc-irc-format--define-color-describer "lightcyan"  11)
(erc-irc-format--define-color-describer "lightblue"  12)
(erc-irc-format--define-color-describer "pink"       13)
(erc-irc-format--define-color-describer "grey"       14)
(erc-irc-format--define-color-describer "lightgrey"  15)

(defun erc-irc-format--describe-italic () (propertize "italic" 'face '(erc-italic-face)))
(defun erc-irc-format--describe-bold () (propertize "bold" 'face '(erc-bold-face)))
(defun erc-irc-format--describe-underline () (propertize "underline" 'face '(erc-underline-face)))

(defun erc-irc-format--color-code (fg &optional bg)
  (if bg (format "\03%d,%d" fg bg) (format "\03%d" fg)))

(defun erc-irc-format--enclose-with (open close)
  (let ((from (region-beginning))
        (to (region-end)))
    (save-excursion
      (goto-char to)
      (insert close)
      (goto-char from)
      (insert open))))

(defun erc-irc-format--insert-or-enclose (open &optional close)
  (if (region-active-p)
      (erc-irc-format--enclose-with open (or close open))
    (insert open)))

(defun erc-irc-format--insert-color (fg &optional bg)
  (erc-irc-format--insert-or-enclose (erc-irc-format--color-code fg bg) "\03"))

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
  "Inserts a ^U control code, or encloses the active region with it"
  (interactive)
  (erc-irc-format--insert-or-enclose "\037"))

;; Macro helper for defining background color transient
(defmacro erc-irc-format--color-define-prefix (name fg color)
  `(transient-define-prefix ,name ()
     [,(format "Foreground: %s; Background: ?" color)
      [("00" "white"      (lambda () (interactive) (erc-irc-format--insert-color ,fg 0))  :description (lambda () (erc-irc-format--describe-color-white      ,fg)))
       ("01" "black"      (lambda () (interactive) (erc-irc-format--insert-color ,fg 1))  :description (lambda () (erc-irc-format--describe-color-black      ,fg)))
       ("02" "blue"       (lambda () (interactive) (erc-irc-format--insert-color ,fg 2))  :description (lambda () (erc-irc-format--describe-color-blue       ,fg)))
       ("03" "green"      (lambda () (interactive) (erc-irc-format--insert-color ,fg 3))  :description (lambda () (erc-irc-format--describe-color-green      ,fg)))]
      [("04" "red"        (lambda () (interactive) (erc-irc-format--insert-color ,fg 4))  :description (lambda () (erc-irc-format--describe-color-red        ,fg)))
       ("05" "brown"      (lambda () (interactive) (erc-irc-format--insert-color ,fg 5))  :description (lambda () (erc-irc-format--describe-color-brown      ,fg)))
       ("06" "magenta"    (lambda () (interactive) (erc-irc-format--insert-color ,fg 6))  :description (lambda () (erc-irc-format--describe-color-magenta    ,fg)))
       ("07" "orange"     (lambda () (interactive) (erc-irc-format--insert-color ,fg 7))  :description (lambda () (erc-irc-format--describe-color-orange     ,fg)))]
      [("08" "yellow"     (lambda () (interactive) (erc-irc-format--insert-color ,fg 8))  :description (lambda () (erc-irc-format--describe-color-yellow     ,fg)))
       ("09" "lightgreen" (lambda () (interactive) (erc-irc-format--insert-color ,fg 9))  :description (lambda () (erc-irc-format--describe-color-lightgreen ,fg)))
       ("10" "cyan"       (lambda () (interactive) (erc-irc-format--insert-color ,fg 10)) :description (lambda () (erc-irc-format--describe-color-cyan       ,fg)))
       ("11" "lightcyan"  (lambda () (interactive) (erc-irc-format--insert-color ,fg 11)) :description (lambda () (erc-irc-format--describe-color-lightcyan  ,fg)))]
      [("12" "lightblue"  (lambda () (interactive) (erc-irc-format--insert-color ,fg 12)) :description (lambda () (erc-irc-format--describe-color-lightblue  ,fg)))
       ("13" "pink"       (lambda () (interactive) (erc-irc-format--insert-color ,fg 13)) :description (lambda () (erc-irc-format--describe-color-pink       ,fg)))
       ("14" "grey"       (lambda () (interactive) (erc-irc-format--insert-color ,fg 14)) :description (lambda () (erc-irc-format--describe-color-grey       ,fg)))
       ("15" "lightgrey"  (lambda () (interactive) (erc-irc-format--insert-color ,fg 15)) :description (lambda () (erc-irc-format--describe-color-lightgrey  ,fg)))]]
     [:class transient-row
      ("s" "spoilers" (lambda () (interactive) (erc-irc-format--insert-color ,fg ,fg)))
      ("RET" (lambda () (interactive) (erc-irc-format--insert-color ,fg)) :description (lambda () (propertize "default" 'face (list (erc-get-fg-color-face ,fg)))))]))

;; Define background color prefixes
;;;###autoload (autoload 'erc-irc-format-color-white "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-white      0 "white")
;;;###autoload (autoload 'erc-irc-format-color-black "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-black      1 "black")
;;;###autoload (autoload 'erc-irc-format-color-blue "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-blue       2 "blue")
;;;###autoload (autoload 'erc-irc-format-color-green "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-green      3 "green")
;;;###autoload (autoload 'erc-irc-format-color-red "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-red        4 "red")
;;;###autoload (autoload 'erc-irc-format-color-brown "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-brown      5 "brown")
;;;###autoload (autoload 'erc-irc-format-color-magenta "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-magenta    6 "magenta")
;;;###autoload (autoload 'erc-irc-format-color-orange "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-orange     7 "orange")
;;;###autoload (autoload 'erc-irc-format-color-yellow "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-yellow     8 "yellow")
;;;###autoload (autoload 'erc-irc-format-color-lightgreen "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-lightgreen 9 "lightgreen")
;;;###autoload (autoload 'erc-irc-format-color-cyan "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-cyan       10 "cyan")
;;;###autoload (autoload 'erc-irc-format-color-lightcyan "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-lightcyan  11 "lightcyan")
;;;###autoload (autoload 'erc-irc-format-color-lightblue "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-lightblue  12 "lightblue")
;;;###autoload (autoload 'erc-irc-format-color-pink "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-pink       13 "pink")
;;;###autoload (autoload 'erc-irc-format-color-grey "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-grey       14 "grey")
;;;###autoload (autoload 'erc-irc-format-color-lightgrey "erc-irc-format" nil t)
(erc-irc-format--color-define-prefix erc-irc-format-color-lightgrey  15 "lightgrey")

;; Main transient menu
;;;###autoload (autoload 'erc-irc-format "erc-irc-format" nil t)
(transient-define-prefix erc-irc-format ()
  ["Foreground"
   [("00" "white"      erc-irc-format-color-white      :description erc-irc-format--describe-color-white)
    ("01" "black"      erc-irc-format-color-black      :description erc-irc-format--describe-color-black)
    ("02" "blue"       erc-irc-format-color-blue       :description erc-irc-format--describe-color-blue)
    ("03" "green"      erc-irc-format-color-green      :description erc-irc-format--describe-color-green)]
   [("04" "red"        erc-irc-format-color-red        :description erc-irc-format--describe-color-red)
    ("05" "brown"      erc-irc-format-color-brown      :description erc-irc-format--describe-color-brown)
    ("06" "magenta"    erc-irc-format-color-magenta    :description erc-irc-format--describe-color-magenta)
    ("07" "orange"     erc-irc-format-color-orange     :description erc-irc-format--describe-color-orange)]
   [("08" "yellow"     erc-irc-format-color-yellow     :description erc-irc-format--describe-color-yellow)
    ("09" "lightgreen" erc-irc-format-color-lightgreen :description erc-irc-format--describe-color-lightgreen)
    ("10" "cyan"       erc-irc-format-color-cyan       :description erc-irc-format--describe-color-cyan)
    ("11" "lightcyan"  erc-irc-format-color-lightcyan  :description erc-irc-format--describe-color-lightcyan)]
   [("12" "lightblue"  erc-irc-format-color-lightblue  :description erc-irc-format--describe-color-lightblue)
    ("13" "pink"       erc-irc-format-color-pink       :description erc-irc-format--describe-color-pink)
    ("14" "grey"       erc-irc-format-color-grey       :description erc-irc-format--describe-color-grey)
    ("15" "lightgrey"  erc-irc-format-color-lightgrey  :description erc-irc-format--describe-color-lightgrey)]]
  [:class transient-row :description "Effects"
   ("i"  "italic"      erc-irc-format-italicize        :description erc-irc-format--describe-italic)
   ("b"  "bold"        erc-irc-format-boldify          :description erc-irc-format--describe-bold)
   ("u"  "underline"   erc-irc-format-underline        :description erc-irc-format--describe-underline)])

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
