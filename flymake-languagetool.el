;;; flymake-languagetool.el --- Flymake support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:37

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Flymake support for LanguageTool
;; Keyword: grammar check
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/emacs-languagetool/flymake-languagetool

;; This file is NOT part of GNU Emacs.

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
;;
;; Flymake support for LanguageTool.
;;

;;; Code:

(require 'flymake)

(defgroup flymake-languagetool nil
  "Flymake support for LanguageTool."
  :prefix "flymake-languagetool-"
  :group 'flymake
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/flymake-languagetool"))

(defcustom flymake-languagetool-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with LanguageTool."
  :type 'list
  :group 'flymake-languagetool)

(defcustom flymake-languagetool-commandline-jar ""
  "The path of languagetool-commandline.jar."
  :type '(file :must-match t))

(defcustom flymake-languagetool-language "en-US"
  "The language code of the text to check."
  :type '(string :tag "Language")
  :safe #'stringp)
(make-variable-buffer-local 'flymake-languagetool-language)

;;; Entry

;;;###autoload
(defun flymake-languagetool-load ()
  "Configure flymake mode to check the current buffer's grammar."
  (interactive)
  (setq flymake-grammarly--last-buffer-string (buffer-string))
  (flymake-grammarly--grammar-check)
  (add-hook 'after-change-functions #'flymake-grammarly--after-change-functions nil t)
  (add-hook 'flymake-diagnostic-functions #'flymake-grammarly--checker nil t))

;;;###autoload
(defun flymake-languagetool-maybe-load ()
  "Call `flymake-languagetool-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-languagetool-active-modes)
    (flymake-languagetool-load)))

(provide 'flymake-languagetool)
;;; flymake-languagetool.el ends here
