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

;;; Core

(defun flymake-languagetool--check-all (source-buffer)
  "Check grammar for SOURCE-BUFFER document."
  (let ((check-list '()))
    (dolist (data flymake-grammarly--point-data)
      (let* ((pt-beg (flymake-grammarly--grab-info data "highlightBegin"))
             (pt-end (flymake-grammarly--grab-info data "highlightEnd"))
             (exp (flymake-grammarly--grab-info data "explanation"))
             (card-desc (unless exp (flymake-grammarly--grab-info data "cardLayout groupDescription")))
             (desc (flymake-grammarly--html-to-text (or exp card-desc "")))
             (type (if exp (if (string-match-p "error" data) :error :warning) :warning)))
        (push (flymake-make-diagnostic source-buffer (1+ pt-beg) (1+ pt-end) type desc) check-list)))
    check-list))

;;; Flymake

(defvar flymake-languagetool--report-fnc nil
  "Record report function/execution.")

(defvar flymake-languagetool--source-buffer nil
  "Record source check buffer.")

(defun flymake-languagetool--report-once ()
  "Report with flymake after done requesting."
  (when (functionp flymake-languagetool--report-fnc)
    (funcall flymake-languagetool--report-fnc
             (flymake-languagetool--check-all flymake-languagetool--source-buffer))))

(defun flymake-languagetool--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (setq flymake-languagetool--report-fnc report-fn
        flymake-languagetool--source-buffer (current-buffer))
  (flymake-languagetool--report-once))

;;; Entry

;;;###autoload
(defun flymake-languagetool-load ()
  "Configure flymake mode to check the current buffer's grammar."
  (interactive)
  (setq flymake-grammarly--last-buffer-string (buffer-string))
  (flymake-grammarly--grammar-check)
  (add-hook 'after-change-functions #'flymake-grammarly--after-change-functions nil t)
  (add-hook 'flymake-diagnostic-functions #'flymake-languagetool--checker nil t))

;;;###autoload
(defun flymake-languagetool-maybe-load ()
  "Call `flymake-languagetool-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-languagetool-active-modes)
    (flymake-languagetool-load)))

(provide 'flymake-languagetool)
;;; flymake-languagetool.el ends here
