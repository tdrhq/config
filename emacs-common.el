;;; emacs-common.el --- Common emacs configuration that I share across machines

;; Copyright (C) 2012  Arnold Noronha

;; Author: Arnold Noronha <arnold@dev138.ash4.facebook.com>
;; Keywords:

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

;;

;;; Code:

(require 'cl)

(ido-mode t)
(setq ido-enable-flex-matching t)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-c" 'compile)

;; do the same for C++ mode
(add-hook 'c++-mode-hook
          (lambda ()
            (interactive)
            (local-set-key "\C-c\C-c" 'compile)))

(global-set-key "\C-x\C-c"
                (lambda ()
                  (interactive)
                  (message "Sorry, I can't quit that way")))

;; rename the current file and buffer
(defun rename-bf (to)
  (interactive "FNew filename: ")
  "rename the file in the current buffer, and reopen it"
  (let ((file (buffer-file-name)))
    (if (file-exists-p file)
	(rename-file file to))
    (set-visited-file-name to)))


(defun arnold-term-char-mode ()
  (interactive)
  (term-char-mode)
  (end-of-buffer))

(defun arnold-term-mode-hooks ()
  (local-set-key "\C-c\C-k" 'arnold-term-char-mode))

(add-hook 'term-mode-hook 'arnold-term-mode-hooks)


(defun arnold-set-buffer-compile-command (command)
  (interactive "Fcommand ")
  (set (make-local-variable 'compile-command)
                       command))

(defun arnold-set-compile-command  (on-hook root command)
  (lexical-let ((root root) (command command))
    (add-hook on-hook
              (lambda ()
                (interactive)
                (if (string-prefix-p (expand-file-name root) (expand-file-name (buffer-file-name)))
                    ;; ok, this is a match, set the compile command to the given command
                    (arnold-set-buffer-compile-command command)
                  )))))

(setenv "EDITOR" "emacsclient")









