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

(add-to-list 'load-path "~/config")

(ido-mode t)
(setq ido-enable-flex-matching t)

(global-set-key "\C-x\C-m" 'execute-extended-command)

;; do the same for C++ mode
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (local-set-key "\C-c\C-c" 'my-compile-c)))

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




(defun ssh-agent-start ()
  (interactive)
  (with-temp-buffer
    (call-process "sh" nil t nil "-c" "eval `ssh-agent` > /dev/null; echo $SSH_AUTH_SOCK; echo $SSH_AGENT_PID")
    ;; first line is the ssh-auth-sock
    (beginning-of-buffer)
    (setenv "SSH_AUTH_SOCK" (substring (thing-at-point 'line) 0 -1))
    (next-line)
    (setenv "SSH_AGENT_PID" (thing-at-point 'word))))

(defun compile-end-of-buffer ()
  (message "entering compile-end-of-buffer"
  (run-at-time "3 secs" nil
               (lambda ()
                 (message "running hook")
                 (with-selected-window (get-buffer-window "*compilation*")
                   (end-of-buffer))))))

(defun compilec (cmd)
  (interactive "sFoo: ")
  (compile cmd)
  (compile-end-of-buffer))

(defun arg-list-intro-setup ()
  (c-set-offset 'arglist-intro '+))

(add-hook 'java-mode-hook 'arg-list-intro-setup)

(global-set-key "\C-cc" 'compile)

(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

(setq make-backup-files nil)

  (defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))

(recentf-mode 1)
(setq recentf-max-saved-items 300)

;; backup files suck
(setq make-backup-files nil)


(tool-bar-mode -1)
(menu-bar-mode -1)


(defun all-caps-last-word ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (let ((cap (upcase word)))
      (message "in here with %s" cap)
      (backward-kill-word 1)
      (insert cap)))
  )

(ert-deftest all-caps-last-word ()
  (with-temp-buffer
    (insert "foo bar")
    (save-excursion
      (insert " too"))
    (all-caps-last-word)
    (should (equal "foo BAR too" (buffer-string)))))


(add-hook 'java-mode
          (lambda ()
            (local-set-key (kbd "\C-ck") 'all-caps-last-word)))


(defun gnome-notify (message)
  (shell-command (format "notify-send '%s'" message)))

(defun compile-finished-notification (buf status)
  (message "Status is %s" status)
  (gnome-notify
         (if (string-match "finished" status)
             "Compilation done"
           "Compilation failed")))

(add-hook 'compilation-finish-functions
          'compile-finished-notification)

;; remove trailing whitespace always
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(require 'uniquify)

(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator "-")


;; prettyfy json
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))
