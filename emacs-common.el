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
    (if (file-directory-p to)
        (setq to (concat to "/" (file-name-nondirectory file))))
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
  ;;(c-set-offset 'arglist-intro '+)
  )

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
  (let ((word (thing-at-point 'symbol)))
    (let ((cap (upcase word)))
      (message "in here with %s" cap)
      (backward-delete-char (length word))
      (insert cap))))


(global-set-key "\C-ck" 'all-caps-last-word)

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

(require 'stripes)

(global-unset-key (kbd "\C-xm"))

(defun camelCasify-word (word)
  (if (< (length word) 1)
      word
    (concat (downcase (substring word 0 1)) (substring word 1))))

(defun arnold/is-upcase (chr)
  (equal chr (upcase chr)))

(defun toggle-camel-str (input)
  (let ((chr-0 (aref input 0))
        (chr-1 (aref input 1)))
    (if (and (equal chr-0 ?m)  (arnold/is-upcase chr-1))
        (camelCasify-word (substring input 1))
      (concat "m" (char-to-string (upcase (aref input 0))) (substring input 1)))))

(defun arnold/find-constructor ()
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward (concat " " (file-name-base (buffer-file-name)) "("))
    (forward-char -1)))

(defun arnold/indent-line ()
  (indent-region (beginning-of-thing 'line) (end-of-thing 'line)))

(defun arnold/def-private (class field)
  (save-excursion
    (arnold/find-constructor)
    (forward-line -1)
    (insert (format "private %s %s;" class field))
    (arnold/indent-line)
    (insert "\n")))

(defun arnold/add-constructor-arg (classname var)
  (save-excursion
    (arnold/find-constructor)
    (forward-sexp)
    (forward-char -1)
    (insert-string ",\n")
    (insert-string (format "%s %s" classname var) )
    (arnold/indent-line)))

(defun arnold/add-field-init-in-constructor (field-name var-name)
  (save-excursion
    (arnold/find-constructor)
    (forward-sexp)
    (forward-sexp) ;; now we're at '}'
    (forward-char -1)

    (save-excursion
      (insert (format "%s = %s;\n" field-name var-name)))

    (arnold/indent-line)
    (arnold/indent-line)))

(defun arnold/add-to-constructor (classname &optional field-name)
  (save-excursion
    (let* ((field-name (or field-name (toggle-camel-str classname)))
           (var-name (toggle-camel-str field-name)))
      (arnold/def-private classname field-name)
      (arnold/add-constructor-arg classname var-name)
      (arnold/add-field-init-in-constructor field-name var-name))))


(defun toggle-camel ()
  (interactive)
  (let ((word (thing-at-point 'word))
        (beg (beginning-of-thing 'word))
        (end (end-of-thing 'word)))
    (goto-char beg)
    (delete-char (- end beg))
    (insert (toggle-camel-str word))))

(defun goto-link ()
  (interactive)
  (arnold/open-in-browser (thing-at-point 'url)))

(global-set-key "\C-cl" 'goto-link)

(defun open-last-phab ()
  (interactive)
  (save-excursion
    (re-search-backward "http://phabricator.fb.com/" nil t)
    (goto-link)))



;; setup ERC (IRC for emacs)
(require 'tls)

(autoload 'erc-nick-notify-mode "erc-nick-notify"
  "Minor mode that calls `erc-nick-notify-cmd' when his nick gets
mentioned in an erc channel" t)
(eval-after-load 'erc '(erc-nick-notify-mode t))

(defun recent-files-in-ansi-term ()
  (interactive)
  (unwind-protect
      (remove-if-not
       'arnold/valid-file-p
       (save-excursion
         (term-line-mode)
         (cl-loop for i from 1 to 50
                  collect
                  (progn
                   (forward-line -1)
                   (end-of-line)
                   (thing-at-point 'filename)))))
    (term-char-mode)))

(defun arnold/valid-file-p (file)
  (if (and file (not (string= file "")))
      (file-exists-p file)))


(defun open-recent-file-from-ansi-term ()
  (interactive)
  (let ((files (recent-files-in-ansi-term)))
    (find-file (ido-completing-read "Select file: " files))))

(desktop-save-mode 1)

;; java @override indentation
(add-hook 'java-mode-hook
          '(lambda () "Treat Java 1.5 @-style annotations as comments."
             (setq c-comment-start-regexp
                   "\\(@\\|/\\(/\\|[*][*]?\\)\\)")
             (modify-syntax-entry ?@ "< b"
                                  java-mode-syntax-table)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

  (defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )

(add-to-list 'auto-mode-alist '("BUCK$" . python-mode))

(defun arnold/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun set-emulator-serial ()
  (interactive)
  (let ((key "ANDROID_SERIAL"))
    (setenv key)
    (setenv "ANDROID_SERIAL" (arnold/chomp (shell-command-to-string "adb -e get-serialno")))))

(defun ansi-term-parse-directory ()
  (save-excursion
    (end-of-buffer)
    (if (re-search-backward "arnold@.*:\\(.*\\)$ " nil t)
        (match-string 1)
      "~")))

(defun ansi-term-with-line-mode (method)
  (let ((was-char-mode (term-in-char-mode)))
    (unwind-protect
      (progn
       (term-line-mode)
       (funcall method))
      (if was-char-mode
          (term-char-mode)))))


(defun ansi-term-get-directory ()
  (interactive)
  (ansi-term-with-line-mode
   (lambda ()
     (ansi-term-parse-directory))))

(defun arnold/open-in-browser (url)
  (interactive)

  (start-process "firefox" nil "firefox" url))
