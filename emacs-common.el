;; emacs-common.el --- Common emacs configuration that I share across machines

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
(require 'compile)
(require 'yasnippet)

(add-to-list 'load-path (file-name-directory load-file-name))

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

(require 'notifications)

(defun compile-finished-notification (buf status)
  (message "Status is %s" status)
  (ignore-errors
      (notifications-notify :transient t :title
                            (if (string-match "finished" status)
                                "Compilation done"
                              "Compilation failed"))))


;;(defun compile-finished-notification (buf status))

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

(defun arnold/is-m-name (input)
  (let ((chr-0 (aref input 0))
              (chr-1 (aref input 1)))
    (and (equal chr-0 ?m)  (arnold/is-upcase chr-1))))

(defun arnold/upcase-first (input)
  (concat (char-to-string (upcase (aref input 0))) (substring input 1)))

(defun toggle-camel-str (input)
    (if (arnold/is-m-name input)
        (camelCasify-word (substring input 1))
      (concat "m" (arnold/upcase-first input))))

(defun arnold/find-constructor ()
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search nil))
    (re-search-forward (concat " " (file-name-base (buffer-file-name)) "("))
    (forward-char -1)))

(defun arnold/indent-line ()
  (indent-region (beginning-of-thing 'line) (end-of-thing 'line)))

(defun arnold/def-private (&optional class field)
  (interactive)
  (let* ((class (or class (arnold/read-classname)))
         (field (or field (toggle-camel-str class))))
    (save-excursion
      (arnold/find-constructor)
      (forward-line -1)
      (insert (format "private %s %s;" class field))
      (arnold/indent-line)
      (insert "\n"))))

(defun arnold/add-constructor-arg (classname var)
  (save-excursion
    (arnold/find-constructor)
    (arnold/add-call-arg (format "%s %s" classname var))))

(defun arnold/add-call-arg (text)
  (forward-sexp)
  (forward-char -1)
  (unless (eql (char-after (- (point) 1)) ?\( )
    (insert ",\n"))
  (insert text)
  (arnold/indent-line))

(defun arnold/field-def-snippet ()
  (yas-expand-snippet "$1 m${2:$1}"))

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

(defun arnold/read-classname ()
  (let* ((word (thing-at-point 'word))
         (guess (when word
                  (if (arnold/is-m-name word)
                      (substring word 1)
                    (arnold/upcase-first word)))))
    (read-string "Classname: " guess)))


(defun arnold/add-to-constructor (&optional classname field-name)
  (interactive)
  (save-excursion
    (let* ((classname (or classname (arnold/read-classname)))
           (field-name (or field-name (toggle-camel-str classname)))
           (var-name (toggle-camel-str field-name)))
      (arnold/def-private classname field-name)
      (arnold/add-constructor-arg classname var-name)
      (arnold/add-field-init-in-constructor field-name var-name))))

(defun arnold/add-field-to-constructor ()
  (interactive)
  (save-excursion
    (goto-char (line-beginning-position))
    (if (re-search-forward "\\([^ ]*\\) \\([^ ]*\\);" (line-end-position) t)
        (progn
          (let* ((classname (match-string 1))
                 (var-name (match-string 2))
                 (local-var-name (toggle-camel-str var-name)))
            (arnold/add-constructor-arg classname (toggle-camel-str var-name))
            (arnold/add-field-init-in-constructor var-name local-var-name))))))


(defun arnold/previous-class-member ()
  (re-search-backward "private .* \\(m[a-zA-Z0-9_]*\\);" nil t))

(defun arnold/class-member-name ()
  (match-string 1))

(defun arnold/find-class-members ()
  (save-excursion
    (arnold/find-constructor)
    (loop while (arnold/previous-class-member)
          collect (arnold/class-member-name))))

(defun arnold/chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun arnold/starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun arnold/line-is-assigning (name)
  (save-excursion
    (let ((line (arnold/chomp (thing-at-point 'line))))
      (arnold/starts-with line (concat name " = ")))))


(defun arnold/find-local-references (name)
  (save-excursion
    (arnold/find-constructor)
     (loop while (re-search-forward name nil t)
           if (progn
                (forward-char -1)
                (and
                 (equal (thing-at-point 'sexp) name)
                 (not (arnold/line-is-assigning name))))
           collect (point))))

(defun arnold/find-unused-fields ()
  (loop for field in (arnold/find-class-members)
        if (equal (length (arnold/find-local-references field)) 0)
        collect field))

(defun arnold/end-of-arglist-item ()
  (save-excursion
    (if (re-search-forward "[,)]" nil t)
        (- (point) 2))))

(defun arnold/arglist-bounds-of-items ()
  ;; we're at the first '('
  (let ((beg (point))
        (end (save-excursion (forward-sexp) (- (point) 1))))
    (loop while (< (point) end)
          collect (cons (point) (arnold/end-of-arglist-item))
          do (goto-char (+ (arnold/end-of-arglist-item) 2)))))

(defun arnold/delete-arglist-item (idx)
  (save-excursion
    (let ((bounds (nth idx (arnold/arglist-bounds-of-items))))
      (delete-region (car bounds) (+ 1 (cdr bounds)))
      (goto-char (car bounds))
      (if (equal ?, (char-after))
          (delete-char 1)))))

(defun arnold/delete-arglist-items (list)
  (loop for i in (sort list '>)
        do (arnold/delete-arglist-item i)))

(defun arnold/get-type-for-var-def ()
  "Get the type for the variable defined on the current line"
  (save-excursion
    (let ((line (thing-at-point 'line)))
      (nth 1 (reverse (split-string line))))))


(defun arnold/delete-field (field)
  (save-excursion
    (arnold/goto-field field)
    (delete-region (beginning-of-thing 'line) (end-of-thing 'line))))

(defun arnold/goto-field (&optional field)
  (interactive)
  (let (dest)
    (save-excursion
      (let ((field (or field (thing-at-point 'word))))
        (arnold/find-constructor)
        (loop while (arnold/previous-class-member)
              if (equal field (arnold/class-member-name))
              do (setq dest (point)))))
    (if dest
        (goto-char dest)
      (message "Could not find a field for %s" field))))

(defun arnold/get-field-type (&optional field)
  (interactive)
  (let ((field (or field (thing-at-point 'sexp))))
    (save-excursion
      (arnold/goto-field field)
      (when (called-interactively-p 'any)
        (message (arnold/get-type-for-var-def)))
      (arnold/get-type-for-var-def))))

(defun abgs ()
  (interactive)
  (let ((text (read-string "Search for: " (thing-at-point 'word))))
    (arnold/open-in-browser (format "https://our.intern.facebook.com/intern/biggrep/?corpus=fbandroid&filename=&case=false&view=default&extre=&s=%s&engine=apr_strmatch&context=false&filter[uninteresting]=false&filter[intern]=false&filter[test]=false" text))))

(defun arnold/delete-unused-fields ()
  (interactive)
  (loop for field in (arnold/find-unused-fields)
        do (arnold/delete-field field)))




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

(defun goto-last-link ()
  (message "going to last link")
  (let ((text (buffer-substring (point-min) (point-max))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (re-search-backward "ttp")
      (message "We're at: %s" (thing-at-point 'line))
      (goto-link))))

(defun goto-link-clever ()
  (interactive)
  (if (and (eq major-mode 'term-mode)
           (term-in-char-mode))
      (goto-last-link)
    (goto-link)))

(global-set-key "\C-cl" 'goto-link-clever)

(defun arnold-ansi-term-hook ()
  (local-set-key "\C-cl" 'goto-link-clever))

(add-hook 'term-mode-hook 'arnold-ansi-term-hook)


(defun open-last-phab ()
  (interactive)
  (save-excursion
    (when (re-search-backward "phabricator.fb.com/D" nil t)
      (goto-link))))



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

(defun revert-all-buffers (&optional even-modified)
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and
             (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (or even-modified (not (buffer-modified-p)))
             (not (string-suffix-p ".jar" (buffer-file-name))))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun truly-revert-all-buffers ()
  (interactive)
  (revert-all-buffers t))

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

  (browse-url url))


(defun arnold/only-make-request-data (data)
  (mapconcat (lambda (arg)
                 (concat (url-hexify-string (car arg))
                         "="
                         (url-hexify-string (cdr arg))))
             data
             "&"))

(defun arnold/trace-internal (name task-name is-end)
  (let ((url-request-method "POST")
        (request-data (arnold/only-make-request-data
                           (list
                            (cons "name"  name)
                            (cons "task_name" task-name)
                            (cons "ts" (format "%.0f" (float-time (current-time))))
                            (cons "is_end" (if is-end "true" "false"))))))
    (url-retrieve (format "http://www.tdrhq.com/eff-trace?%s" request-data)
                  (lambda (status)) nil t t)
    nil))

(defun arnold/start-trace (name task-name)
  (arnold/trace-internal name task-name nil))

(defun arnold/end-trace (name task-name)
  (arnold/trace-internal name task-name t))

;; (arnold/start-trace "foo" "foobar")
;; (arnold/end-trace "foo" "foobar")


(defun shell-mode-command (command)
  (goto-char (point-max))
  (term/insert (format "%s\n" command )))

(defun goto-dir ()
  (interactive)
  (let ((dir (file-name-directory (buffer-file-name (current-buffer)))))
    (switch-to-buffer "*ansi-term*")
    (shell-mode-command (concat "cd " dir))))


(defun arnold/delete-compilation ()
  (interactive)
  (loop for w in (window-list)
        if (let ((name (buffer-name (window-buffer w)))) (or (equal name "*Help*") (equal name "*compilation*")))
        do (delete-window w)))

(global-set-key "\C-cd" 'arnold/delete-compilation)

(defun refactor ()
  (interactive)
  (let ((add-constructor-arg "Add constructor arg") (replace-term "Replace term in buffer") (test-option "Test option"))
    (let ((request (popup-menu*
                    (list add-constructor-arg replace-term test-option)
                    :point (point))))
      (cond
       ((equal add-constructor-arg request) (arnold/add-to-constructor))
       ((equal test-option request) (message "got option"))
       ((equal replace-term request) (arnold/replace-term-in-buffer))
       (t (message "Deselected"))))))

(defun arnold/replace-term-in-buffer ()
  (let ((orig (thing-at-point 'word)) (replacement (read-string "Replace with: ")))
    (save-excursion
      (goto-char (point-min))
      (replace-string orig replacement))))


(defun arnold/get-compile-status ()
  (save-window-excursion
    (set-buffer "*compilation*")
    (let ((mode-line (if (listp mode-line-process) (second mode-line-process) mode-line-process)))
      (cond
       ((equal mode-line ":%s") 'run)
       ((equal mode-line ":exit [0]") 'success)
       (t 'error)))))

(defun compile-status ()
  (interactive)
  (message "%s" (arnold/get-compile-status)))

(defun break-up-params (&optional beg end)
  (interactive)
  (let ((beg2 (or beg (point)))
        (end2 (or end (line-end-position)))
        final-end)
    (save-excursion
      (while (search-forward "," end2 t)
        (replace-match ",\n" nil t)
        (c-indent-line-or-region)
        (setf end2 (+ 1 end2))))))

(defun break-up-params-line ()
  (interactive)
  (break-up-params (line-beginning-position)
                   (line-end-position)))


(defun arnold/arglist-cont (arg)
  (let ((pos (cdr arg)))
    (save-excursion
      (goto-char (line-beginning-position))
      (if (re-search-forward "^[ ]*\\." (line-end-position) t)
          '++))))

(defun arnold/add-arglist-cont ()
  (loop for style in '(arglist-cont arglist-cont-nonempty)
        do
        (let ((a (assoc style c-offsets-alist)))
          (setf (cdr a)
                (cons 'arnold/arglist-cont (cdr a))))))


(add-hook 'java-mode-hook 'arnold/add-arglist-cont)
(defun arnold/delete-compilation-error (name)
  (when compilation-error-regexp-alist-alist
    (setf compilation-error-regexp-alist-alist
	  (assq-delete-all name compilation-error-regexp-alist-alist )))
  (when compilation-error-regexp-alist
    (setf compilation-error-regexp-alist
	  (remove name compilation-error-regexp-alist))))


(defun arnold/add-compilation-error (name match-config)
  (arnold/delete-compilation-error name)
  (setf compilation-error-regexp-alist-alist
        (acons
         name
         match-config
         compilation-error-regexp-alist-alist))
  (setf compilation-error-regexp-alist
        (cons
         name
         compilation-error-regexp-alist))
  )

(defun arnold/read-all-sexp ()
  (save-excursion
    (goto-char (point-min))
    (let (sexp)
    (loop while (< (+ (point) 1) (point-max))
            collect (read (current-buffer))))))

(defun arnold/all-used-functions (sexps)
  (loop for sexp in (cons nil sexps)
        append (arnold/all-used-functions-r sexp)))

(defun arnold/all-used-functions-r (sexp)
  (when (and sexp (listp sexp))
    (let ((this-call (car sexp)))
      (if (listp this-call)
          (loop for item in sexp
                append (arnold/all-used-functions-r item))
        (cons this-call
              (loop for item in (cdr sexp)
                    append (arnold/all-used-functions-r item)))))))

(defun arnold/get-defuns (sexps)
  (loop for sexp in sexps
        if (eq (car sexp) 'defun)
        collect (second sexp)))

(defun arnold/find-unused-methods ()
  (interactive)
  (let ((defuned-methods (arnold/get-defuns (arnold/read-all-sexp)))
        (used-methods (arnold/all-used-functions (arnold/read-all-sexp))))
    (loop for defuned in defuned-methods
          if (not (member defuned used-methods))
          collect defuned)))

(defun term/insert (text)
  "similar to insert, but does the right thing in terminals"
  (term-send-raw-string text))


(defun arnold/empty-func ()
  (interactive)
  (message "Cannot save buffer"))

(defun arnold/disable-saving-on-buffer ()
  (interactive)
  (local-set-key (kbd "\C-x\C-s") 'arnold/empty-func))

(add-hook 'compilation-mode-hook 'arnold/disable-saving-on-buffer)
(add-hook 'lisp-interaction-mode 'arnold/disable-saving-on-buffer)
(add-hook 'term-mode-hook 'arnold/disable-saving-on-buffer)

(arnold/add-compilation-error
 'gradle-first-error-test
 '(":.*:.*WithJavac\\(.*\\):\\(.*\\): error:.*"
   1 2))

(arnold/add-compilation-error
 'gradle-first-error-test-javadoc
 '(":.*:.*androidJavadoc\\(.*\\):\\(.*\\): error:.*"
   1 2))

(arnold/add-compilation-error
 'gradle-first-error-test-for-kotlin
 '(":.*:.*Kotline: \\(.*.kt\\): (\\(.*\\), .*):.*"
   1 2))

(arnold/add-compilation-error
 'gradle-kotlin-regular-error
 '("e: \\(.*.kt\\): (\\(.*\\), .*):.*"
   1 2))


(arnold/add-compilation-error
 'gradle-first-error-xml
 '(":.*:mergeDebugResources\\(.*\\):\\(.*\\): .*error:.*"
   1 2))




(setf yas-snippet-dirs
      (cons
       (concat (getenv "HOME") "/config/snippets")
       yas-snippet-dirs))
