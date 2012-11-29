(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-1-prefix")
 '(display-battery-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(ido-enable-flex-matching t)
 '(ruby-deep-arglist nil)
 '(ruby-deep-indent-paren t)
 '(safe-local-variable-values (quote ((js-indent-level . 2))))
 '(save-place t nil (saveplace))
 '(sbt-program-name "java -jar /home/arnold/Downloads/sbt-launch.jar")
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(which-function-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "gray88"))) t))

(add-hook 'font-lock-mode-hook
            (function
             (lambda ()
               (setq font-lock-keywords
                     (append font-lock-keywords
                             '(("\t+" (0 'my-tab-face t))
                               ("^.\\{101,\\}$" (0 'my-long-line-face t))
                               ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

(global-cwarn-mode )
(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/site-lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/jdee/build/lisp") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/lal") load-path))
(setq load-path (cons (expand-file-name "~/.emacs.d/git-emacs") load-path))

(setq inhibit-startup-message t)


(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
 (interactive)
 (c-mode)
 (c-set-style "linux"))

(defun linux-c++-mode ()
 "C++ mode with big tabs.. "
 (interactive)
 (c++-mode)
 (c-set-style "linux") )

(defun linux-php-mode ()
  "PHP mode with big tabs"
  (interactive)
  (php-mode)
  (c-set-style "linux"))

(setq auto-mode-alist (cons '(".*\\.[ch]$" . linux-c-mode)
	auto-mode-alist))

(setq auto-mode-alist (cons '(".*\\.cpp$" . linux-c++-mode)
	auto-mode-alist))

(setq auto-mode-alist (cons '(".*\\.inc$" . linux-php-mode )
        auto-mode-alist))

(setq auto-mode-alist (cons '(".*\\.php$" . linux-php-mode )
        auto-mode-alist))


;;(set-default-font "dejavu sans mono:pixelsize=16")
;;(set-default-font "courier:pixelsize=16")
(set-default-font "bitstream vera sans mono:pixelsize=14")

;; for js
(setq-default tab-width 4)


;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/rhtml")

(require 'rhtml-mode nil t)

(ido-mode t)

(put 'downcase-region 'disabled nil)

(defalias 'rb 'revert-buffer)

(global-set-key "\C-x\C-b" nil)
(global-set-key "\C-x\C-k\r" nil)

(add-to-list 'load-path "~/.emacs.d/scamacs/scala")
(require 'scala-mode-auto nil t)


  (defun my-insert-file-name (filename &optional args)
    "Insert name of file FILENAME into buffer after point.

  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.

  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.

  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
    ;; Based on insert-file in Emacs -- ashawley 20080926
    (interactive "*fInsert file name: \nP")
    (cond ((eq '- args)
           (insert (file-relative-name filename)))
          ((not (null args))
           (insert (expand-file-name filename)))
          (t
           (insert filename))))


(setq-default indent-tabs-mode nil)
(setq make-backup-files nil)

(require 'yasnippet nil t)

(add-to-list 'load-path "~/.emacs.d/lal")
;; (require 'lal-add-import nil t)

;;(add-to-list 'lal-load-jars "/home/arnold/builds/LalAndroid/lib_managed/scala_2.8.1/compile/guice-2.0-no_aop.jar")
;; (add-to-list 'lal-load-jars "/home/arnold/builds/LalAndroid/lib_managed/scala_2.8.1/compile/roboguice-1.1.jar")
;; (add-to-list 'lal-load-jars "/home/arnold/builds/LalAndroid/target/scala_2.8.1/classes.min.jar")

;; give me some screen real estate.
(menu-bar-mode -1)

;; (require 'lal-commit-editmsg)

(setq ruby-deep-indent-paren nil)
(require 'git-emacs nil t)

;; use google-chrome as default browser
(setq browse-url-generic-program "/usr/bin/google-chrome"
      browse-url-browser-function 'browse-url-generic)



(global-set-key "\C-ck" 'upcase-last-word)
(global-set-key "\C-xk" 'kill-buffer)
(global-set-key "\C-cc" 'compile)

    ;; Draw tabs with the same color as trailing whitespace
    (add-hook 'font-lock-mode-hook
              (lambda ()
                (font-lock-add-keywords
                  nil
                  '(("\t" 0 'trailing-whitespace prepend)))))

(setq compilation-scroll-output 'first-error)

;; call this from the emacsclient session
;; (send-string-to-terminal (concat "\033]2;" "Emacsclient" "\007"))

(setq outline-regexp "[*\f]+")

(require 'cl)









