(setf kill-emacs-hook nil)

(load "java-mode")

(load-file "emacs-common.el")
(desktop-save-mode nil)
(defun desktop-kill ())
(save-buffers-kill-emacs)
