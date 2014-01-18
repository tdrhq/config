
(in-package :stumpwm)
(set-prefix-key (kbd "C-u"))


;; xlock
(stumpwm:define-key stumpwm:*top-map* (stumpwm:kbd "C-M-l") "run-shell-command xlock")
(define-key *top-map* (kbd "C-M-o") "run-shell-command fetchotp -x --username Google\\ Internal\\ 2Factor")
(define-key *root-map* (kbd "C-r") "loadrc")


;; Toggle trayer

(defmacro toggle-command (state-var command kill-command)
	     `(progn (if ,state-var
			 (run-shell-command ,kill-command)
		       (run-shell-command ,command))
		     (setf ,state-var (not ,state-var))))

(defmacro deftoggle (function-sym command kill-command)
  `(let (toggle-state)
    (defun ,function-sym ()
       (toggle-command toggle-state ,command ,kill-command))))


(deftoggle toggle-trayer "trayer --SetDockType false" "killall trayer")
(deftoggle toggle-xclock "xclock" "killall xclock")

;; (defvar *trayer-state* nil)

;; (defun toggle-trayer ()
;;  (toggle-command *trayer-state* "trayer --SetDockType false" "killall trayer"))

;; (defun toggle-trayer ()
;;  (if *trayer-state*
;;      (run-shell-command "killall trayer")
;;    (run-shell-command "trayer --SetDockType false"))
;;  (setf *trayer-state* (not *trayer-state*)))

(define-key *root-map* (kbd "C-t") "eval (toggle-trayer)")

;; redefine from windows to window list
(define-key *root-map* (kbd "w") "windowlist")
(define-key *root-map* (kbd "C-w") "windowlist")
(define-key *root-map* (kbd "c") "exec urxvt")


;; (let ((count 0)
  ;; (defun incrcount () (setf count (1+ count))))


;; frame preferences

(clear-window-placement-rules)
(define-frame-preference "Default"
  (0 t t :instance "google-chrome"))

(setf *window-format* "%m%n%s%2gg0c: %20t")



;; startup apps
;; wait, how do I safeguard these against loadrc?

;; (run-shell-command "gnome-volume-control-applet")
;; (run-shell-command "nm-applet --sm-disable")
;; (run-shell-command "xscreensaver &")
;; (run-shell-command "gnome-power-manager &")

;; volume control
(load-module "amixer")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-1-")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
(define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle")


;; get a warning red box around an unfocused window
(setf *window-border-style* :thick)
(set-unfocus-color "red")
(set-focus-color "grey")


;; C-z C-z shouldn't pull to current frame. No command should pull. That sucks.
(define-key *root-map* (kbd "C-z") "other-window")

;; xrandr commands, FYI
;; xrandr -q | grep "VGA1 connected" &&  xrandr --output VGA1 --left-of LVDS --preferred &
;; xrandr -q | grep "VGA-0 connected" ||bterm
;;  xrandr --output VGA-0 --off &

(setf *c-b-browser* (cons "conkeror" "Conkeror"))

(defcommand set-firefox-default () ()
            (setf *c-b-browser* (cons "firefox" "Firefox")))
(defcommand set-chrome-default () ()
            (setf *c-b-browser* (cons "google-chrome" "chrome")))
(set-chrome-default)

(defcommand google-chrome () ()
	    "Load google chrome"
	    (run-or-raise (car *c-b-browser*) (list :class (cdr *c-b-browser*))))

(defcommand emacs-urxvt () ()
	    "Load emacs"
	    (run-or-raise "emacsclient -c" '(:title "Emacsclient")))

(define-key *root-map* (kbd "C-b") "google-chrome")


(defun run-and-get-output (cmd &optional (args ()) (env ()))
  (let ((p (sb-ext:process-output (sb-ext:run-program cmd () :output :stream :wait t :search t :environment env))))
    (read-line p nil 'foo)))

(run-and-get-output "date")

(defun time-for-zone (zone)
  (run-and-get-output "date" () (list (concatenate 'string "TZ=" zone))))


(concatenate 'string "TZ=" "Asia/Calcutta")

(defcommand echo-date () ()
  "Display the date and time."
  (apply #'message "~a~%~a~%~a~%~a" (run-and-get-output "/usr/bin/acpi") (mapcar #'time-for-zone (list "America/Los_Angeles" "America/New_York" "Asia/Calcutta" ))))

(defcommand setup-thinkpad-kbd () ()
            " setup the think pad keyboard"
                        (run-and-get-output "/home/arnold/.local/bin/setup-thinkpad-keyboard.sh"))
(sb-posix:putenv  (concat "PATH=" (getenv "HOME") "/.local/bin:" (getenv "PATH")))

(defcommand dock () ()
            "first thing to do when you dock"
            (run-and-get-output "/home/arnold/.local/bin/setup-thinkpad-keyboard.sh")
            (run-and-get-output "/usr/bin/xrandr --output VGA1 --right-of LVDS1 --preferred"))

(setf *mouse-focus-policy* :click)


;; (toggle-mode-line (current-screen) (current-head))

;; (load "/usr/share/common-lisp/source/slime/swank-loader.lisp")
;; (swank-loader:init)
;; (defcommand swank () ()
;;   (setf stumpwm:*top-level-error-action* :break)
;;   (swank:create-server :port 4005
;;                        :style swank:*communication-style*
;;                        :dont-close t)
;;   (echo-string (current-screen) "Starting swank."))
(define-key *root-map* (kbd "C-s") "swank")

(setf (symbol-function 'screen-windows) #'stumpwm::screen-windows)


;; Start a swank server

(let ((swank-loader "/home/arnold/builds/slime/swank-loader.lisp"))
  (if (proble-file swank-loader)
      (load swank-loader)
    ;; Make sure emacs loads slime from quicklisp, eh?
    (ql:quickload "swank"))
  (swank-loader:init))

(defcommand swank () ()
  (swank:create-server :port 5005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))


(defun my-terminals ()
  (remove-if-not (lambda (x) (equal (stumpwm::window-class x) "URxvt")) (stumpwm::screen-windows (stumpwm:current-screen))))
(window-user-title (second (my-terminals)))


(defcommand move-next-urxvt () ()
  (run-or-raise "urxvt" '(:class "URxvt")))

(define-key *root-map* (kbd "C-c") "move-next-urxvt")


;;;; Define a custom editor
;; While most of the time I'm editing in Emacs, every now and then I'm
;; editing in a console, probably ssh-ing somewhere and using an emacs
;; over a console. These commands help me control which window is the
;; editor that is bound to C-uC-e

(define-window-slot "IS-EDITOR")

(defcommand toggle-is-editor () ()
  "Set the current window as an editor window"
  (setf (window-is-editor (current-window)) (not (window-is-editor (current-window)))))

(defun editors ()
  (remove-if-not (lambda (x) (window-is-editor x)) (screen-windows (current-screen))))

(defun move-to-next-editor ()
  (let* ((matches (editors))
         (other-matches (member (current-window) matches))

         (win (or
               (second other-matches)
               (first matches))))
         (if win
             (pull-window win))))

(defcommand move-to-editor () ()
  "Move to the next editor. You can mark an editor using
  set-is-editor. If no editors are found it tries to look up the first
  emacs window"
  (if (editors)
      (move-to-next-editor)
    (run-or-raise "emacsclient -c" '(:class "Emacs"))))

(define-key *root-map* (kbd "C-e") "move-to-editor")
