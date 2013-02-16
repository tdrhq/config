
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
(define-key *root-map* (kbd "C-e") "emacs-urxvt")


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

(in-package :stumpwm)
         
;; (toggle-mode-line (current-screen) (current-head))

(load "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(swank-loader:init)
(defcommand swank () ()
  (setf stumpwm:*top-level-error-action* :break)
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t)
  (echo-string (current-screen) "Starting swank."))
(define-key *root-map* (kbd "C-s") "swank")     

            
