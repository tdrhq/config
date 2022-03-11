
(in-package :stumpwm)
(set-prefix-key (kbd "C-u"))

(ql:quickload "str")

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

(defvar *focus-mode* nil)

(defcommand focus-mode () ()
  (setf *focus-mode* (not *focus-mode*)))

(deftoggle toggle-trayer "/usr/bin/trayer --SetDockType false" "killall trayer")
(deftoggle toggle-xclock "xclock" "killall xclock")

;; (defvar *trayer-state* nil)

;; (defun toggle-trayer ()
;;  (toggle-command *trayer-state* "trayer --SetDockType false" "killall trayer"))

;; (defun toggle-trayer ()
;;  (if *trayer-state*
;;      (run-shell-command "killall trayer")
;;    (run-shell-command "trayer --SetDockType false"))
;;  (setf *trayer-state* (not *trayer-state*)))

(define-key *root-map* (kbd "C-t") "eval (stumpwm::toggle-trayer)")

;; redefine from windows to window list
(define-key *root-map* (kbd "w") "windowlist")
(define-key *root-map* (kbd "C-w") "windowlist")
(define-key *root-map* (kbd "c") "exec urxvt")

(defun is-personal-laptop ()
  (equal (machine-instance) "arnold-laptop"))

(defun is-desktop ()
  (equal (machine-instance) "ThinkCenter"))

;; (let ((count 0)
  ;; (defun incrcount () (setf count (1+ count))))


;; frame preferences

(defcommand make-standard-preference () ()
  (clear-window-placement-rules)
  (define-frame-preference "Default"
      (0 t t :instance "firefox"))

  (define-frame-preference "Default"
      (0 t t :instance "emacs")))

(defcommand make-split-screen-preference () ()
  (clear-window-placement-rules)
  (define-frame-preference "Default"
      (0 t t :instance "firefox"))

  (define-frame-preference "Default"
      (1 t t :instance "emacs")))

(setf *window-format* "%m%n%s%2gg0c: %20t")

(message "loading shit")


;; startup apps
;; wait, how do I safeguard these against loadrc?

;; (run-shell-command "gnome-volume-control-applet")
;; (run-shell-command "nm-applet --sm-disable")
;; (run-shell-command "xscreensaver &")
;; (run-shell-command "gnome-power-manager &")

;; volume control

;;(unless (is-personal-laptop)
;;  (load-module "amixer")
;;  (define-key *top-map* (kbd "XF86AudioLowerVolume") "amixer-Master-1-")
;;  (define-key *top-map* (kbd "XF86AudioRaiseVolume") "amixer-Master-1+")
;;  (define-key *top-map* (kbd "XF86AudioMute") "amixer-Master-toggle"))


;; get a warning red box around an unfocused window
(setf *window-border-style* :none)
(set-unfocus-color "red")
(set-focus-color "grey")


;; C-z C-z shouldn't pull to current frame. No command should pull. That sucks.
(define-key *root-map* (kbd "C-z") "other-window")

(message "define first key and stuff")

;; xrandr commands, FYI
;; xrandr -q | grep "VGA1 connected" &&  xrandr --output VGA1 --left-of LVDS --preferred &
;; xrandr -q | grep "VGA-0 connected" ||bterm
;;  xrandr --output VGA-0 --off &

(setf *c-b-browser* (cons "conkeror" "Conkeror"))

(defcommand set-firefox-default () ()
            (setf *c-b-browser* (cons "firefox" "Firefox")))
(defcommand set-chrome-default () ()
            (setf *c-b-browser* (cons "google-chrome" "chrome")))
(defcommand set-iceweasel-default () ()
            (setf *c-b-browser* (cons "firefox" "Iceweasel")))

(set-firefox-default)

(defcommand google-chrome () ()
  "Load google chrome"
  (if *focus-mode*
      (message "FOCUS!")
      (unless (%next-browser)
        (run-or-raise (car *c-b-browser*) (list :class (cdr *c-b-browser*))))))

(defcommand emacs-urxvt () ()
	    "Load emacs"
	    (run-or-raise "emacsclient -c" '(:title "Emacsclient")))

(define-key *root-map* (kbd "C-b") "google-chrome")

(defcommand xchat () ()
  "jump to xchat"
  (run-or-raise "xchat" (list :class "Xchat")))

(define-key *root-map* (kbd "C-x") "xchat")



(defcommand screensaver () ()
  (message "running screensaver")
  (run-shell-command "xscreensaver-command -l || gnome-screensaver-command -l"))

(defun read-lines (stream)
  (let ((line (read-line stream nil)))
    (when line
      (cons (format nil "~A~%" line) (read-lines stream)))))

(defun run-and-get-output (cmd &optional (args ()) (env ()))
  (with-open-stream
   (p (sb-ext:process-output (sb-ext:run-program cmd () :output :stream :wait t :search t :environment env)))
   (apply #'concatenate 'string (read-lines p))))



(run-and-get-output "date")

(defun time-for-zone (zone)
  (run-and-get-output "date" () (list (concatenate 'string "TZ=" zone))))


(concatenate 'string "TZ=" "Asia/Calcutta")

(defcommand echo-date () ()
  "Display the date and time."
  (apply #'message "~a:~%NYC :~a~%~%CA  :~a~%IST :~a" (run-and-get-output "/usr/bin/acpi") (mapcar #'time-for-zone (list "America/New_York" "America/Los_Angeles" "Asia/Calcutta" ))))

(defcommand setup-thinkpad-kbd () ()
            " setup the think pad keyboard"
                        (run-and-get-output "/home/arnold/.local/bin/setup-thinkpad-keyboard.sh"))
(sb-posix:putenv  (concat "PATH=" (getenv "HOME") "/.local/bin:" (getenv "PATH")))

(message "just before dock")

(defcommand dock () ()
            "first thing to do when you dock"
            (run-and-get-output "/home/arnold/.local/bin/setup-thinkpad-keyboard.sh")
            (run-and-get-output "/usr/bin/xrandr --output VGA1 --right-of LVDS1 --preferred"))

(setf *mouse-focus-policy* :click)



(message "just before myterminal")

(defun my-terminals ()
  (remove-if-not (lambda (x) (equal (stumpwm::window-class x) "URxvt")) (stumpwm::screen-windows (stumpwm:current-screen))))

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

(message "just before editors")
(defun editors ()
  (remove-if-not (lambda (x) (window-is-editor x)) (screen-windows (current-screen))))

(defun all-browsers ()
  (loop for w in (sort (screen-windows (current-screen)) #'< :key 'window-number)
     if (let ((class (string-downcase (window-class w))))
          (or (str:containsp "firefox" class)
              (str:containsp "google-chrome" class)))
     collect
     w))

(defun %next-browser ()
  (let* ((windows (all-browsers))
         (cur-pos (position (current-window) windows)))
    (when windows
      (if (not cur-pos)
        (pull-window (car windows))
        (pull-window (elt windows (mod (+ cur-pos 1) (length windows)))))
      t)))

(defcommand move-next-browser () ()
  (%next-browser))

(setf *screen-mode-line-format*
      "[^B%n^b] %d %W" )

(setf *time-modeline-string*
      "%a %b %e %k:%M")

(defun find-next-editor ()
  (let* ((matches (editors))
         (other-matches (member (current-window) matches))

         (win (or
               (second other-matches)
               (first matches))))
    win))

(defun move-to-next-editor ()
  (if (find-next-editor)
      (pull-window (find-next-editor))))

(message "just before move-to-editor")

(defcommand move-to-editor () ()
  "Move to the next editor. You can mark an editor using
  set-is-editor. If no editors are found it tries to look up the first
  emacs window"
  (if (editors)
      (move-to-next-editor)
    (run-or-raise "emacsclient -c" '(:class "Emacs"))))

(defcommand move-to-monitor () ()
  (run-or-raise "monitor" '(:class "Monitor")))

(defcommand move-to-thunderbird () ()
  (run-or-raise "thunderbird" '(:class "Thunderbird")))

(define-key *root-map* (kbd "C-d") "move-to-monitor")

(define-key *root-map* (kbd "C-i") "move-to-thunderbird-2")

(defcommand fix-titles () ()
  (loop for w in (screen-windows (current-screen))
        do
        (if (equal (window-class w) "Thunderbird")
            (setf (window-user-title w) "Thunderbird"))
        (if (equal (window-class w) "Firefox")
            (setf (window-user-title w) "Firefox"))
        (if (equal (window-class w) "Monitor")
            (setf (window-user-title w) "Monitor"))))


(message "just before first define-key")

(define-key *root-map* (kbd "C-e") "move-to-editor")

(message "after first define-key")


;; Start a swank server. Since this is brittle (because of the
;; existence of external slime repo), keep this at the absolute bottom


(message "just before loading-swank")



(define-key *root-map* (kbd "C-s") "swank")


(message "before cleanup windows")
(defcommand cleanup-windows () ()

  ;; clean up "bad" windows... at the time of writing just "unnamed
  ;; device" windows

  (loop for window in (screen-windows (current-screen))
        do (when (equal (window-class window) "Nautilus")
             (delete-window window))))


(define-key *root-map* (kbd "L") "screensaver")

(message "the end")

(defun load-swank ()
  (load-swank-step-2))

(defun load-swank-step-2 ()
   (ql:quickload "slynk")  
   (uiop:call-function "slynk:create-server" :port 5005
                       :dont-close t)
  (echo-string (current-screen)
               "Starting swank. M-x slime-connect RET RET, then (in-package stumpwm)."))


(defcommand swank () ()
  (load-swank))

(defcommand kill-all-urxvt () ()
  (mapc 'stumpwm:kill-window (my-terminals)))
