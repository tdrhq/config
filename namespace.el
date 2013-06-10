

(defun namespace-starts-with (s arg)
      "returns non-nil if string S starts with ARG.  Else nil."
      (cond ((>= (length s) (length arg))
             (string-equal (substring s 0 (length arg)) arg))
            (t nil)))

(defun namespace-rename (namespace x)
  (cond ((listp x) (mapcar (lambda (y) (namespace-rename namespace y)) x))
        ((symbolp x) 
         (if (namespace-starts-with (symbol-name x) "::")
             (intern (concat namespace (symbol-name x)))
           x))
         (t x)))

(defmacro with-namespace-internal (namespace &rest body)
  (let* ((form2 (cons 'progn body)))
    (mapcar (lambda (y) (namespace-rename namespace y)) form2)))


(macroexpand-1 (defun ff (x) (+ 1 x)))

(macroexpand (with-namespace-internal
 "foo"
 (defun ::xyz (x)
   (+ 1 x))
 (mapcar '::xyz '(1 2 3))))




;; more better syntax: we use a global *current-namespace* to indicate
;; the current namespace so that we can bind C-xC-e to
;; eval-with-namespace for faster iteration

(setf *current-namespace* "internal")

;; Note that this is a macro that is setting the namespace because it
;; needs to be set at compile time
(defmacro namespace-set (namespace)
  (namespace-set-internal (symbol-name namespace)))

(namespace-set foo-bar)

(defmacro in-namespace (&rest body)
  `(with-namespace-internal ,*current-namespace* ,@body))

(macroexpand '(in-namespace 
 (setf ::bye 2)
 (setf ::cy 3)))


  













