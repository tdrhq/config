;;; Compiled snippets and support files for `lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'lisp-mode
                     '(("template" "(in-package :${1:`(file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))`})\n\n(markup:enable-reader)\n\n$0\n" "template" nil nil nil "/home/arnold/config/snippets/lisp-mode/template.snippet" nil nil)
                       ("tag" "<$1 $2>\n  $0\n</$1>\n" "tag" nil nil nil "/home/arnold/config/snippets/lisp-mode/tag.snippet" nil nil)
                       ("package" "(in-package ${1:`(guess-lisp-package)`})\n\n$0" "package" nil nil nil "/home/arnold/config/snippets/lisp-mode/package.snippet" nil nil)
                       ("div" "<div class=\"$1\">\n  $0\n</div>" "div" nil nil nil "/home/arnold/config/snippets/lisp-mode/div.snippet" nil nil)
                       ("defpackage" "(defpackage :$1\n  (:use :cl\n        :alexandria`(if (string-prefix-p \"test-\" (file-name-nondirectory (buffer-file-name))) \"\\n:fiveam\" \"dd\")`$0))\n(in-package :$1)\n\n$0\n" "defpackage" nil nil nil "/home/arnold/config/snippets/lisp-mode/defpackage.snippet" nil nil)
                       ("def-suite" "(def-suite* ${1:`(guess-lisp-package-in-buffer)`})\n" "def-suite" nil nil nil "/home/arnold/config/snippets/lisp-mode/def-suite.snippet" nil nil)
                       ("clos" "($1 :accessor $1\n    :initarg :$1\n    :initform nil)" "clos-field" nil nil nil "/home/arnold/config/snippets/lisp-mode/clos-field.snippet" nil nil)))


;;; Do not edit! File generated at Sat Sep  5 17:45:59 2020
