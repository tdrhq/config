;;; Compiled snippets and support files for `lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'lisp-mode
                     '(("template" "(in-package :${1:`(file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name))))`})\n\n(markup:enable-reader)\n\n$0\n" "template" nil nil nil "/home/arnold/config/snippets/lisp-mode/template.snippet" nil nil)
                       ("tag" "<$1 $2>\n  $0\n</$1>\n" "tag" nil nil nil "/home/arnold/config/snippets/lisp-mode/tag.snippet" nil nil)
                       ("package" "(in-package ${1:`(guess-lisp-package)`})\n\n$0" "package" nil nil nil "/home/arnold/config/snippets/lisp-mode/package.snippet" nil nil)
                       ("mpl" ";;;; Copyright 2018-Present Modern Interpreters Inc.\n;;;;\n;;;; This Source Code Form is subject to the terms of the Mozilla Public\n;;;; License, v. 2.0. If a copy of the MPL was not distributed with this\n;;;; file, You can obtain one at https://mozilla.org/MPL/2.0/.\n" "mpl" nil nil nil "/home/arnold/config/snippets/lisp-mode/mpl.snippet" nil nil)
                       ("div" "<div class=\"$1\">\n  $0\n</div>" "div" nil nil nil "/home/arnold/config/snippets/lisp-mode/div.snippet" nil nil)
                       ("defpackage" "(pkg:define-package :$1\n  (:use #:cl\n        #:alexandria))\n\n$0" "defpackage" nil nil nil "/home/arnold/config/snippets/lisp-mode/defpackage.snippet" nil nil)
                       ("def-suite" "(def-suite* ${1:`(guess-lisp-package-in-buffer)`})\n" "def-suite" nil nil nil "/home/arnold/config/snippets/lisp-mode/def-suite.snippet" nil nil)
                       ("clos" "($1 :accessor $1\n    :initarg :$1\n    :initform nil)" "clos-field" nil nil nil "/home/arnold/config/snippets/lisp-mode/clos-field.snippet" nil nil)))


;;; Do not edit! File generated at Fri Jul 23 10:06:43 2021
