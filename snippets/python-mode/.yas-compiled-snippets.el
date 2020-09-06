;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("visi" "visibility = [\n    \"PUBLIC\",\n],\n" "visi" nil nil nil "/home/arnold/config/snippets/python-mode/visi.snippet" nil nil)
                       ("test-template" "from __future__ import absolute_import\nfrom __future__ import division\nfrom __future__ import print_function\nfrom __future__ import unicode_literals\n\nimport unittest\n\nclass $1(unittest.TestCase):\n    def setUp(self):\n        pass\n\n    def tearDown(self):\n        pass\n\n    ${0:pass}\n\nif __name__ == '__main__':\n    unittest.main()\n" "test-template" nil nil nil "/home/arnold/config/snippets/python-mode/test-template.snippet" nil nil)
                       ("script" "#!/usr/bin/env python\n\nfrom __future__ import absolute_import\nfrom __future__ import division\nfrom __future__ import print_function\nfrom __future__ import unicode_literals\n\nimport sys, scribe_logger\n\n$0\n\ndef main(argv):\n    sys.exit(argv)\n\n\nif __name__ == '__main__':\n    with scribe_logger.ErrorHandler():\n        sys.exit(main(sys.argv))\n" "script" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/arnold/config/snippets/python-mode/script.snippet" nil nil)
                       ("java" "java_library(\n    name = \"`package-name`\",\n    srcs = glob([\"*.java\"]),\n    visibility = [\n        \"PUBLIC\",\n    ],\n)" "java" nil nil
                        ((package-name
                          (file-name-base
                           (directory-file-name
                            (file-name-directory
                             (buffer-file-name))))))
                        "/home/arnold/config/snippets/python-mode/java.snippet" nil nil)
                       ("java-test" "java_test(\n  name = \"`package-name`\",\n  srcs = glob([\"*.java\"]),\n  deps = [\n    \"`(replace-regexp-in-string \"//javatests\" \"//java\" (arnold/guess-buck-target))`\",\n  ],\n  visibility = [\n    \"PUBLIC\",\n  ],\n)\n" "java-test" nil nil
                        ((package-name
                          (file-name-base
                           (directory-file-name
                            (file-name-directory
                             (buffer-file-name))))))
                        "/home/arnold/config/snippets/python-mode/java-test.yasnippet" nil nil)
                       ("jar" "prebuilt_jar(\n    name = \"$1\",\n    binary_jar = \"$1-$2.jar\",\n    source_jar = \"$1-$2-sources.jar\",\n    visibility = [\n        \"PUBLIC\",\n    ]\n)" "jar" nil nil nil "/home/arnold/config/snippets/python-mode/jar.yas" nil nil)
                       ("itest" "load(\"@xplat//configurations/buck/android:instrumentation_tests.bzl\", \"instrumentation_test\")\n\ninstrumentation_test(\n    name = \"`target-name`\",\n    srcs = glob([\"*.java\"]),\n    deps = [\n    ],\n)\n" "itest" nil nil
                        ((target-name
                          (file-name-nondirectory
                           (directory-file-name
                            (file-name-directory
                             (buffer-file-name))))))
                        "/home/arnold/config/snippets/python-mode/itest.snippet" nil nil)
                       ("buck-target" "fb_android_library(\n    name = \"`package-name`\",\n    srcs = glob([\"*.java\"]),\n    deps = [$0\n    ],\n    visibility = [\n        \"PUBLIC\",\n    ],\n)\n" "buck-target" nil nil
                        ((package-name
                          (file-name-base
                           (directory-file-name
                            (file-name-directory
                             (buffer-file-name))))))
                        "/home/arnold/config/snippets/python-mode/buck-target.yasnippet" nil nil)
                       ("aar" "android_prebuilt_aar(\n    name = \"$1\",\n    aar = \"$1-$2.aar\",\n    source_jar = \"$1-$2-sources.jar\",\n    visibility = [\n        \"PUBLIC\",\n    ],\n)\n" "android_prebuilt_aar" nil nil nil "/home/arnold/config/snippets/python-mode/android_prebuilt_aar.yas" nil nil)
                       ("*Backtrace*" "" "*Backtrace*" nil nil nil "/home/arnold/config/snippets/python-mode/*Backtrace*" nil nil)))


;;; Do not edit! File generated at Sat Sep  5 17:46:00 2020
