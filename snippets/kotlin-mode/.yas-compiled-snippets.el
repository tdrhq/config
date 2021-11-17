;;; Compiled snippets and support files for `kotlin-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'kotlin-mode
                     '(("test-template" "package `(lal-expected-package-name-from-buffername)`\n\nimport org.junit.Assert.*\n\npublic class `class-name` {\n  $0\n}\n" "test-template" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/kotlin-mode/test-template.snipppet" nil nil)
                       ("tc" "InstrumentationRegistry.getTargetContext()" "tc" nil nil nil "/home/arnold/config/snippets/kotlin-mode/tc.snippet" nil nil)
                       ("new-test" "@Test\nfun test$1() {\n  $0\n}" "new-test" nil nil nil "/home/arnold/config/snippets/kotlin-mode/new-test.snippet" nil nil)
                       ("log" "Log.i(\"`class-name`\", \"$0\")" "log" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/kotlin-mode/log.snippet" nil nil)
                       ("before" "@Before\nfun before() {\n  $0\n}\n" "before" nil nil nil "/home/arnold/config/snippets/kotlin-mode/before.snippet" nil nil)
                       ("after" "@After\nfun after() {\n  $0\n}\n" "after" nil nil nil "/home/arnold/config/snippets/kotlin-mode/after.snippet" nil nil)))


;;; Do not edit! File generated at Sun Nov 14 10:35:13 2021
