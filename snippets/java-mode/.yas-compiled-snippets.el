;;; Compiled snippets and support files for `java-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'java-mode
                     '(("throwre" "throw new RuntimeException(${1:e});" "throwre" nil nil nil "/home/arnold/config/snippets/java-mode/throwre.snippet" nil nil)
                       ("tests-for" "/**\n * Tests {@link `(substring class-name 0 -4)`}\n */" "tests-for" nil nil
                        ((package-name
                          (replace-regexp-in-string "[/]" "."
                                                    (directory-file-name
                                                     (file-relative-name
                                                      (file-name-directory
                                                       (buffer-file-name))
                                                      "~/fbandroid"))))
                         (class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/java-mode/tests-for.snippet" nil nil)
                       ("test-template" "// (c) 2018 Arnold Noronha <arnold@tdrhq.com>\n\npackage `(lal-expected-package-name-from-buffername)`;\n\n`(if is-itest \"import android.support.test.runner.AndroidJUnit4;\n\nimport org.junit.runner.RunWith;\n\" \"\")`import static org.junit.Assert.*;\n\n`(if is-itest \"@RunWith(AndroidJUnit4.class)\n\" \"\")`public class `class-name` {\n  $0\n\n}" "test-template" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name))))
                         (is-itest
                          (string-match ".*instrumentation_test.*"
                                        (buffer-file-name))))
                        "/home/arnold/config/snippets/java-mode/test-template.yasnippet" nil nil)
                       ("test-setup" "@Override\npublic void setUp() throws Exception {\n  super.setUp();\n  $0\n}\n\n@Override\npublic void tearDown() throws Exception {\n  super.tearDown();\n}" "test-setup" nil nil nil "/home/arnold/config/snippets/java-mode/test-setup.yasnippet" nil nil)
                       ("test-imports" "import static org.mockito.Mockito.verify;\nimport static org.mockito.Mockito.eq;\nimport static org.mockito.Mockito.when;\nimport static org.mockito.Mockito.mock;\nimport static org.mockito.Mockito.any;\nimport org.junit.Before;\nimport org.junit.After;\nimport org.junit.Test;\nimport org.junit.Rule;\nimport static org.junit.Assert.fail;\n" "test-imports" nil nil nil "/home/arnold/config/snippets/java-mode/test-imports.snippet" nil nil)
                       ("tc" "InstrumentationRegistry.getTargetContext()" "tc" nil nil nil "/home/arnold/config/snippets/java-mode/tc.yasnippet" nil nil)
                       ("stack" "Object[] args, int start, int length" "stack" nil nil nil "/home/arnold/config/snippets/java-mode/stack.snippet" nil nil)
                       ("sleep" "try {\n  Thread.sleep($1);\n} catch (InterruptedException e) {\n  throw new RuntimeException(e);\n}\n" "sleep" nil nil nil "/home/arnold/config/snippets/java-mode/sleep.yasnippet" nil nil)
                       ("set-get" "private ${1:TypeName} m${2:$1};\n\npublic void set$2($1 ${2:$(camelCasify-word yas-text)}) {\n   m$2 = ${2:$(camelCasify-word yas-text)};\n}\n\npublic $1 get$2() {\n  return m$2;\n}\n" "set-get" nil nil nil "/home/arnold/config/snippets/java-mode/set-get.yas" nil nil)
                       ("serial" "private static final long serialVersionUID = `key`L;" "serial" nil nil
                        ((key
                          (random)))
                        "/home/arnold/config/snippets/java-mode/serial.snippet" nil nil)
                       ("runnable" "new Runnable() {\n  @Override\n  public void run() {\n    $0\n  }\n}\n" "runnable" nil nil nil "/home/arnold/config/snippets/java-mode/runnable.yasnippet" nil nil)
                       ("private-method" "private ${1:void} ${2:name}(${3:arguments}) {\n  $0\n}\n" "private-method" nil nil
                        ((yas-indent-line 'auto))
                        "/home/arnold/config/snippets/java-mode/private-method.yasnippet" nil nil)
                       ("priv" "private ${1:TypeName} ${2:$1$(camelCasify-word yas-text)};\n" "priv" nil nil nil "/home/arnold/config/snippets/java-mode/priv.yas" nil nil)
                       ("onuithread" "UiThreadHelper.runOnUiThread(new Runnable() {\n  @Override\n  public void run() {\n    $0\n  }\n});" "onuithread" nil nil
                        ((yas-indent-line 'auto))
                        "/home/arnold/config/snippets/java-mode/onuithread.yasnippet" nil nil)
                       ("new-test" "@Test\npublic void test${1:NameOfTest}() throws Throwable {\n  $0\n}" "new-test" nil nil
                        ((yas-indent-line 'auto))
                        "/home/arnold/config/snippets/java-mode/new-test.yasnippet" nil nil)
                       ("new-module" "// Copyright 2004-present Facebook. All Rights Reserved.\n\npackage com.facebook.places.checkin.activity;\n\nimport com.facebook.inject.AbstractLibraryModule;\n\npublic class `class-name` extends AbstractLibraryModule {\n  @Override\n  protected void configure() {\n    $0\n  }\n}\n" "new-module" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/java-mode/new-module.snippet" nil nil)
                       ("measure" "ViewHelpers.setupView(${1:mView})\n     .setExactWidthDp($2)\n     .layout()\n     .dispatchPreDraw();\n" "measure" nil nil nil "/home/arnold/config/snippets/java-mode/measure.snippet" nil nil)
                       ("log" "Log.i(\"`class-name`\", \"$0\");" "log" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/java-mode/log.snippet" nil nil)
                       ("jdoc" "/**\n * $0\n */" "jdoc" nil nil
                        ((yas-indent-line 'auto))
                        "/home/arnold/config/snippets/java-mode/jdoc.yasnippet" nil nil)
                       ("import-matchers" "import static org.hamcrest.Matchers.*;\n" "import-matchers" nil nil nil "/home/arnold/config/snippets/java-mode/import-matchers.snippet" nil nil)
                       ("ignore" "@org.junit.Ignore(\"$0\")" "ignore" nil nil nil "/home/arnold/config/snippets/java-mode/ignore.snippet" nil nil)
                       ("design" "android.support.design." "design" nil nil nil "/home/arnold/config/snippets/java-mode/design.yas" nil nil)
                       ("callable" "new Callable<$1>() {\n  @Override\n  public $1 call() {\n    $0\n  }\n}\n" "callable" nil nil nil "/home/arnold/config/snippets/java-mode/callable.yasnippet" nil nil)
                       ("blog-tag" "private static final Class<?> TAG = `class-name`.class;\n" "blog-tag" nil nil
                        ((class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/java-mode/blog-tag.snippet" nil nil)
                       ("before" "@Before\npublic void before() throws Throwable {\n  $0\n}\n" "before" nil nil nil "/home/arnold/config/snippets/java-mode/before.yas" nil nil)
                       ("arg" "args[start + $0]" "arg" nil nil nil "/home/arnold/config/snippets/java-mode/arg.snippet" nil nil)
                       ("appc" "android.support.v7.appcompat." "appc" nil nil nil "/home/arnold/config/snippets/java-mode/appc.yas" nil nil)
                       ("anon" "new ${1:name}($2) {\n  @Override\n  public void $3() {\n    $0\n  }\n}\n" "anon" nil nil nil "/home/arnold/config/snippets/java-mode/anon.yasnippet" nil nil)
                       ("android-template" "// (c) 2018 Arnold Noronha <arnold@tdrhq.com>\n\npackage `(lal-expected-package-name-from-buffername)`;\n\npublic class `class-name` {\n  public `class-name`() {\n  }\n\n  $0\n}\n" "android-template" nil nil
                        ((package-name
                          (replace-regexp-in-string "[/]" "."
                                                    (directory-file-name
                                                     (file-relative-name
                                                      (file-name-directory
                                                       (buffer-file-name))
                                                      "~/fbandroid"))))
                         (class-name
                          (file-name-sans-extension
                           (file-name-nondirectory
                            (buffer-file-name)))))
                        "/home/arnold/config/snippets/java-mode/android-template-yasnippet" nil nil)
                       ("after" "@After\npublic void after() throws Throwable {\n  $0\n}" "after" nil nil nil "/home/arnold/config/snippets/java-mode/after.snippet" nil nil)))


;;; Do not edit! File generated at Fri Jan 14 16:24:05 2022
