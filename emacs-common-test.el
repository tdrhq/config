
(ert-deftest test-ansi-term-parse-directory ()
  (with-temp-buffer
    (insert "Total 4 (delta 3), reused 0 (delta 0)
To git@github.com:tdrhq/config.git
   6aef822..e80b5c8  master -> master
arnold@ThinkStation:~/config$
arnold@ThinkStation:~/config$ ")
    (should (equal "~/config" (ansi-term-parse-directory)))))

(ert-deftest test-could-not-parse ()
  (with-temp-buffer
    (insert "not going to be found")
    (should (equal "~" (ansi-term-parse-directory)))))
