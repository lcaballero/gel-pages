(defun pages-build/root ()
  (locate-dominating-file buffer-file-name ".dir-locals.el"))

(defun pages-build/file (name args)
  (format "%s%s %s" (pages-build/root) name args))

(pages-build/file "run.sh" "dist")

(defun pages-build/sh (script func)
  (shell-command (pages-build/file script func)))

(defun pages-build/deploy ()
  (interactive)
  (pages-build/sh "deploy" "www"))

(defun pages-build/deploy-clean ()
  (interactive)
  (pages-build/sh "deploy" "clean"))

(defun pages-build/run-dist ()
  (interactive)
  (pages-build/sh "run.sh" "dist"))

(defun pages-build/run-build ()
  (interactive)
  (pages-build/sh "run.sh" "build"))

(defun pages-build/run-clean ()
  (interactive)
  (pages-build/sh "run.sh" "clean"))

(defun pages-build/run-tests ()
  (interactive)
  (pages-build/sh "run.sh" "tests"))

(defun pages-build/run-tests ()
  "Uses the `command` function to run compilation but
additionally using the testing-command that should be provided
via .dir-locals "
  (interactive)
  (compile (format "%s%s %s" (tools-local-file) "run.sh" "tests")))

(defun pages-build/run-lint ()
  (interactive)
  (pages-build/run "run.sh" "lint"))

(defhydra pages-build (:color pink :hint nil :exit t)
  "
^Dev Local^                 ^Deploy^
----------------------------------------------------------------------------------------------------------------
_d_: ./run.sh dist          _W_: ./deploy www
_b_: ./run.sh build         _C_: ./deploy clean
_c_: ./run.sh clean
_t_: ./run.sh tests
_l_: ./run.sh lint

"
  ;; local tooling for ./run.sh functions
  ("d" pages-build/run-dist nil)
  ("b" pages-build/run-build nil)
  ("c" pages-build/run-clean nil)
  ("t" pages-build/run-tests nil)
  ("l" pages-build/run-lint nil)

  ;; local tooling for ./deploy functions
  ("W" pages-build/deploy nil)
  ("C" pages-build/deploy-clean nil)

  ("q" nil "quit" :exit t))
