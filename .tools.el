(setq compilation-post-dir (format "%spkg/src" default-directory))

(defun compilation-post-dir (&rest _args)
  "Setting 'default-directory' after 'compilation-start'.
Can be an advice for 'compilation-start'."
  (when (and (stringp compilation-post-dir)
             (buffer-live-p compilation-last-buffer))
    (let ((post-dir compilation-post-dir))
      (with-current-buffer compilation-last-buffer
        (cd post-dir)))))

(advice-add 'compilation-start :after #'compilation-post-dir)

(defun tools-root-dir ()
  (locate-dominating-file buffer-file-name ".dir-locals.el"))

(defun tools-load ()
  (interactive)
  (load (format "%s%s" (tools-root-dir) ".tools.el")))

(defun region-to-inserted (first second)
  (save-excursion
    (narrow-to-region beg end)
    (set-mark nil)
    (goto-char (point-min))
    (insert first)
    (goto-char (point-max))
    (insert second)
    (widen)))

(defun region-to-p-tag (beg end)
  (interactive "r")
  (region-to-inserted "P.Text(`" "`),"))

(defun region-to-xlink-tag (beg end)
  (interactive "r")
  (region-to-inserted "XLink(\"" "\"),"))

(defun region-to-code-tag (beg end)
  (interactive "r")
  (region-to-inserted "Code.Text(\"" "\"),"))

(defun region-to-insert-in-text (beg end)
  (interactive "r")
  (region-to-inserted "`),\n" "Text(`"))

(defun open-setup-org ()
  (interactive)
  (find-file "~/.emacs.d/setup.org"))

(defun tools-edit-line ()
  (interactive)
  (let* ((line0 (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))
         (line1 (s-replace-regexp "," ")." line0))
         (line2 (s-replace-regexp ":" "," line1))
         (line3 (s-replace-regexp "^" "Add(" line2)))
    (delete-region
     (line-beginning-position)
     (line-end-position))
    (insert line3)))

(defun sed-on-string (s &rest args)
  (while args
    (let ((re (first args))
          (sub (second args))
          (rest (nthcdr 2 args)))
      (setq args rest)
      (setq s (s-replace-regexp re sub s))))
  s)

;; (sed-on-string "       \"title\": mime.HTML,"
;;                "," ")."
;;                ":" ","
;;                "^ *" ""
;;                "^" "Add(")

(defun tools-shell-command (file cmd)
  (format "%s%s %s" (tools-root-dir) file cmd))

;;(tools-shell-command "run.sh" "www")

(defun tools-run-command (cmd arg)
  (shell-command (tools-shell-command cmd arg)))

(defun tools-deploy-www-read-later-net ()
  (interactive)
  (tools-run-command "deploy" "www"))

(defun tools-deploy-clean-read-later-net ()
  (interactive)
  (tools-run-command "deploy" "clean"))

(defun tools-run-dist-read-later-net ()
  (interactive)
  (tools-run-command "run.sh" "dist"))

(defun tools-run-build-read-later-net ()
  (interactive)
  (tools-run-command "run.sh" "build"))

(defun tools-run-clean-read-later-net ()
  (interactive)
  (tools-run-command "run.sh" "clean"))

(defun tools-run-tests-read-later-net ()
  (interactive)
  (tools-run-command "run.sh" "tests"))

(defun tools-run-tests-read-later-net ()
  "Uses the `command` function to run compilation but
additionally using the testing-command that should be provided
via .dir-locals "
  (interactive)
  (compile (format "%s%s %s" (tools-local-file) "run.sh" "tests")))

(defun tools-run-lint-read-later-net ()
  (interactive)
  (tools-run-command "run.sh" "lint"))

(defhydra local-tools-hydra (:color pink :hint nil :exit t)
  "
^Go Element (gel)^            ^Boilerplates^         ^Tools^                       ^Notes^
----------------------------------------------------------------------------------------------------------------
_i_: make Text() of region    _B_: bash file         _R_: reload local .tools.el   _r_: reload the notes file
_p_: make P() tag of region   _D_: bash $DIR         _o_: open setup.org           _g_: link to line in file
_m_: make XLink() of region   _H_: go http handler   _a_: run key/val to Add()
_e_: make Code() of region    _T_: go test file      ^ ^
^ ^                           ^ ^                    _w_: ./deploy www
^ ^                           ^ ^                    _C_: ./deploy clean
^ ^                           ^ ^                    ^ ^
^ ^                           ^ ^                    _d_: ./run.sh dist
^ ^                           ^ ^                    _b_: ./run.sh build
^ ^                           ^ ^                    _c_: ./run.sh clean
^ ^                           ^ ^                    _t_: ./run.sh tests
^ ^                           ^ ^                    _l_: ./run.sh lint

"
  ;; Go Element (gel)
  ("i" region-to-insert-in-text nil)
  ("p" region-to-p-tag nil)
  ("m" region-to-xlink-tag nil)
  ("e" region-to-code-tag nil)
  ;; Boilerplates
  ("B" tmpl-new-bash nil)
  ("D" tmpl-script-dir nil)
  ("H" tmpl-go-http-handler-func nil)
  ("T" tmpl-go-new-test-file nil)
  ;; Tools
  ("R" tools-load nil)
  ("o" open-setup-org nil)
  ("a" tools-edit-line nil)
  ;; --------------------------------------------
  ("w" tools-deploy-www-read-later-net nil)
  ("C" tools-deploy-clean-read-later-net nil)
  ;; --------------------------------------------
  ("d" tools-run-dist-read-later-net nil)
  ("b" tools-run-build-read-later-net nil)
  ("c" tools-run-clean-read-later-net nil)
  ("t" tools-run-tests-read-later-net nil)
  ("l" tools-run-lint-read-later-net nil)

  ;; Notes
  ("r" notes-reload nil)
  ("g" notes-to-temp-file nil)

  ("q" nil "quit"))

(global-set-key (kbd "C-c C-v") 'local-tools-hydra/body)
