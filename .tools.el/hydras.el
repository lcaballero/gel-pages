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

(defun gel-region-to-para (beg end)
  (interactive "r")
  (save-excursion
    (narrow-to-region beg end)
    (set-mark nil)
    (goto-char (point-min))
    (insert "P.Text(`")
    (goto-char (point-max))
    (insert "`),")
    (widen)))

(defhydra org-gel-tools-hydra (:color pink :hint nil :exit t)
  "
^Org Gel^
----------------------------------------------------------------------------------------------------------------
_p_: region to gel para

"
  ("p" gel-region-to-para nil)

  ("q" nil "quit" :exit t))

(defhydra local-boilerplates (:color pink :hint nil :exit t)
    "
^Boilerplates^
----------------------------------------------------------------------------------------------------------------
_b_: bash file
_d_: bash $DIR
_h_: go http handler
_t_: go test file

"
    ;; Boilerplates
    ("b" tmpl-new-bash nil)
    ("d" tmpl-script-dir nil)
    ("h" tmpl-go-http-handler-func nil)
    ("t" tmpl-go-new-test-file nil)
    ;; exit
    ("q" nil "quit" :exit t))

(defhydra tools-gel-pages (:color pink :hint nil :exit t)
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
  ("d" tools-run-dist-read-later-net nil)
  ("b" tools-run-build-read-later-net nil)
  ("c" tools-run-clean-read-later-net nil)
  ("t" tools-run-tests-read-later-net nil)
  ("l" tools-run-lint-read-later-net nil)

  ;; local tooling for ./deploy functions
  ("W" tools-deploy-www-read-later-net nil)
  ("C" tools-deploy-clean-read-later-net nil)

  ("q" nil "quit" :exit t))

(defhydra local-tools-hydra (:color pink :hint nil :exit t)
  "
^Go Element (gel)^            ^Tools^                       ^Notes^                     ^Sub Category^
----------------------------------------------------------------------------------------------------------------
_i_: make Text() of region    _R_: reload local .tools.el   _r_: reload the notes file  _k_: org to gel hydra
_p_: make P() tag of region   _o_: open setup.org           _g_: link to line in file   _b_: boilerplates hydra
_m_: make XLink() of region   ^ ^                           ^ ^                         _t_: pages ./run or ./deploy hydra
_e_: make Code() of region
_a_: run key/val to Add()

"
  ;; Go Element (gel)
  ("i" region-to-insert-in-text nil)
  ("p" region-to-p-tag nil)
  ("m" region-to-xlink-tag nil)
  ("e" region-to-code-tag nil)
  ("a" tools-edit-line nil)

  ;; gel-region
  ("k" org-gel-tools-hydra/body nil)
  ("b" local-boilerplates/body nil)
  ("t" tools-gel-pages/body nil)

  ;; Tools
  ("R" tools-load nil)
  ("o" open-setup-org nil)

  ;; Notes
  ("r" notes-reload nil)
  ("g" notes-to-temp-file nil)

  ("q" nil "quit"))

(global-unset-key (kbd "C-c C-v"))
(global-set-key (kbd "C-c C-v") 'local-tools-hydra/body)
(global-set-key (kbd "C-c v") 'local-tools-hydra/body)
(global-set-key (kbd "C-c e") #'(lambda () (interactive) (eval-buffer)))

(defun org-gel/parse-org-link ()
  (interactive)
  (let ((found nil)
        (d 0))
    (while (not found)
      (let (
            (props (cadr (org-element-link-parser)))
            (begin (plist-get (cadr (org-element-link-parser)) :begin))
            (end (plist-get (cadr (org-element-link-parser)) :end))
            (contents-begin (plist-get (cadr (org-element-link-parser)) :contents-begin))
            (contents-end (plist-get (cadr (org-element-link-parser)) :contents-end))
            (raw-link (plist-get (cadr (org-element-link-parser)) :raw-link))
            )
        (setq d (+ d 1))
        ;;(message "point: %S, begin: %S, end: %S, depth: %d, props: %S" (point) begin end d props)
        (if (or begin (= d 33))
            (progn
              (setq found t)
              (let (
                    (contents (buffer-substring-no-properties contents-begin contents-end))
                    )
                (delete-region begin end)
                (insert (format "A.Atts(\"href\",\"%s\").Text(\"%s\")" raw-link contents))
                ;;(message "contents: %s" contents)
                ))

          (progn
            (backward-word)
            (backward-word)
            (forward-word)
            (forward-char 1)))))))

;;org-element-link-parser 4103


(defhydra bookmarks-run-tools (:cool pink :hint nil :exit t)
  "
^Build Tools^
----------------------------------------------------------------------------------------------------------------
_b_: ./run.sh build

"
  ("b" (lambda () (interactive) (tools-run-command "bookmarks.sh" "build")) nil)
  ("q" nil "quit" :exit t))

(global-set-key (kbd "C-c a") 'org-gel/parse-org-link)
(global-set-key (kbd "C-c .") 'bookmarks-run-tools/body)

(load "~/emacs-code/emacs-go-tag/go-tag.el")
