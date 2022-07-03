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

(defun tools-load ()
  (interactive)
  (let ((default-directory (locate-dominating-file buffer-file-name ".dir-locals.el"))
        (tools-file (format "%s.tools.el" default-directory)))
    (load tools-file)
    (message "reloaded '%s'" tools-file)))

(defun region-to-inserted (first second)
  (narrow-to-region beg end)
  (set-mark nil)
  (goto-char (point-min))
  (insert first)
  (goto-char (point-max))
  (insert second)
  (widen))

(defun region-to-p-tag (beg end)
  (interactive "r")
  (save-excursion
    (region-to-inserted "P.Text(`" "`),")))

(defun region-to-xlink-tag (beg end)
  (interactive "r")
  (save-excursion
    (region-to-inserted "XLink(\"" "\"),")))

(defun region-to-code-tag (beg end)
  (interactive "r")
  (save-excursion
    (region-to-inserted "Code.Text(\"" "\"),")))

(defun region-to-insert-in-text (beg end)
  (interactive "r")
  (save-excursion
    (region-to-inserted "`),\n" "Text(`")))

(defun open-setup-org ()
  (interactive)
  (find-file "~/.emacs.d/setup.org"))

(defhydra local-tools-hydra (:color pink :hint nil)
  "

^Go Element (gel)^            ^Boilerplates^         ^Tools^
----------------------------------------------------------
_i_: make Text() of region    _b_: bash file         _r_: reload local .tools.el   
_p_: make P() tag of region   _d_: bash $DIR         _o_: open setup.org
_m_: make XLink() of region   _h_: go http handler
_c_: make Code() of region    _t_: go test file

"

  ;; Go Element (gel)
  ("i" region-to-insert-in-text nil)
  ("p" region-to-p-tag nil)
  ("m" region-to-xlink-tag nil)
  ("c" region-to-code-tag nil)

  ;; Boilerplates
  ("b" tmpl-new-bash nil)
  ("h" tmpl-go-http-handler-func nil)
  ("t" tmpl-go-new-test-file nil)
  ("d" tmpl-script-dir nil)

  ;; Tools
  ("r" tools-load nil)
  ("o" open-setup-org nil)
  
  ("q" nil "quit")
  )

(global-set-key (kbd "C-c C-v") 'local-tools-hydra/body)
