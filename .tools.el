(setq compilation-post-dir (format "%ssrc" default-directory))

(defun tools-load ()
  (interactive)
  (let ((default-directory (locate-dominating-file buffer-file-name ".dir-locals.el"))
        (tools-file (format "%s.tools.el" default-directory)))
    (load tools-file)
    (message "reloaded '%s'" tools-file)))

(defun compilation-post-dir (&rest _args)
  "Setting 'default-directory' after 'compilation-start'.
Can be an advice for 'compilation-start'."
  (when (and (stringp compilation-post-dir)
             (buffer-live-p compilation-last-buffer))
    (let ((post-dir compilation-post-dir))
      (with-current-buffer compilation-last-buffer
        (cd post-dir)))))

(advice-add 'compilation-start :after #'compilation-post-dir)

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

(defhydra local-tools-hydra (:color blue :columns 3)
  "local-tools"
  ("r" tools-load "Reload Tools")
  ("i" region-to-insert-in-text "Splice Insert Text() ")
  ("p" region-to-p-tag "Region to p tag")
  ("m" region-to-xlink-tag "Region to xlink")
  ("c" region-to-code-tag "Region to code tag"))

(global-set-key (kbd "C-c C-v") 'local-tools-hydra/body)
