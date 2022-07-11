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
