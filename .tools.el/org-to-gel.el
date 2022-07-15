(defun org-gel/wrap (beg end at-start at-end)
  (narrow-to-region beg end)
  (set-mark nil)
  (goto-char (point-min))
  (insert at-start)
  (goto-char (point-max))
  (insert at-end)
  (widen))

(defun org-gel/para (beg end)
  (interactive "r")
  (save-excursion
    (org-gel/wrap beg end "P.Text(`" "`),")))

(defun org-gel/text (beg end)
  (interactive "r")
  (save-excursion
    (org-gel/wrap beg end "Text(`""`),")))

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

(defhydra org-gel-tools (:color pink :hint nil :exit t)
  "
^Org Gel^
----------------------------------------------------------------------------------------------------------------
_p_: region to gel para
_l_: org link to gel A
_t_: org text to gel Text

"
  ("p" gel-region-to-para nil)
  ("l" org-gel/parse-org-link nil)
  ("t" org-gel/text nil)

  ("q" nil "quit" :exit t))

(global-set-key (kbd "C-c a") 'org-gel-tools/body)
