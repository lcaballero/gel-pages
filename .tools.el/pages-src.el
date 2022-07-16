(defun pages-src/change-label-to-add ()
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

(defun pages-src/region-to-inserted (first second)
  (save-excursion
    (narrow-to-region beg end)
    (set-mark nil)
    (goto-char (point-min))
    (insert first)
    (goto-char (point-max))
    (insert second)
    (widen)))

(defun pages-src/region-to-p-tag (beg end)
  (interactive "r")
  (pages-src/region-to-inserted "P.Text(`" "`),"))

(defun pages-src/region-to-xlink-tag (beg end)
  (interactive "r")
  (pages-src/region-to-inserted "XLink(\"" "\"),"))

(defun pages-src/region-to-code-tag (beg end)
  (interactive "r")
  (pages-src/region-to-inserted "Code.Text(\"" "\"),"))

(defun pages-src/region-split-text (beg end)
  (interactive "r")
  (pages-src/region-to-inserted "Text(`" "`),\n"))

(defhydra local-gel (:color pink :hint nil :exit t)
  "
^Go Element (gel)^
----------------------------------------------------------------------------------------------------------------
_i_: make Text() of region
_p_: make P() tag of region
_m_: make XLink() of region
_e_: make Code() of region
_a_: run key/val to Add()

"
  ;; Go Element (gel)
  ("i" pages-src/region-split-text nil)
  ("p" pages-src/region-to-p-tag nil)
  ("m" pages-src/region-to-xlink-tag nil)
  ("e" pages-src/region-to-code-tag nil)
  ("a" pages-src/change-label-to-add nil)

  ("q" nil "quit"))

(defhydra local-tools (:color pink :hint nil :exit t)
  "
^Tools^                       ^Notes^                     ^Sub Category^
----------------------------------------------------------------------------------------------------------------
_R_: reload local .tools.el   _r_: reload the notes file  _k_: org to gel hydra
_o_: open setup.org           _g_: link to line in file   _b_: boilerplates hydra
^ ^                           ^ ^                         _t_: pages ./run or ./deploy hydra

"
  ;; Tools
  ("R" tools-el/load-all nil)
  ("o" open-setup-org nil)

  ;; Notes
  ("r" notes-reload nil)
  ("g" notes-to-temp-file nil)

  ;; gel-region
  ("k" org-gel-tools-hydra/body nil)
  ("b" my/boilerplates/body nil)
  ("t" pages-build/body nil))

(global-unset-key (kbd "C-c C-v"))
(global-set-key (kbd "C-c C-v") 'local-gel/body)
(global-set-key (kbd "C-c p") 'local-tools/body)
