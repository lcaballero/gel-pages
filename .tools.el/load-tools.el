(defun tools-el/dir ()
  (locate-dominating-file buffer-file-name ".tools.el"))

(defun tools-el/file (name)
  (format "%s.tools.el/%s" (tools-el/dir) name))

(defun tools-el/load-all ()
  (interactive)
  (load (tools-el/file "sed-on-string.el"))
  (load (tools-el/file "go-compilation.el"))
  (load (tools-el/file "misc.el"))
  (load (tools-el/file "pages-src.el"))
  (load (tools-el/file "pages-build.el"))
  (load (tools-el/file "pages-bookmarks.el"))
  (load (tools-el/file "org-to-gel.el"))
  (load (tools-el/file "my-boilerplates.el")))

(tools-el/load-all)

(load "~/emacs-code/emacs-go-tag/go-tag.el")
