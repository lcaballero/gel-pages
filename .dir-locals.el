
((nil . ((eval . (progn
                   (setq default-directory
                         (locate-dominating-file
                          buffer-file-name ".dir-locals.el"))

                   (setq compilation-read-command nil)

                   (setq compile-command
                         (format "%s%s" default-directory "run.sh build"))

                   (load (format "%s.tools.el/load-tools.el" default-directory))

                   )))))
