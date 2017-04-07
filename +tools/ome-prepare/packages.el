
(setq ome-prepare-packages
      '(
        url
        ))

(defun run-su-shell (command)
  (shell-command (concat "echo " (shell-quote-argument (read-passwd "Password? "))
                         " | sudo -S " command))
  )
(defun run-shell (command)
  (shell-command (concat command " " (shell-quote-argument (read-string "argument:"))))
  )
(defun which-utils(file)
  (let ((path (split-string (getenv "PATH") ":")))
    (cl-loop for dir in path
             when (and (string-or-null-p dir) (file-exists-p dir) (file-exists-p (concat dir  "/" file)))
             return (concat dir "/" file)
             )))

(defun ome-prepare/init-url ()
  "Initialize my package"
  (use-package url
    :defer t
    :config
    (let* ((directory (file-name-directory load-file-name))
          (plantuml (concat directory "/" "plantuml.jar")))
      (unless (file-exists-p plantuml)
        (url-copy-file "https://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar" plantuml))
      (setq-default org-plantuml-jar-path plantuml)
      (when (not (executable-find "dot"))
        (run-su-shell "sudo apt-get install graphviz"))
      )
    ))
