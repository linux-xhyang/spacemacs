
(setq ome-prepare-packages
      '(
        url
        ))

(defun ome-prepare/init-url ()
  "Initialize my package"
  (use-package url
    :defer t
    :config
    (let* ((directory (file-name-directory load-file-name))
          (plantuml (concat directory "/" "plantuml.jar")))
      (unless (file-exists-p plantuml)
        (url-copy-file "https://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar" plantuml))
      (setq org-plantuml-jar-path plantuml)
      (when (not (executable-find "dot"))
        (run-su-shell "sudo apt-get install graphviz"))
      )
    ))
