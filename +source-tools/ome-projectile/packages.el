(defconst ome-projectile-packages
  '(
    projectile
    ))

(defun ome-projectile/init-projectile ()
  (use-package projectile
    :config
    (progn
      ;;(setq projectile-mode-line
      ;;      ''(:eval (format "Projectile[%s]" default-directory)))
      (setq projectile-switch-project-action 'projectile-vc)
      (setq projectile-use-git-grep t)
      (setq projectile-completion-system 'ivy
            projectile-globally-ignored-buffers '("*eshell*"
                                                  "*magit-process*"
                                                  "TAGS"
                                                  "*compilation*")
            projectile-globally-ignored-files '("TAGS")
            projectile-globally-ignored-file-suffixes '(".gif"
                                                        ".gitkeep"
                                                        ".jpeg"
                                                        ".jpg"
                                                        ".png"
                                                        ".zip")
            projectile-globally-ignored-modes '("erc-mode"
                                                "help-mode"
                                                "completion-list-mode"
                                                "Buffer-menu-mode"
                                                "gnus-.*-mode"
                                                "occur-mode"
                                                "compilation-mode"))
      (projectile-mode)
      (add-hook 'compilation-mode-hook (lambda ()
                                        (ad-deactivate 'compilation-find-file)))
      )))
