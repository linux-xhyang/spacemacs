
(defconst ome-git-packages
  '(
    ;;git-gutter
    git-gutter-fringe
    git-emacs
    (git-emacs :location (recipe
                             :fetcher github
                             :repo "tsgates/git-emacs"))
    vlf
    ))

(defun ome-git/init-git-gutter ()
  (use-package git-gutter
    :config
    (progn
      (require 'git-gutter)
      ;; improve performance
      ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
      (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
      (add-to-list 'git-gutter:update-commands 'other-window)

      ;; some keybindings
      (global-set-key (kbd "C-x v g") 'git-gutter:toggle)
      (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
      ;; Jump to next/previous hunk
      (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
      (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
      ;; Stage current hunk
      (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
      ;; Revert current hunk
      (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

      (global-git-gutter-mode +1)
      ;(git-gutter:linum-setup)
      (set-face-background 'git-gutter:modified "purple") ;; background color
      (set-face-foreground 'git-gutter:added "green")
      (set-face-foreground 'git-gutter:deleted "red")
      (set-face-foreground 'git-gutter:separator "yellow")
      ))
  )

(defun ome-git/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :config
    (progn
      (require 'git-gutter-fringe)
      ;; improve performance
      ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
      (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
      (add-to-list 'git-gutter:update-commands 'other-window)

      ;; some keybindings
      (global-set-key (kbd "C-x v g") 'git-gutter:toggle)
      (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
      ;; Jump to next/previous hunk
      (global-set-key (kbd "C-x v p") 'git-gutter:previous-hunk)
      (global-set-key (kbd "C-x v n") 'git-gutter:next-hunk)
      ;; Stage current hunk
      (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
      ;; Revert current hunk
      (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

      (setq-default left-fringe-width  20)
      (setq-default right-fringe-width 5)
      (global-git-gutter-mode +1)
      (set-face-background 'git-gutter-fr:modified "purple") ;; background color
      (set-face-foreground 'git-gutter-fr:added "green")
      (set-face-foreground 'git-gutter-fr:deleted "red")
      (fringe-helper-define 'git-gutter-fr:added nil
                            "...XX......XX..."
                            "...XX......XX..."
                            "...XX......XX..."
                            "XXXXXXXX.XXXXXXX"
                            "XXXXXXXX.XXXXXXX"
                            "...XX......XX..."
                            "...XX......XX..."
                            "...XX......XX...")

      (fringe-helper-define 'git-gutter-fr:deleted nil
                            "................"
                            "................"
                            "................"
                            "XXXXXXXX.XXXXXXX"
                            "XXXXXXXX.XXXXXXX"
                            "................"
                            "................"
                            "................")

      (fringe-helper-define 'git-gutter-fr:modified nil
                            "................"
                            "................"
                            "................"
                            "................"
                            "................"
                            "................"
                            "................"
                            "................")
      ))
  )


(defun ome-git/init-git-emacs ()
  (use-package git-emacs
    :config
    (progn
      (require 'git-emacs)
      )))

(defun ome-git/init-vlf ()
  (use-package vlf
    :config
    (progn
      (require 'vlf-setup)
      )))
