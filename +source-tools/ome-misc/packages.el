
(defconst ome-misc-packages
  '(
    vlf
    dts-mode
    counsel-etags
    ))

(defun ome-misc/init-vlf ()
  (use-package vlf
    :config
    (progn
      (require 'vlf-setup)
      ;;file auto mode
      (add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.mbn\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.elf\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.hex\\'" . hexl-mode))
      )))

(defun ome-misc/init-dts-mode ()
  (use-package vlf
    :config
    (progn
      (require 'dts-mode)
      ;;file auto mode
      )))


(defun ome-misc/init-counsel-etags ()
  (use-package counsel-etags
    :defer t
    :init
    ;; Setup auto update now
    (defun update-etags-hook ()
      (add-hook 'after-save-hook
                'counsel-etags-virtual-update-tags 'append 'local))
    (add-hook 'prog-mode-hook #'update-etags-hook)
    :config
    ;; Don't ask before rereading the TAGS files if they have changed
    (setq tags-revert-without-query t)
    ;; Don't warn when TAGS files are large
    (setq large-file-warning-threshold nil)
    (global-set-key (kbd "C-c g d") 'counsel-etags-find-tag-at-point)
    ))
