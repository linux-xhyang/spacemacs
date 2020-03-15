
(defconst ome-misc-packages
  '(
    vlf
    dts-mode
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
