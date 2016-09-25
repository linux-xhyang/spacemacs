(defconst ome-systemtap-packages
  '(
    systemtap-mode
    ))

(defun ome-systemtap/init-systemtap-mode ()
  (use-package systemtap-mode
    :config
    (progn
      (require 'systemtap-mode)
      (setq auto-mode-alist (append '(("\\.stp$" . systemtap-mode)) auto-mode-alist))
      (setq auto-mode-alist (append '(("\\.stpm$" . systemtap-mode)) auto-mode-alist))
      )))
