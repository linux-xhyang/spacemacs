(defconst ome-java-packages
  '(
    meghanada
    eclim
    ))

(defun ome-java/init-meghanada ()
  (use-package meghanada
    :config
    (progn
      (require 'meghanada)
      (add-hook 'java-mode-hook
                (lambda ()
                  ;; meghanada-mode on
                  (meghanada-mode t)
                  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
      )))

(defun ome-java/init-eclim ()
  (use-package eclim
    :config
    (require 'eclim)
    (setq eclimd-autostart t)
    (global-eclim-mode)
    (require 'eclimd)
    (ome/eclim-eclipse-directory)
    ))
