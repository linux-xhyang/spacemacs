(defconst ome-java-packages
  '(
    meghanada
    company
    ggtags
    helm-gtags
    (java-mode :location built-in)
    ;;eclim
    ))

(defun ome-java/post-init-company ()
  (spacemacs|add-company-hook java-mode))

(defun ome-java/post-init-ggtags ()
  (add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun ome-java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun ome-java/init-meghanada ()
  (use-package meghanada
    :config
    (progn
      (require 'meghanada)
      (diminish 'meghanada-mode "M")
      (add-hook 'java-mode-hook
                (lambda ()
                  ;; meghanada-mode on
                  (advice-add 'meghanada--start-server-process :around #'custom-meghanada--start-server-process)
                  (meghanada-mode t)
                  ;;(advice-remove 'meghanada--start-server-process #'custom-meghanada--start-server-process)
                  (setq company-backends-java-mode '(company-meghanada company-emacs-eclim
                                                     (company-dabbrev-code company-keywords)
                                                     company-files company-dabbrev))
                  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
      )))

;; (defun ome-java/init-eclim ()
;;   (use-package eclim
;;     :config
;;     (when (executable-find "eclipse")
;;       (require 'eclim)
;;       (setq eclimd-autostart t)
;;       (global-eclim-mode)
;;       (diminish 'eclim-mode "E")
;;       (require 'eclimd)
;;       (ome/eclim-eclipse-directory)
;;       (setq company-backends-java-mode '(company-emacs-eclim
;;                                          (company-dabbrev-code company-keywords)
;;                                          company-files company-dabbrev))
;;       )
;;     ))
