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
  (unless (macrop 'spacemacs|add-company-backends)
    (spacemacs|add-company-hook java-mode)
    )
  )

(defun ome-java/post-init-ggtags ()
  ;;(add-hook 'java-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
)

(defun ome-java/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'java-mode))

(defun ome-java/init-meghanada ()
  (use-package meghanada
    :config
    (progn
      (require 'meghanada)
      (diminish 'meghanada-mode "M")
      (auto-update-meghanda-android-conf)
      (add-hook 'java-mode-hook
                (lambda ()
                  ;; meghanada-mode on
                  (semantic-mode)
                  (advice-add 'meghanada--start-server-process :around #'custom-meghanada--start-server-process)
                  (meghanada-mode t)
                  ;;(advice-remove 'meghanada--start-server-process #'custom-meghanada--start-server-process)
                  (if (macrop 'spacemacs|add-company-backends)
                      (spacemacs|add-company-backends :backends '(company-meghanada ;;company-emacs-eclim
                                                                                    (company-dabbrev-code company-keywords)
                                                                                    company-files company-dabbrev) :mode java-mode)
                    (setq company-backends-java-mode '(company-meghanada ;;company-emacs-eclim
                                                                         (company-dabbrev-code company-keywords)
                                                                         company-files company-dabbrev))
                      )
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
