;;; packages.el --- kotlin layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Shanavas M <shanavas@disroot.org>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ome-kotlin-packages
  '(
    flycheck
    (flycheck-kotlin :requires flycheck)
    ggtags
    helm-gtags
    kotlin-mode
    ))

(spacemacs|define-jump-handlers kotlin-mode)

(defun ome-kotlin/post-init-flycheck ()
  (spacemacs/enable-flycheck 'kotlin-mode))

(defun ome-kotlin/init-flycheck-kotlin ()
  (use-package flycheck-kotlin
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-kotlin-setup)))

(defun ome-kotlin/post-init-ggtags ()
  (add-hook 'kotlin-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun ome-kotlin/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'kotlin-mode))

;; kotlin mode
(defun ome-kotlin//kotlin-setup-lsp ()
  "Setup LSP Kotlin."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  (if (configuration-layer/layer-used-p 'dap)
      (progn
        (require 'dap-java)
        (spacemacs/dap-bind-keys-for-mode 'kotlin-mode))
    (message "`dap' layer is not installed, please add `dap' layer to your dotfile.")))

(defun ome-kotlin//kotlin-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
         :backends company-lsp
         :modes kotlin-mode
         :append-hooks nil
         :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun ome-kotlin/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t
    :init
    (progn
      (add-to-list 'exec-path "~/src/kotlin-language-server/server/build/install/server/bin/")
      (add-hook 'kotlin-mode-local-vars-hook #'ome-kotlin//kotlin-setup-lsp)
      ;;(add-hook 'kotlin-mode-hook #'ome-kotlin//kotlin-setup-lsp)
      (put 'kotlin-backend 'safe-local-variable 'symbolp)
      (add-hook 'kotlin-mode-local-vars-hook #'ome-kotlin//kotlin-setup-lsp-company)
      ;;(add-hook 'kotlin-mode-hook #'ome-kotlin//kotlin-setup-lsp-company)
      )))
