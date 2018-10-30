;;; packages.el --- Language Server Protocol packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ome-lsp-packages
  '(
    (company-lsp :requires company)
    (company-go :requires company)
    ;; `flycheck-lsp' does not exist so we defined it as built-in to avoid
    ;; fetching it from ELPA repositories.
    ;; this logical package serves to hook all flycheck related configuration
    ;; for LSP.
    (flycheck-lsp :requires flycheck :location built-in)
    lsp-mode
    lsp-ui
    (cquery :location (recipe
                       :fetcher github
                       :repo "cquery-project/emacs-cquery"))
    (lsp-java :location (recipe
                         :fetcher github
                         :repo "emacs-lsp/lsp-java"))
    (lsp-go
     :requires lsp-mode
     :location (recipe :fetcher github
                       :repo "emacs-lsp/lsp-go"))
    ))


(defun ome-lsp/init-lsp-go ()
  (use-package lsp-go
    :commands lsp-go-enable))

(defun ome-lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)))

(defun ome-lsp/init-flycheck-lsp ()
  ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
  (setq lsp-enable-flycheck nil))

(defun ome-lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :config
    (progn
      (require 'lsp-imenu)
      (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
      (lsp-define-stdio-client lsp-python "python"
                               #'projectile-project-root
                               '("pyls"))
      (spacemacs|hide-lighter lsp-mode))))

(defun ome-lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :init (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    :config
    (progn
      (setq lsp-ui-sideline-ignore-duplicate t)
      (spacemacs//lsp-sync-peek-face)
      (add-hook 'spacemacs-post-theme-change-hook
                #'spacemacs//lsp-sync-peek-face))))

(defun java-enable ()
  (when (and (projectile-project-root) buffer-file-name)
    (lsp-java-enable)
    (dolist (mode '(java-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "dd" 'lsp-ui-peek-find-definitions
        "dr" 'lsp-ui-peek-find-references
        "d[" 'lsp-ui-peek-jump-backward
        "d]" 'lsp-ui-peek-jump-forward
        "ll" 'lsp-ui-imenu
	      "lr" 'lsp-rename)
      )
    ))

(defun ome-lsp/init-lsp-java ()
  (use-package lsp-java
    :init
    (progn
      (require 'company-lsp)
      (add-hook 'java-mode-hook 'java-enable)
      ))
  )

(defun cquery-enable ()
  (setq cquery-project-roots (projectile-project-root))
  (when (and cquery-project-roots (file-exists-p (concat cquery-project-roots "/compile_commands.json")))
    (lsp-cquery-enable)
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "dd" 'lsp-ui-peek-find-definitions
        "dr" 'lsp-ui-peek-find-references
        "d[" 'lsp-ui-peek-jump-backward
        "d]" 'lsp-ui-peek-jump-forward
        "qb" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
        "qc" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/callers"))
        "qd" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/derived"))
        "qv" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/vars"))
        ;; "R"  #'cquery-freshen-index
        "hm" #'cquery-member-hierarchy
        "hi" #'cquery-inheritance-hierarchy
        "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
        "hc" #'cquery-call-hierarchy
        "hC" (lambda () (interactive) (cquery-call-hierarchy t))
        "ll" 'lsp-ui-imenu
        "lr" 'lsp-rename)
      ))
  )

(defun ome-lsp/init-cquery ()
  (use-package cquery
    :init
    (progn
      (require 'company-lsp)
      (setq cquery-executable "~/src/cquery/build/cquery")
      (when (file-executable-p cquery-executable)
        (setq cquery-extra-args '("--log-file=/tmp/cq.log --log-all-to-stderr"))
        (setq cquery-extra-init-params '(:index (:comments 2 :threads 1) :discoverSystemIncludes :json-false :cacheFormat "msgpack" :completion (:detailedLabel t)))
        ;;(setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t)))
        ;;(setq cquery-extra-init-params '(:cacheFormat "json" :completion (:detailedLabel t)))

        (add-hook 'c++-mode-hook 'cquery-enable)
        (add-hook 'c-mode-hook 'cquery-enable)
        ))))

(defun java-enable ()
  (when (and (projectile-project-root) buffer-file-name)
    (lsp-java-enable)
    (dolist (mode '(java-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "dd" 'lsp-ui-peek-find-definitions
        "dr" 'lsp-ui-peek-find-references
        "d[" 'lsp-ui-peek-jump-backward
        "d]" 'lsp-ui-peek-jump-forward
        "ll" 'lsp-ui-imenu
	      "lr" 'lsp-rename)
      )
    ))

(defun ome-lsp/init-lsp-java ()
  (use-package lsp-java
    :init
    (progn
      (require 'company-lsp)
      (add-hook 'java-mode-hook 'java-enable)
      ))
  )

(defun cquery-enable ()
  (setq cquery-project-roots (projectile-project-root))
  (when (and cquery-project-roots (file-exists-p (concat cquery-project-roots "/compile_commands.json")))
    (lsp-cquery-enable)
    (dolist (mode '(c-mode c++-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "dd" 'lsp-ui-peek-find-definitions
        "dr" 'lsp-ui-peek-find-references
        "d[" 'lsp-ui-peek-jump-backward
        "d]" 'lsp-ui-peek-jump-forward
        "qb" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
        "qc" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/callers"))
        "qd" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/derived"))
        "qv" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/vars"))
        ;; "R"  #'cquery-freshen-index
        "hm" #'cquery-member-hierarchy
        "hi" #'cquery-inheritance-hierarchy
        "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
        "hc" #'cquery-call-hierarchy
        "hC" (lambda () (interactive) (cquery-call-hierarchy t))
        "ll" 'lsp-ui-imenu
        "lr" 'lsp-rename)
      ))
  )
