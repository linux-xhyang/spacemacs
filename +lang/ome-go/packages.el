(defconst ome-go-packages
      '(
        counsel-gtags
        flycheck
        (flycheck-gometalinter :toggle (and go-use-gometalinter
                                            (configuration-layer/package-used-p
                                             'flycheck)))
        (flycheck-golangci-lint :toggle (and go-use-golangci-lint
                                             (configuration-layer/package-used-p
                                              'flycheck)))
        go-eldoc
        go-fill-struct
        go-gen-test
        go-guru
        go-impl
        go-mode
        go-rename
        go-tag
        godoctor
        popwin
        ))

(defun ome-go/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'go-mode))

(defun ome-go/post-init-flycheck ()
  (spacemacs/enable-flycheck 'go-mode))

(defun ome-go/init-flycheck-gometalinter ()
  (use-package flycheck-gometalinter
    :defer t
    :init (add-hook 'go-mode-hook 'spacemacs//go-enable-gometalinter t)))

(defun ome-go/init-flycheck-golangci-lint ()
  (use-package flycheck-golangci-lint
    :defer t
    :init (add-hook 'go-mode-hook 'spacemacs//go-enable-golangci-lint t)))

(defun ome-go/post-init-ggtags ()
  (add-hook 'go-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun ome-go/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'go-mode))

(defun ome-go/init-go-eldoc ()
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun ome-go/init-go-gen-test()
  (use-package go-gen-test
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "tgg" 'go-gen-test-dwim
        "tgf" 'go-gen-test-exported
        "tgF" 'go-gen-test-all))))

(defun ome-go/init-go-guru ()
  (use-package go-impl
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'go-mode "mf" "guru")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "f<" 'go-guru-callers
        "f>" 'go-guru-callees
        "fc" 'go-guru-peers
        "fd" 'go-guru-describe
        "fe" 'go-guru-whicherrs
        "ff" 'go-guru-freevars
        "fi" 'go-guru-implements
        "fj" 'go-guru-definition
        "fo" 'go-guru-set-scope
        "fp" 'go-guru-pointsto
        "fr" 'go-guru-referrers
        "fs" 'go-guru-callstack))))

(defun ome-go/init-go-impl()
  (use-package go-impl
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "ri" 'go-impl)))

(defun ome-go/init-go-mode()
  (use-package go-mode
    :defer t
    :init
    (progn
      ;; get go packages much faster
      (setq go-packages-function 'spacemacs/go-packages-gopkgs)
      (add-hook 'go-mode-hook 'spacemacs//go-set-tab-width)
      (add-hook 'go-mode-local-vars-hook
                #'spacemacs//go-setup-backend)
      (dolist (value '(lsp go-mode))
        (add-to-list 'safe-local-variable-values
                     (cons 'go-backend value))))
    :config
    (progn
      (when go-format-before-save
        (add-hook 'before-save-hook 'gofmt-before-save))
      (spacemacs/declare-prefix-for-mode 'go-mode "me" "playground")
      (spacemacs/declare-prefix-for-mode 'go-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'go-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'go-mode "mi" "imports")
      (spacemacs/declare-prefix-for-mode 'go-mode "mr" "refactoring")
      (spacemacs/declare-prefix-for-mode 'go-mode "mt" "test")
      (spacemacs/declare-prefix-for-mode 'go-mode "mx" "execute")
      (spacemacs/set-leader-keys-for-major-mode 'go-mode
        "="  'gofmt
        "eb" 'go-play-buffer
        "ed" 'go-download-play
        "er" 'go-play-region
        "ga" 'ff-find-other-file
        "gc" 'go-coverage
        "hh" 'godoc-at-point
        "ia" 'go-import-add
        "ig" 'go-goto-imports
        "ir" 'go-remove-unused-imports
        "tP" 'spacemacs/go-run-package-tests-nested
        "tp" 'spacemacs/go-run-package-tests
        "ts" 'spacemacs/go-run-test-current-suite
        "tt" 'spacemacs/go-run-test-current-function
        "xx" 'spacemacs/go-run-main))))

(defun ome-go/init-go-rename ()
  (use-package go-rename
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rN" 'go-rename)))

(defun ome-go/init-go-tag ()
  (use-package go-tag
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rf" 'go-tag-add
            "rF" 'go-tag-remove)))

(defun ome-go/init-godoctor ()
  (use-package godoctor
    :defer t
    :init (spacemacs/set-leader-keys-for-major-mode 'go-mode
            "rd" 'godoctor-godoc
            "re" 'godoctor-extract
            "rn" 'godoctor-rename
            "rt" 'godoctor-toggle)))

(defun ome-go/post-init-popwin ()
  (push (cons go-test-buffer-name '(:dedicated t :position bottom :stick t :noselect t :height 0.4))
        popwin:special-display-config))
