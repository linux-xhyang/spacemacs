(defconst ome-lsp-packages
  '(
    (cquery :location (recipe
                       :fetcher github
                       :repo "cquery-project/emacs-cquery"))
    (lsp-java :location (recipe
                         :fetcher github
                         :repo "linux-xhyang/lsp-java"))
    ))

(defun java-enable ()
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
  )

(defun ome-lsp/init-lsp-java ()
  (use-package lsp-java
    :init
    (progn
      (add-hook 'java-mode-hook 'java-enable)
      ))
  )

(defun cquery-enable ()
  (setq cquery-project-roots (projectile-project-root))
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
    )
  )

(defun ome-lsp/init-cquery ()
  (use-package cquery
    :init
    (progn
      (setq cquery-executable "~/src/cquery/build/release/bin/cquery")
      (when (file-executable-p cquery-executable)
        (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
        (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))
        ;;(setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t)))
        ;;(setq cquery-extra-init-params '(:cacheFormat "json" :completion (:detailedLabel t)))

        (add-hook 'c++-mode-hook 'cquery-enable)
        (add-hook 'c-mode-hook 'cquery-enable)
        ))))
