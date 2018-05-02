(defconst ome-lsp-packages
  '(
    (lsp-mode :location (recipe
                         :fetcher github
                         :repo "emacs-lsp/lsp-mode"))
    (cquery :location (recipe
                       :fetcher github
                       :repo "cquery-project/emacs-cquery"))

    (lsp-ui :location (recipe
                       :fetcher github
                       :repo "emacs-lsp/lsp-ui"))
    (company-lsp :location (recipe
                            :fetcher github
                            :repo "tigersoldier/company-lsp"))
    ))


(defun ome-lsp/init-lsp-ui ()
  (use-package lsp-ui
    :init
    (progn
      (setq cquery-executable "~/src/cquery/build/release/bin/cquery")
      (when (file-executable-p cquery-executable)
        ;; ;; Log file
        (add-hook 'lsp-mode-hook 'lsp-ui-mode)
        )
      )))

(defun ome-lsp/init-lsp-mode ()
  (use-package lsp-mode
    :init
    (progn
      (setq cquery-executable "~/src/cquery/build/release/bin/cquery")
      (when (file-executable-p cquery-executable)
        ;; ;; Log file
        (setq cquery-extra-args '("--log-file=/tmp/cq.log"))
        )
      )))


(defun cquery-enable ()
  (condition-case nil
      (when (projectile-project-root)
        (setq cquery-project-roots (projectile-project-root))
        (lsp-cquery-enable)
        (dolist (mode '(c-mode c++-mode))
          (spacemacs/set-leader-keys-for-major-mode mode
            "dd" 'lsp-ui-peek-find-definitions
            "dr" 'lsp-ui-peek-find-references
            "d[" 'lsp-ui-peek-jump-backward
            "d]" 'lsp-ui-peek-jump-forward
            ;; "qb" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
            ;; "qc" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/callers"))
            ;; "qd" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/derived"))
            ;; "qv" (lambda () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/vars"))
            ;; "R"  #'cquery-freshen-index
            ;; "hm" #'cquery-member-hierarchy
            ;; "hi" #'cquery-inheritance-hierarchy
            ;; "hI" (lambda () (interactive) (cquery-inheritance-hierarchy t))
            ;; "hc" #'cquery-call-hierarchy
            ;; "hC" (lambda () (interactive) (cquery-call-hierarchy t))
            "ll" 'lsp-ui-imenu
	          "lr" 'lsp-rename))
        )
    (user-error nil)))

(defun ome-lsp/init-cquery ()
  (use-package cquery
    :init
    (progn
      (setq cquery-executable "~/src/cquery/build/release/bin/cquery")
      (when (file-executable-p cquery-executable)
        (setq cquery-extra-init-params '(:index (:comments 2)
                                                :cacheFormat "msgpack" :completion (:detailedLabel t)))
        (add-hook 'c-mode-common-hook 'cquery-enable)
        ))))

(defun ome-lsp/init-company-lsp ()
  (use-package company-lsp
    :init
    (progn
      (setq cquery-executable "~/src/cquery/build/release/bin/cquery")
      (when (file-executable-p cquery-executable)
        (with-eval-after-load 'company
          (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common))
        ))))
