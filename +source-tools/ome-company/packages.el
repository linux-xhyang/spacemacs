(defconst ome-company-packages
  '(
    company
    (citre :location (recipe
                      :fetcher github
                      :repo "universal-ctags/citre"))
    yasnippet-snippets
    ))

(defun ome-company/init-citre ()
  "docstring"
  (use-package citre
    :diminish
    :bind (("C-x c j" . citre-jump+)
           ("C-x c k" . citre-jump-back)
           ("C-x c p" . citre-peek)
           ("C-x c a" . citre-ace-peek)
           ("C-x c u" . citre-update-this-tags-file))
    :hook (prog-mode . citre-auto-enable-citre-mode)
    :init
    (defun citre-jump+ ()
      "Jump to the definition of the symbol at point.
Fallback to `xref-find-definitions'."
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))
    :config
    (with-no-warnings
      (with-eval-after-load 'cc-mode (require 'citre-lang-c))
      (with-eval-after-load 'dired (require 'citre-lang-fileref))
      (with-eval-after-load 'projectile
        (setq citre-project-root-function #'projectile-project-root))

      ;; Integrate with `lsp-mode' and `eglot'
      (define-advice xref--create-fetcher (:around (fn &rest args) fallback)
        (let ((fetcher (apply fn args))
              (citre-fetcher
               (let ((xref-backend-functions '(citre-xref-backend t)))
                 (ignore xref-backend-functions)
                 (apply fn args))))
          (lambda ()
            (or (with-demoted-errors "%s, fallback to citre"
                  (funcall fetcher))
                (funcall citre-fetcher)))))

      (defun lsp-citre-capf-function ()
        "A capf backend that tries lsp first, then Citre."
        (let ((lsp-result ((lsp-completion-at-point))))
          (if (and lsp-result
                   (try-completion
                    (buffer-substring (nth 0 lsp-result)
                                      (nth 1 lsp-result))
                    (nth 2 lsp-result)))
              lsp-result
            (citre-completion-at-point))))

      (defun lsp-citre-capf-function ()
        "A capf backend that tries lsp first, then Citre."
        (let ((lsp-result (lsp-completion-at-point)))
          (if (and lsp-result
                   (try-completion
                    (buffer-substring (nth 0 lsp-result)
                                      (nth 1 lsp-result))
                    (nth 2 lsp-result)))
              lsp-result
            (citre-completion-at-point))))

      (defun enable-lsp-citre-capf-backend ()
        "Enable the lsp + Citre capf backend in current buffer."
        (add-hook 'completion-at-point-functions #'lsp-citre-capf-function nil t))

      (defun company-citre (-command &optional -arg &rest _ignored)
        "Completion backend of Citre.  Execute COMMAND with ARG and IGNORED."
        (interactive (list 'interactive))
        (cl-case -command
          (interactive (company-begin-backend 'company-citre))
          (prefix (and (bound-and-true-p citre-mode)
                       (or (citre-get-symbol) 'stop)))
          (meta (citre-get-property 'signature -arg))
          (annotation (citre-capf--get-annotation -arg))
          (candidates (all-completions -arg (citre-capf--get-collection -arg)))
          (ignore-case (not citre-completion-case-sensitive))))

      (add-hook 'citre-mode-hook #'enable-lsp-citre-capf-backend)))
  )

(defun ome-company/post-init-yasnippet-snippets ()
    (require 'yasnippet-snippets)
    (push yasnippet-snippets-dir yas-snippet-dirs)
    (yas-reload-all)
  )

(defun ome-company/post-init-company ()
  (define-key company-active-map (kbd "SPC") #'company-abort-and-insert-space)
  (define-key company-active-map (kbd "SPC") #'company-abort-and-insert-space)
  (define-key company-active-map (kbd ".") (lambda() (interactive) (company-complete-selection-insert-key-and-complete '".")))
  (define-key company-active-map (kbd ";") (lambda() (interactive) (company-complete-selection-insert-key '";")))
  (define-key company-active-map (kbd "C-e") #'company-other-backend)
  (define-key company-active-map (kbd "<backspace>") 'company-backspace)

  (spacemacs|add-company-backends :backends (company-citre company-capf) ;:with company-yasnippet :separate
                                  :modes c-mode-common emacs-lisp-mode python-mode java-mode)

  (advice-add #'company-yasnippet :around #'company-yasnippet/disable-after-dot)
  (ome-company/init-company)
  )
