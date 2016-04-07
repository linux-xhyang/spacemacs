
(defconst ome-ccl-packages
  '(
    ac-slime
    evil-smartparens
    ))

(defun ome-ccl/init-evil-smartparens ()
  (use-package evil-smartparens
    :config
    (progn
      (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
      )))

(defun ome-ccl/init-ac-slime ()
  (use-package ac-slime
    :config
    (progn
      ;; see http://nklein.com/2010/05/getting-started-with-clojureemacsslime/
      (defmacro defslime-start (name lisp-impl)
        `(when (executable-find (symbol-name ,lisp-impl))
           (defun ,name ()
             (interactive)
             (let ((slime-default-lisp ,lisp-impl))
               (slime)))))

      (setq slime-lisp-implementations
            `((sbcl (,(executable-find "sbcl")) :coding-system utf-8-unix)
              (ccl (,(executable-find "ccl")))
              (ccl64 (,(executable-find "ccl64")))
              (clisp (,(executable-find "clisp")))))

      (if (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
          (load (expand-file-name "~/quicklisp/slime-helper.el"))
        )

      (defslime-start slime-sbcl 'sbcl)
      (defslime-start slime-ccl 'ccl)
      (defslime-start slime-ccl64 'ccl64)
      (defslime-start slime-clisp 'clisp)

      ;; If you use ubuntu/mint, then "sudo apt-get install hyperspec" will set
      ;; this for you in a file like "/etc/emacs/site-start.d/60hyperspec.el"
      ;; (setq common-lisp-hyperspec-root "/usr/share/doc/hyperspec/")

      ;; Open SBCL rc file in lisp-mode
      (add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))

      (setq slime-net-coding-system 'utf-8-unix)
      (setq slime-complete-symbol*-fancy t)
      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
      (setq inferior-lisp-program
            (or (executable-find "sbcl")
                (executable-find "ccl")
                (executable-find "ccl64")
                (executable-find "clisp")))
      (slime-setup '(slime-fancy
                     slime-indentation
                     slime-banner
                     slime-highlight-edits))

      (add-hook 'slime-mode-hook
                (lambda ()
                  (set-up-slime-ac t)))     ; use slime-fuzzy-complete-symbol
      (add-hook 'slime-repl-mode-hook
                (lambda ()
                  (set-up-slime-ac t)))
      (eval-after-load "auto-complete"
        '(add-to-list 'ac-modes 'slime-repl-mode)))
    ;;file auto mode
    ))
