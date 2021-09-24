(defconst ome-misc-packages
  '(
    vlf
    dts-mode
    ;;(code-compass :location (recipe
    ;;                         :fetcher github
    ;;                         :repo "ag91/code-compass"
    ;;                         :files ("*.el" "scripts" "pages")))
    (xgtags :location (recipe :fetcher github :repo "linux-xhyang/xgtags"))
    ;;exec-path-from-shell
    smartparens
    ))

(defun ome-misc/post-init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config
    (progn
      (require 'exec-path-from-shell)
      ;;(exec-path-from-shell-initialize)
      )
    )
  )

(defun ome-misc/init-xgtags ()
  (use-package xgtags
    :defer t
    :config
    :init
    (progn
      (require 'xgtags)
      (diminish 'xgtags-mode " ")
      (add-hook 'prog-mode-hook
                (lambda ()
                  (xgtags-mode 1)))
      )))

(defun ome-misc/init-vlf ()
  (use-package vlf
    :defer t
    :config
    (progn
      (require 'vlf-setup)
      ;;file auto mode
      (add-to-list 'auto-mode-alist '("\\.img\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.mbn\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.elf\\'" . hexl-mode))
      (add-to-list 'auto-mode-alist '("\\.hex\\'" . hexl-mode))
      )))

(defun ome-misc/init-dts-mode ()
  (use-package vlf
    :config
    (progn
      (require 'dts-mode)
      ;;file auto mode
      )))

(defun ome-misc/init-code-compass ()
  (use-package code-compass
    :defer t
    :init
    (progn
      (require 'code-compass)))
  )

(defun ome-misc/post-init-smartparens ()
  "docstring"
  (setq sp-ignore-modes-list (quote (minibuffer-inactive-mode
                                     Info-mode
                                     term-mode
                                     org-mode
                                     org-journal-mode
                                     markdown-mode
                                     ivy-occur-mode)))
  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-M-d" . kill-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("C-M-u" . sp-backward-unwrap-sexp)
   ("C-M-c" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
   ("C-c `"  . wrap-with-back-quotes)
   )
  )
