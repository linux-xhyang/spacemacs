;;; packages.el --- ome layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: xhyang <xhyang@xhyang-ThinkPad-Edge>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `ome-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `ome/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `ome/pre-init-PACKAGE' and/or
;;   `ome/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ome-packages
  '(
    (xgtags :location (recipe :fetcher github :repo "linux-xhyang/xgtags"))
    exec-path-from-shell
    ;;(company-tabnine :requires company)
    (company-lsp :requires company)
    ob-ipython
    eacl
    ))

(defvar ome-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "oh-my-emacs home directory.")

(defun ome-load (module &rest header-or-tags)
  (let ((module-name (file-name-base module))
        (file (expand-file-name (if (string-match "ome-.+\.org" module)
                                    module
                                  (format "ome-%s.org" module))
                                ome-dir)))
    (if header-or-tags
        (dolist (header-or-tag header-or-tags)
          (let* ((base (file-name-nondirectory file))
                 (dir  (file-name-directory file))
                 (partial-file (expand-file-name
                                (concat "." (file-name-sans-extension base)
                                        ".part." header-or-tag ".org")
                                dir)))
            (unless (file-exists-p partial-file)
              (with-temp-file partial-file
                (insert
                 (with-temp-buffer
                   (insert-file-contents file)
                   (save-excursion
                     (condition-case nil ;; collect as a header
                         (progn
                           (org-link-search (concat "#" header-or-tag))
                           (org-narrow-to-subtree)
                           (buffer-string))
                       (error ;; collect all entries with as tags
                        (let (body)
                          (org-map-entries
                           (lambda ()
                             (save-restriction
                               (org-narrow-to-subtree)
                               (setq body (concat body "\n" (buffer-string)))))
                           header-or-tag)
                          body))))))))
            (org-babel-load-file partial-file)))
      (org-babel-load-file file))
    ))

(defun ome/init-xgtags ()
  (use-package xgtags
    :defer t
    :config
    :init
    (progn
      (require 'xgtags)
      (diminish 'xgtags-mode " ")
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (xgtags-mode 1)))

      (add-hook 'sh-mode-hook
                (lambda ()
                  (xgtags-mode 1)))

      (add-hook 'asm-mode-hook
                (lambda ()
                  (xgtags-mode 1)))

      (add-hook 'makefile-mode-hook
                (lambda ()
                  (xgtags-mode 1)))

      (add-hook 'lisp-mode-hook
                (lambda ()
                  (xgtags-mode 1)))

      )))

(defun ome/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config
    :init
    (progn
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize)
      )
    )
  )

(defun ome/init-ob-ipython ()
  "docstring"
  (use-package ob-ipython
    :defer t
    :config
    (require 'ob-ipython)
    ))


(defun ome/init-company-lsp ()
  (use-package company-lsp
    :defer t
    ))

(defun ome/post-init-company()
  (spacemacs|add-company-backends :backends company-elisp
                                  :modes emacs-lisp-mode)

  (spacemacs|add-company-backends :backends (company-lsp company-ctags)
                                  :modes
                                  c-mode-common)

  (spacemacs|add-company-backends :backends company-lsp
                                  :modes
                                  python-mode)
  )

(defun ome/init-eacl ()
  "docstring"
  (use-package eacl
    :defer t
    :config
    (require 'eacl)
    )
  )

(with-eval-after-load 'smartparens
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
