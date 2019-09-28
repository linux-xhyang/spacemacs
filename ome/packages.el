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
    lsp-mode
    company-box
    company
    (company-tabnine :requires company)
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

(defun ome/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (advice-add 'lsp :after #'tabnine//merge-company-tabnine-to-company-lsp))
  )

(defun ome/post-init-company-box ()
  (spacemacs|use-package-add-hook company-box
    :post-config
    (progn
      (push #'tabnine//company-box-icons--tabnine
            company-box-icons-functions)
      (map-put company-box-backends-colors
               'company-tabnine  '(:all
                                   tabnine-company-box-backend-tabnine-face
                                   :selected
                                   tabnine-company-box-backend-tabnine-selected-face))
      )
    )
  )

(defun ome/init-company-tabnine ()
  "docstring"
  (use-package company-tabnine
    :defer t
    :config
    (progn
      (setq company-tabnine-max-num-results 3)

      (add-to-list 'company-transformers 'tabnine//sort-by-tabnine t)
      ;; The free version of TabNine is good enough,
      ;; and below code is recommended that TabNine not always
      ;; prompt me to purchase a paid version in a large project.
      (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
        (let ((company-message-func (ad-get-arg 0)))
          (when (and company-message-func
                     (stringp (funcall company-message-func)))
            (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
              ad-do-it))))
      )
  ))
;; (defun ome/init-nlinum()
;;   (use-package nlinum
;;     :config
;;     (progn
;;       (require 'nlinum)
;;       (global-nlinum-mode t))))

;;; packages.el ends here
