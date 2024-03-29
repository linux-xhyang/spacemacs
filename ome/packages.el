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
    ;;(company-tabnine :requires company)
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

(defun ome/post-init-company()
  (spacemacs|add-company-backends :backends company-elisp
                                  :modes emacs-lisp-mode)

  (spacemacs|add-company-backends :backends (company-capf company-ctags)
                                  :modes
                                  c-mode-common)

  (spacemacs|add-company-backends :backends company-capf
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
