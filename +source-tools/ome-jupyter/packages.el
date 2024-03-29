;;; packages.el --- jupyter layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Benedikt Tissot <benedikt.tissot@googlemail.com>
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
;; added to `jupyter-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `jupyter/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `jupyter/pre-init-PACKAGE' and/or
;;   `jupyter/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst ome-jupyter-packages
  '(
    company
    (jupyter :location (recipe
                              :fetcher github
                              :repo "nnicandro/emacs-jupyter"))
    smartparens
    (ox-ipynb :location (recipe
                         :fetcher github
                         :repo "jkitchin/ox-ipynb"))
    ))

(defun ome-jupyter/init-jupyter ()
  (if (executable-find "jupyter")
      (use-package jupyter
        :defer t
        :init
        (progn
          (spacemacs/set-leader-keys
            "aja" 'jupyter-repl-associate-buffer
            "ajc" 'jupyter-connect-repl
            "ajr" 'jupyter-run-repl
            "ajs" 'jupyter-server-list-kernels
            )
          (spacemacs/set-leader-keys-for-major-mode 'jupyter-repl-mode
            "i" 'jupyter-inspect-at-point
            "l" 'jupyter-load-file
            "s" 'jupyter-repl-scratch-buffer
            "I" 'jupyter-repl-interrupt-kernel
            "R" 'jupyter-repl-restart-kernel)
          ;; TODO for some reason this does not work in hybrid mode
          (evil-define-key '(insert normal emacs) jupyter-repl-mode-map
            (kbd "C-j") 'jupyter-repl-history-next
            (kbd "C-k") 'jupyter-repl-history-previous
            (kbd "M-j") 'jupyter-repl-forward-cell
            (kbd "M-k") 'jupyter-repl-backward-cell
            (kbd "C-l") 'jupyter-repl-clear-cells
            (kbd "C-s") 'jupyter-repl-scratch-buffer
            (kbd "C-R") 'isearch-forward
            (kbd "C-r") 'isearch-backward)
            (setq jupyter-repl-echo-eval-p t)
            )
          )
    (message "jupyter was not found in your path, jupyter is not loaded")))

(defun org-babel-edit-worlfram-post (babel-info)
    (let ((buffername (->> babel-info caddr (alist-get :tangle))))
        (when buffername
            (setq-local buffer-file-name buffername))
        )
    )

(defun ome-jupyter/post-init-jupyter ()
    (advice-add  'org-babel-edit-prep:jupyter :after #'org-babel-edit-worlfram-post))

(defun ome-jupyter/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes jupyter-repl-mode))

(defun ome-jupyter/post-init-smartparens ()
  (add-hook 'jupyter-repl-mode-hook 'smartparens-mode))

(defun ome-jupyter/init-ox-ipynb ()
  (use-package ox-ipynb
    :defer t
    :after jupyter org
    ))
