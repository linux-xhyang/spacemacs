
(setq doxymacs-packages
      '(
        (doxymacs :location (recipe
                              :fetcher github
                              :repo "linux-xhyang/doxymacs"))
        ))

(defun doxymacs/init-doxymacs ()
  "Initialize my package"
  (use-package android-mode
    :defer t
    :init
    (progn
      (add-hook 'c-mode-common-hook
                (lambda ()
                  (require 'doxymacs)
                  (doxymacs-mode t)
                  (doxymacs-font-lock)))
      )))
