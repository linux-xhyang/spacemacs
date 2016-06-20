
(setq android-mode-packages
      '(
        (android-mode :location (recipe
                              :fetcher github
                              :repo "linux-xhyang/android-mode"))
        ))

(defun android-mode/init-android-mode ()
  "Initialize my package"
  (use-package android-mode
    :defer t
    :init
    (progn
      (setq android-mode-sdk-dir "~/android-sdk/")
      (require 'android-mode)
      )))
