
(setq android-mode-packages
      '(
        (android-mode :location (recipe
                              :fetcher github
                              :repo "linux-xhyang/android-mode"))
        elogcat
        ))

(defun android-mode/init-android-mode ()
  "Initialize my package"
  (use-package android-mode
    :defer t
    :init
    (progn
      (setq android-mode-sdk-dir "")
      (setq android-mode-sdk-tool-subdirs (mapcar #'append (split-string (concat (getenv "ANDROID_BUILD_PATHS") ":" (getenv "ANDROID_DEV_SCRIPTS") )":" t)))
      (require 'android-mode)
      )))

(defun android-mode/init-elogcat ()
  "Initialize my package"
  (use-package elogcat
    :defer t
    :init
    (progn
      (require 'elogcat)
      )))
