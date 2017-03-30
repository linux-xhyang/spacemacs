
(setq android-mode-packages
      '(
        (android-mode :location (recipe
                              :fetcher github
                              :repo "linux-xhyang/android-mode"))
        elogcat
        ))

(defun android-mode/load-android-ide ()
  (when (getenv "ANDROID_BUILD_TOP")
    (let ((android-top (getenv "ANDROID_BUILD_TOP")))
      (require 'android-mode)
      (add-to-list 'load-path (concat android-top "/development/ide/emacs/"))
      (add-to-list 'load-path (concat android-top "/prebuilts/devtools/tools/lib/"))
      (require 'android)
      (require 'android-compile)
      (require 'android-host)
      (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.projectile"))
        (write-region "" nil (concat (getenv "ANDROID_BUILD_TOP") "/.projectile")))
      (setq android-build-top (getenv "ANDROID_BUILD_TOP"))
      (setq android-mode-sdk-dir (concat (getenv "ANDROID_BUILD_TOP") "/prebuilts/devtools/"))
      (setq android-mode-sdk-tool-subdirs (mapcar #'append (split-string (concat (getenv "ANDROID_BUILD_PATHS") ":" (getenv "ANDROID_DEV_SCRIPTS")
                                                                                 ":" (getenv "PATH"))":" t)))
      )))

(defun android-mode/init-android-mode ()
  "Initialize my package"
  (use-package android-mode
    :defer t
    :init
    (progn
      (android-mode/load-android-ide)
      )))

(defun android-mode/init-elogcat ()
  "Initialize my package"
  (use-package elogcat
    :defer t
    :init
    (progn
      (require 'elogcat)
      )))
