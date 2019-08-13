
(setq android-mode-packages
      '(
        (android-mode :location (recipe
                              :fetcher github
                              :repo "remvee/android-mode"))
        (android-emacs-ide :location (recipe
                                 :fetcher github
                                 :repo "linux-xhyang/android-emacs-ide"))
        elogcat
        ))

(defun android-mode/load-android-ide ()
  (when (getenv "ANDROID_BUILD_TOP")
    (let ((android-top (getenv "ANDROID_BUILD_TOP")))
      (require 'android)
      (require 'android-compile)
      (require 'android-host)
      (add-hook 'c++-mode-hook 'android-compile)
      (add-hook 'java-mode-hook 'android-compile)
      (global-set-key [f9] 'android-compile)
      )))

(defun android-mode/load-android ()
  (when (getenv "ANDROID_BUILD_TOP")
    (let ((android-top (getenv "ANDROID_BUILD_TOP")))
      (require 'android-mode)
      (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.projectile"))
        (write-region "" nil (concat (getenv "ANDROID_BUILD_TOP") "/.projectile"))
        )
      (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.python_version"))
        (write-region "2.7" nil (concat (getenv "ANDROID_BUILD_TOP") "/.python_version"))
        )
      (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.venv"))
        (let (venv (pyvenv-virtualenv-list))
          (if venv
              (write-region (car venv) nil (concat (getenv "ANDROID_BUILD_TOP") "/.venv"))
            (write-region "3.6.1" nil (concat (getenv "ANDROID_BUILD_TOP") "/.venv")))
          ))
      (setq android-mode-sdk-dir "~/Android")
      (setq android-mode-gradle-plugin "2.3.0") ;;gradle 3.3+
      )))

(defun android-mode/init-android-mode ()
  "Initialize my package"
  (use-package android-mode
    :defer t
    :init
    (progn
      (android-mode/load-android)
      )))

(defun android-mode/init-android-emacs-ide ()
  "Initialize my package"
  (use-package android-emacs-ide
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
