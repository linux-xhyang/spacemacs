
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

(defun android-mode/android-p-build ()
  (when (getenv "ANDROID_BUILD_TOP")
    (let ((android-top (getenv "ANDROID_BUILD_TOP")))
      (if (file-exists-p (concat android-top "/" "build/soong/soong_ui.bash"))
          t
        nil)
      )
    )
  )
(defun rebind-compilation-find-file ()
  "docstring"
  (eval-after-load "projectile"
    '(defun compilation-find-file-projectile-find-compilation-buffer (orig-fun marker filename directory &rest formats)
       "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function."
       (let* ((topdir (concat (getenv "ANDROID_BUILD_TOP") "/"))
              (path (concat topdir  filename)))
         (when (file-exists-p path)
           (setq filename path)
           ))
       (when (and (not (file-exists-p (expand-file-name filename)))
                  (projectile-project-p))
         (let* ((root (projectile-project-root))
                (dirs (cons "" (projectile-current-project-dirs)))
                (new-filename (car (cl-remove-if-not
                                    #'file-exists-p
                                    (mapcar
                                     (lambda (f)
                                       (expand-file-name
                                        filename
                                        (expand-file-name f root)))
                                     dirs)))))
           (when new-filename
             (setq filename new-filename))))

       (apply orig-fun `(,marker ,filename ,directory ,@formats)))
    )
  )

(defun android-mode/load-android-ide ()
  (when (getenv "ANDROID_BUILD_TOP")
    (let ((android-top (getenv "ANDROID_BUILD_TOP")))
      (require 'android)
      (require 'android-compile)
      (require 'android-host)
      (rebind-compilation-find-file)
      (defun androidmk-compile ()
        "docstring"
        (interactive)
        (let* ((topdir (concat (getenv "ANDROID_BUILD_TOP") "/"))
               (makefile (android-find-makefile topdir))
               (options
                (concat " -j " (number-to-string android-compilation-jobs))))
          ;;(unless (file-exists-p (concat topdir "buildspec.mk"))
          ;;  (error "buildspec.mk missing in %s." topdir))
          ;; Add-hook do not re-add if already present. The compile
          ;; filter hooks run after the comint cleanup (^M).
          (add-hook 'compilation-filter-hook 'android-compile-filter)
          (set (make-local-variable 'compile-command)
               (if (cadr makefile)
                   ;; The root Makefile is not invoked using ONE_SHOT_MAKEFILE.
                   (concat "make -C " topdir options) ; Build the whole image.
                 (if (android-mode/android-p-build)
                     (let* ((androidmk (directory-file-name (file-name-directory (car makefile))))
                            (modules (concat "MODULES-IN-" (replace-regexp-in-string "/" "-" androidmk))))
                       (concat "ONE_SHOT_MAKEFILE=" (car makefile) " "
                               (concat topdir "/" "build/soong/soong_ui.bash --make-mode ") modules)
                       )
                   (concat "ONE_SHOT_MAKEFILE=" (car makefile)
                           " make -C " topdir options " all_modules ")
                   )
                 ))
          (if (interactive-p)
              (call-interactively 'compile)))
        )
      (add-hook 'c++-mode-hook 'android-compile)
      (add-hook 'java-mode-hook 'android-compile)
      (global-set-key [f9] 'androidmk-compile)
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
