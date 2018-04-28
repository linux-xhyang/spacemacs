(defconst ome-python-packages
  '(
    elpy
    ob-ipython
;;    company-jedi
    ))

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "from IPython.core.debugger import Tracer;Tracer()()")
  (highlight-lines-matching-regexp "^[ ]*from IPython.core.debugger import Tracer;Tracer()()"))

(defun python-interactive ()
  "Enter the interactive Python environment"
  (interactive)
  (progn
    (insert "!import code; code.interact(local=vars())")
    (move-end-of-line 1)
    (comint-send-input)))

(defun ome-python/init-elpy ()
  (use-package elpy
    :config
    (progn
      (add-hook 'inferior-python-mode-hook 'python-shell-completion-native-turn-on)
      (when (executable-find "ipython")
        (setq
         python-shell-interpreter "ipython"
         python-shell-interpreter-args "-i --simple-prompt --pprint" ;;-i --simple-prompt --pprint
         ;;python-shell-prompt-regexp "In \\[[0-9]+\\]: "
         ;;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
         ))

      (add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)))

      (require 'pyvenv)
      (setenv "WORKON_HOME" "~/.pyenv/versions/")
      (when (getenv "ANDROID_BUILD_TOP")
        (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.python_version"))
          (write-region "2.7" nil (concat (getenv "ANDROID_BUILD_TOP") "/.python_version"))
          )
        (unless (file-exists-p (concat (getenv "ANDROID_BUILD_TOP") "/.venv"))
          (let ((venv (pyvenv-virtualenv-list)))
            (if venv
                (write-region (car venv) nil (concat (getenv "ANDROID_BUILD_TOP") "/.venv"))
              (write-region "3.6.1" nil (concat (getenv "ANDROID_BUILD_TOP") "/.venv")))
            ))
        )
      (elpy-enable)
      )))

(defun ome-python/init-ob-ipython ()
  (use-package ob-ipython
    )
  )

;; (defun ome-python/init-company-jedi ()
;;   (use-package company-jedi
;;     )
;;   )
