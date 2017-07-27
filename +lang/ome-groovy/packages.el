
(defconst ome-groovy-packages
  '(
    groovy-mode
    ))

(defun ome-groovy/init-groovy-mode ()
  (use-package groovy-mode
    :config
    (progn
     (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
     (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
     (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
     )
   )
  )
