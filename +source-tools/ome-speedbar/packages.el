
(defconst ome-speedbar-packages
  '(
    sr-speedbar
    ))

(defun ome-speedbar/init-sr-speedbar ()
  (use-package sr-speedbar
    :config
    (progn
      (require 'sr-speedbar)
      ;;(add-hook 'after-init-hook '(lambda () (sr-speedbar-toggle)))
      (evil-leader/set-key
        "srr" 'sr-speedbar-refresh-toggle
        "srt" 'sr-speedbar-toggle
        )
      )))
