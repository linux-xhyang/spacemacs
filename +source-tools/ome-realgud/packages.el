
(defconst ome-realgud-packages
  '(
    realgud
    isend-mode
    ))

(defun ome-realgud/init-realgud ()
  (use-package realgud
    :config
    (progn
      (require 'realgud)
      )))

(defun ome-realgud/init-isend-mode ()
  (use-package isend-mode
    :config
    (progn
      (require 'isend)
      (add-hook 'isend-mode-hook 'isend-default-ipython-setup)
      (add-hook 'ein:notebook-multilang-mode-hook 'isend-mode)
      (add-hook 'python-mode-hook 'isend-mode)
      )))
