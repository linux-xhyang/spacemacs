
(setq w3m-packages
      '(
        w3m
        ))

(defun w3m/init-w3m ()
  "Initialize my package"
  (use-package w3m
    :defer t
    :init
    (progn
      (require 'w3m)
      (setq w3m-use-cookies t)
      (setq w3m-cookie-accept-bad-cookies t))))
