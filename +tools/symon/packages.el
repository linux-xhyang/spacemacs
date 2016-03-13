
(setq symon-packages
      '(
        symon
        ))

(defun symon/init-symon ()
  "Initialize my package"
  (use-package symon
    :defer t
    :init
    (progn
      (require 'symon)
      (symon-mode))))
