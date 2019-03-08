
(setq autopair-packages
      '(
        autopair
        ))

(defun autopair/init-autopair ()
  "Initialize my package"
  (use-package autopair
    :defer t
    :init
    (progn
      (require 'autopair)
      (autopair-global-mode))))
