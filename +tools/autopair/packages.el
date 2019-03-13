
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
      (add-hook 'c-mode-common-hook
                #'(lambda ()
                    (push ?'
                          (cl-getf autopair-dont-pair :never))
                    (push ?\"
                          (cl-getf autopair-dont-pair :never))
                    (push ?[
                          (cl-getf autopair-dont-pair :never))
                    (autopair-mode)))

      (add-hook 'org-mode-hook
                #'(lambda ()
                    (push ?<
                          (cl-getf autopair-dont-pair :never))
                    (autopair-mode)))
      )))
