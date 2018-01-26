
(defconst ome-ggtags-packages
  '(
    ggtags
    ))

(defun ome-ggtags/init-ggtags ()
  (use-package ggtags
    :config
    (progn
       (require 'ggtags)
       (add-hook 'c-mode-common-hook
                 (lambda ()
                   (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                     ;;(ggtags-mode 1)
                     )))
       )))
