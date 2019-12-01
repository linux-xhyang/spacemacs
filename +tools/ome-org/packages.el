
(setq ome-org-packages
      '(
        org-super-agenda
        ))

(defun ome-org/init-org-super-agenda ()
  "Initialize my package"
  (use-package org-super-agenda
    :defer t
    :init
    (progn
      )))
