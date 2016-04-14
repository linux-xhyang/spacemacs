
(defconst ome-ecb-packages
  '(
    (ecb :location (recipe
                    :fetcher github
                    :repo "emacsmirror/ecb"))

    ))

(defun ome-ecb/init-ecb ()
  (use-package ecb
    :config
    (progn
       (require 'ecb)
       (setq ecb-source-path '("~/src/Android-L"
                               "~/src/linux"))
       (setq ecb-layout-name "left9")
       (setq ecb-tip-of-the-day nil)
       )))
