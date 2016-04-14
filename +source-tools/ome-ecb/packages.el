
(defconst ome-ecb-packages
  '(
    (ecb :location (recipe
                    :fetcher github
                    :repo "emacsmirror/ecb"))

    ))

(defun ome-ecb/init-ecb ()
  (use-package ecb
    :config
    (
     (require 'ecb)
     (require 'ecb-autoloads)
     )))
