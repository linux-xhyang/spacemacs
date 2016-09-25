
(setq ome-gui-packages
      '(
        all-the-icons
        yahoo-weather
        ))


(defun ome-gui/init-yahoo-weather ()
  "Initialize my package"
  (use-package yahoo-weather
    :defer t
    :init
    (progn
      (require 'yahoo-weather)
      )))


(defun ome-gui/init-all-the-icons ()
  "Initialize my package"
  (use-package all-the-icons
    :defer t
    :init
    (progn
      (require 'all-the-icons)
      (require 'powerline)
      )))
