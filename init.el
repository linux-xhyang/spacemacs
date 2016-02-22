

(setq user-full-name "xhyang"
      user-mail-address "linux.xhyang@gmail.com")

(setq frame-title-format "%f@xhyang")

(ome-load "core/ome-gui.org")
(ome-load "core/ome-misc.org")

(ome-load "modules/ome-linemark.org")
(ome-load "modules/ome-ifdef-jump.org")
(ome-load "modules/ome-android.org")

(setq helm-gtags-use-input-at-cursor nil)
(evil-mode)
