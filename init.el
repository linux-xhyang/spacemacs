

(setq user-full-name "xhyang"
      user-mail-address "linux.xhyang@gmail.com")

(setq frame-title-format "%f@xhyang")

(ome-load "core/ome-gui.org")
(ome-load "core/ome-misc.org")

(ome-load "modules/ome-linemark.org")
(ome-load "modules/ome-ifdef-jump.org")
(ome-load "modules/ome-android.org")
(ome-load "modules/ome-common-lisp.org")
(ome-load "modules/ome-taskjuggler.org")
(ome-load "modules/ome-define.org")
(ome-load "modules/ome-ycmd.org")

(setq helm-gtags-use-input-at-cursor nil)
(evil-mode)

(if (equal 'windows-nt system-type)
    (progn
      (if (file-exists-p "D:/note/my-org.el")
          (progn
            (load "D:/note/my-org.el"))))
  (progn
    (if (file-exists-p "~/note/my-org.el")
        (progn
          (load "~/note/my-org.el")))))
