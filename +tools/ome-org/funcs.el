(defun ome-org/switch-to-agenda ()
  (interactive)
  (org-agenda nil " "))

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "org-capture") (window-system . x)))
  (select-frame-by-name "org-capture")
  (counsel-org-capture)
  (delete-other-windows)
  )
;; emacsclient -e '(make-orgcapture-frame)'
