

(defun ome/eclim-eclipse-valid ()
  (cl-loop for dir in eclim-eclipse-dirs
           when (and (message dir) (string-or-null-p dir) (file-exists-p dir) (file-exists-p (concat dir "/eclipse")))
               return dir
               ))

(defun ome/eclim-eclipse-directory ()
  (setq eclim-eclipse-dirs (append eclim-eclipse-dirs (list (getenv "HOME"))))
  (unless (ome/eclim-eclipse-valid)
    (let ((eclipse-dir (expand-file-name (read-directory-name "Eclipse Directory: "))))
      (custom-set-variables
       '(eclim-eclipse-dirs (list eclipse-dir))
       '(eclim-executable (concat eclipse-dir "/eclim"))
       ))))
