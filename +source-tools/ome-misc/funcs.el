(defvar ome-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "oh-my-emacs home directory.")

(defun ome-load (module &rest header-or-tags)
  (let ((module-name (file-name-base module))
        (file (expand-file-name (if (string-match "ome-.+\.org" module)
                                    module
                                  (format "ome-%s.org" module))
                                ome-dir)))
    (if header-or-tags
        (dolist (header-or-tag header-or-tags)
          (let* ((base (file-name-nondirectory file))
                 (dir  (file-name-directory file))
                 (partial-file (expand-file-name
                                (concat "." (file-name-sans-extension base)
                                        ".part." header-or-tag ".org")
                                dir)))
            (unless (file-exists-p partial-file)
              (with-temp-file partial-file
                (insert
                 (with-temp-buffer
                   (insert-file-contents file)
                   (save-excursion
                     (condition-case nil ;; collect as a header
                         (progn
                           (org-link-search (concat "#" header-or-tag))
                           (org-narrow-to-subtree)
                           (buffer-string))
                       (error ;; collect all entries with as tags
                        (let (body)
                          (org-map-entries
                           (lambda ()
                             (save-restriction
                               (org-narrow-to-subtree)
                               (setq body (concat body "\n" (buffer-string)))))
                           header-or-tag)
                          body))))))))
            (org-babel-load-file partial-file)))
      (org-babel-load-file file))
    ))
