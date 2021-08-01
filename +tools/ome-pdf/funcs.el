(defun my-org-docview-open-hack (orig-func &rest args)
  (let* ((link (car args)) path page)
    (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
    (setq path (match-string 1 link))
    (setq page (and (match-beginning 2)
                    (string-to-number (match-string 2 link))))
    (org-open-file path 1)
    (when page
      (cond
       ((eq major-mode 'pdf-view-mode)
        (pdf-view-goto-page page))
       (t
        (doc-view-goto-page page))))))
