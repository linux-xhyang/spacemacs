
(evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+)
(evil-leader/set-key "oo" 'youdao-dictionary-search-from-input)
;; activate whitespace-mode to view all whitespace characters
(evil-leader/set-key "ow" 'whitespace-mode)
(evil-leader/set-key "orf" 'org-roam-find-file)
(evil-leader/set-key "ori" 'org-roam-insert)
(evil-leader/set-key "org" 'org-roam-graph-show)
(evil-leader/set-key "orU" 'org-roam-db-build-cache)
(evil-leader/set-key "ort" 'org-roam-tag-add)
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(evil-leader/set-key "ocl" 'eacl-complete-line)
(evil-leader/set-key "ocm" 'eacl-complete-multiline)
