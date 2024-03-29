(evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+)
(evil-leader/set-key "oo" 'youdao-dictionary-search-from-input)
;; activate whitespace-mode to view all whitespace characters
(evil-leader/set-key "ow" 'whitespace-mode)
(evil-leader/set-key "orf" 'org-roam-node-find)
(evil-leader/set-key "ora" 'org-roam-alias-add)
(evil-leader/set-key "org" 'org-roam-graph)
(evil-leader/set-key "orU" 'org-roam-db-build-cache)
(evil-leader/set-key "ort" 'org-roam-tag-add)
(evil-leader/set-key "ori" 'org-roam-node-insert)
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

(evil-leader/set-key "ocl" 'eacl-complete-line)
(evil-leader/set-key "ocm" 'eacl-complete-multiline)
(evil-leader/set-key "oim" 'my-convert-mail-header-to-org-link)

(evil-leader/set-key "oea" 'editorconfig-apply)
(evil-leader/set-key "oed" 'editorconfig-display-current-properties)
(evil-leader/set-key "oef" 'editorconfig-find-current-editorconfig)
