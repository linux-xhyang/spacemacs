
(setq ome-org-packages
      '(
        (org-roam ;; :location (recipe
                  ;;                :fetcher github
                  ;;                :repo "org-roam/org-roam")
                  )
        (org-roam-server ;; :location (recipe
                         ;;     :fetcher github
                         ;;     :repo "org-roam/org-roam-server"
                         ;;     :files ("*.el" "assets"))
                         )
        (company-org-roam ;; :location (recipe
                          ;;    :fetcher github
                          ;;    :repo "org-roam/company-org-roam")
                          )
        org-clock-convenience
        org-noter
        ))

(defun ome-org/init-org-roam ()
  "Initialize my package"
  (use-package org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link :foreground "#005200"))))
  :custom
  (org-roam-graph-viewer 'eww-open-file)
  :init
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n c" . org-roam-capture)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert)))
  :config
  (require 'org-roam-protocol)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t)))))

(defun ome-org/init-org-roam-server ()
  (use-package org-roam-server
    :ensure t)
  )

(defun ome-org//org-roam-company-setup()
  (spacemacs|add-company-backends
    :backends (company-org-roam company-tabnine company-dabbrev company-keywords)
    :modes org-mode
    :append-hooks nil
    :call-hooks nil
    ))

(defun ome-org/init-company-org-roam ()
  (use-package company-org-roam
    :after org-roam
    :config
    (progn
      (ome-org//org-roam-company-setup)
      (add-hook 'org-mode-hook #'(lambda ()
                                   (define-key org-mode-map (kbd "M-n") 'company-complete-common)
                                   ))
      )
  ))

(defun ome-org/init-org-clock-convenience ()
  (use-package org-clock-convenience
    :bind (:map org-agenda-mode-map
                ("<S-up>" . org-clock-convenience-timestamp-up)
                ("<S-down>" . org-clock-convenience-timestamp-down)
                ("o" . org-clock-convenience-fill-gap)
                ("e" . org-clock-convenience-fill-gap-both)))
  )

(defun ome-org/init-org-noter ()
  "docstring"
  (use-package org-noter
    :ensure t)
   )
