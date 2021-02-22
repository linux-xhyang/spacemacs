
(setq ome-org-packages
      '(
        (org-roam-server  :location (recipe
                              :fetcher github
                              :repo "org-roam/org-roam-server"
                              :files ("*.el" "assets"))
                          )
        org-clock-convenience
        org-noter
        ))

(defun ome-org//org-roam-company-setup()
  (spacemacs|add-company-backends
    :backends (company-org-block company-dabbrev company-keywords)
    :modes org-mode
    :append-hooks nil
    :call-hooks nil
    ))

(with-eval-after-load 'org-roam
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
           :unnarrowed t))))

(with-eval-after-load 'org
  (ome-org//org-roam-company-setup)
  (if (equal 'windows-nt system-type)
      (progn
        (if (file-exists-p "D:/note/my-org.el")
            (progn
              (load "D:/note/my-org.el"))))
    (progn
      (if (file-exists-p "~/note/my-org.el")
          (progn
            (load "~/note/my-org.el")))))
  )

(defun ome-org/init-org-roam-server ()
  (use-package org-roam-server
    :ensure t)
  )

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
