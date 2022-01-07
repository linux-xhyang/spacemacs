(setq ome-org-packages
    '(
         org
         company-org-block
         org-clock-convenience
         org-noter
         org-roam
         (org-roam-ui :location
             (recipe
                 :fetcher github
                 :repo "org-roam/org-roam-ui"
                 :files ("*.el" "out")))
         (laas :location (recipe
                             :fetcher github
                             :repo "tecosaur/LaTeX-auto-activating-snippets")
             :toggle (configuration-layer/layer-used-p 'latex))
         anki-editor
         (auctex :toggle (configuration-layer/layer-used-p 'latex))
         ))

(defun ome-org/init-anki-editor ()
    "docstring"
    (use-package anki-editor
        :defer 10
        :bind (:map org-mode-map
                  ("<f12>" . anki-editor-cloze-region-dont-incr)
                  ("<f11>" . anki-editor-cloze-region-auto-incr)
                  ("<f10>" . anki-editor-reset-cloze-number)
                  ("<f9>"  . anki-editor-push-tree)
                  ("C-c o P"     . anki-editor-push-notes)
                  ("C-c o L"     . org-cliplink)
                  )
        :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
        :config
        (setq anki-editor-create-decks t
            anki-editor-org-tags-as-anki-tags t)

        (defun anki-editor-cloze-region-auto-incr (&optional arg)
            "Cloze region without hint and increase card number."
            (interactive)
            (anki-editor-cloze-region my-anki-editor-cloze-number "")
            (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
            (forward-sexp))
        (defun anki-editor-cloze-region-dont-incr (&optional arg)
            "Cloze region without hint using the previous card number."
            (interactive)
            (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
            (forward-sexp))
        (defun anki-editor-reset-cloze-number (&optional arg)
            "Reset cloze number to ARG or 1"
            (interactive)
            (setq my-anki-editor-cloze-number (or arg 1)))
        (defun anki-editor-push-tree ()
            "Push all notes under a tree."
            (interactive)
            (anki-editor-push-notes '(4))
            (anki-editor-reset-cloze-number))
        ;; Initialize
        (anki-editor-reset-cloze-number)
        )
    )

(defun ome-org/init-company-org-block()
    (use-package company-org-block
        :ensure t
        :custom
        (company-org-block-edit-style 'inline) ;; 'auto, 'prompt, or 'inline
        :init
        (progn
            (company-mode t)
            (spacemacs|add-company-backends :backends company-org-block
                :modes org-mode))))

(defun ome-org/post-init-auctex ()
    (spacemacs|add-company-backends :backends company-auctex
        :modes org-mode)
    (spacemacs|add-company-backends :backends company-math
        :modes org-mode)
    (spacemacs|add-company-backends :backends company-reftex
        :modes org-mode)
    )

(defun ome-org/init-laas ()
    ""
    (use-package laas
        :ensure t
        :hook (org-mode . laas-mode)
        :config
        ;; 不自动插入空格
        (setq laas-enable-auto-space nil)
        (aas-set-snippets 'laas-mode
            ;; 只在 org latex 片段中展开
            :cond #'org-inside-LaTeX-fragment-p
            "tan" "\\tan"
            ;; 内积
            "i*" (lambda () (interactive)
                     (yas-expand-snippet "\\langle $1\\rangle$0"))
            "sr" "^2"
            ;; 还可以绑定函数，和 yasnippet 联动
            "Sum" (lambda () (interactive)
                      (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
            ;; 这是 laas 中定义的用于包裹式 latex 代码的函数，实现 \bm{a}
            :cond #'laas-object-on-left-condition
            ",." (lambda () (interactive) (laas-wrap-previous-object "bm"))
            ".," (lambda () (interactive) (laas-wrap-previous-object "bm"))))
    )

(defun ome-org/post-init-org-roam ()
    (require 'org-roam-protocol)
    (setq org-roam-v2-ack t)
    (setq-default org-roam-capture-templates
        '(("d" "default" plain "%?"
              :if-new
              (file+head "${slug}.org"
                  "#+date: %<%Y-%m-%d %T %:z>
#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
                  )
              :unnarrowed t)
             ("p" "private" plain "%?"
                 :if-new
                 (file+head "private-${slug}.org"
                     "#+TITLE: ${title}\n")
                 :unnarrowed t)))
    (setq-default org-roam-ref-capture-templates
        '(("r" "ref" plain "%?"
              :if-new
              (file+head "websites/${slug}.org"
                  "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
#+STARTUP: indent
- source :: ${ref}")
              :unnarrowed t)))
    )

(defun ome-org/init-org-roam-ui ()
    (use-package org-roam-ui
        :after org-roam ;; or :after org
        :hook (org-roam . org-roam-ui-mode)
        )
    )

(defun ome-org/post-init-org ()
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
