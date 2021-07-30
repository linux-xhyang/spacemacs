(setq ome-org-packages
      '(
        org
        company-org-block
        org-clock-convenience
        org-noter
        org-roam
        (laas :location (recipe
                              :fetcher github
                              :repo "tecosaur/LaTeX-auto-activating-snippets"))
        ))

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
           :unnarrowed t)))
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
