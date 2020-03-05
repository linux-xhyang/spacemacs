
(defvar pretty-magit--use-commit-prompt? nil
  "Do we need to use the magit commit prompt?")

(defvar pretty-magit--prompt t
  "A list of commit leader prompt candidates.")

(defvar pretty-magit--prompt-type '("feat" "fix" "docs")
  "A list of commit leader prompt type candidates.")

(defvar pretty-magit--prompt-scope '("systemui" "settings" "framework" "kernel" "autofocus" "keystone" "fmsdk")
  "A list of commit leader prompt scope candidates.")

(defvar pretty-magit--alist nil
  "An alist of regexes, an icon, and face properties to apply to icon.")

(defun pretty-magit-add-leader (word char face-props)
  "Replace sanitized WORD with CHAR having FACE-PROPS and add to prompts."
  (add-to-list 'pretty-magit--alist
               (list (rx-to-string `(: bow
                                       (group ,word ":")))
                     char face-props))
  (add-to-list 'pretty-magit--prompt
               (concat word ": ")))

(defun pretty-magit-add-leaders (leaders)
  "Map `pretty-magit-add-leader' over LEADERS."
  (-each leaders
    (-applify #'pretty-magit-add-leader)))

(defun pretty-magit--add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (-each pretty-magit--alist
      (-lambda ((rgx char face-props))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward rgx nil t)
            (-let [(start end) (match-data 1)]
              (compose-region start end char)
              (when face-props
                (add-face-text-property start end face-props)))))))))

(defun pretty-magit--use-commit-prompt (&rest args)
  (setq pretty-magit--use-commit-prompt? t))

(defun pretty-magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when (and pretty-magit--use-commit-prompt?
             pretty-magit--prompt)
    (setq pretty-magit--use-commit-prompt? nil)
    (insert (ivy-read "Commit Type " pretty-magit--prompt-type
                      :require-match t
                      :sort t
                      :preselect 0)
            (concat "(" (ivy-read "Commit Scope " pretty-magit--prompt-scope
                      :require-match nil
                      :sort t
                      :preselect 0) ")")

            ": \n\n"
            (ivy-read "Commit Bug " '("#")
                      :require-match nil
                      :sort t
                      )
            )
    (pretty-magit--add-magit-faces)
    (evil-insert 3)))

(defun pretty-magit-setup ()
  "docstring"
  (interactive "P")
  (advice-add 'magit-refresh-buffer :after 'pretty-magit--add-magit-faces)
  (remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
  (add-hook    'git-commit-setup-hook 'pretty-magit-commit-prompt)
  (advice-add 'magit-commit-create :after 'pretty-magit--use-commit-prompt)
  )
