
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defun +rime--char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun +rime--string-match-p (regexp string &optional start)
  "与 `string-match-p' 类似，如果 REGEXP 和 STRING 是非字符串时，
不会报错。"
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string start)))

(defun +rime--probe-auto-english ()
  "激活这个探针函数后，使用下面的规则自动切换中英文输入：

1. 当前字符为英文字符（不包括空格）时，输入下一个字符为英文字符
2. 当前字符为中文字符或输入字符为行首字符时，输入的字符为中文字符
3. 以单个空格为界，自动切换中文和英文字符
   即，形如 `我使用 emacs 编辑此函数' 的句子全程自动切换中英输入法
"
  (let ((str-before-1 (+rime--char-before-to-string 0))
        (str-before-2 (+rime--char-before-to-string 1)))
    (unless (string= (buffer-name) " *temp*")
      (if (> (point) (save-excursion (back-to-indentation)
                                     (point)))
          (or (if (+rime--string-match-p " " str-before-1)
                  (+rime--string-match-p "\\cc" str-before-2)
                (not (+rime--string-match-p "\\cc" str-before-1))))))))

(defun +rime--beancount-p ()
  "当前为`beancount-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun +rime--evil-mode-p ()
  "检测当前是否在 `evil' 模式下。"
  (or (evil-normal-state-p)
      (evil-visual-state-p)
      (evil-motion-state-p)
      (evil-operator-state-p)))

(defun +rime-english-prober()
  "自定义英文输入探针函数，用于在不同mode下使用不同的探针列表"
  (let ((use-en (or (button-at (point))
                    (+rime--evil-mode-p))))
    (if (derived-mode-p 'telega-chat-mode)
        (setq use-en (or use-en
                         (+rime--probe-auto-english)))
      (when (derived-mode-p 'text-mode)
        (setq use-en (or use-en
                         (+rime--probe-auto-english))))
      (when (derived-mode-p 'prog-mode 'conf-mode)
        (setq use-en (or use-en
                         (rime--after-alphabet-char-p))))
      (setq use-en (or use-en
                       (rime--prog-in-code-p)
                       (+rime--beancount-p))))
    use-en))

(defun +rime-force-enable ()
  "强制 `rime' 使用中文输入状态.
如果当前不是 `rime' 输入法，则先激活 `rime' 输入法。如果当前是
`evil' 的非编辑状态，则转为 `evil-insert-state'。"
  (interactive)
  (let ((input-method "rime"))
    (unless (string= current-input-method input-method)
      (activate-input-method input-method))
    (when (rime-predicate-evil-mode-p)
      (if (= (+ 1 (point)) (line-end-position))
          (evil-append 1)
        (evil-insert 1)))
    (rime-force-enable)))

(defun +rime-convert-string-at-point (&optional return-cregexp)
  "将光标前的字符串转换为中文."
  (interactive "P")
  (+rime-force-enable)
  (let ((string (if mark-active
                    (buffer-substring-no-properties
                     (region-beginning) (region-end))
                  (buffer-substring-no-properties
                   (line-beginning-position) (point))))
        code
        length)
    (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
           (setq code (replace-regexp-in-string
                       "^[-']" ""
                       (match-string 0 string)))
           (setq length (length code))
           (setq code (replace-regexp-in-string " +" "" code))
           (if mark-active
               (delete-region (region-beginning) (region-end))
             (when (> length 0)
               (delete-char (- 0 length))))
           (when (> length 0)
             (setq unread-command-events
                   (append (listify-key-sequence code)
                           unread-command-events))))
          (t (message "`+rime-convert-string-at-point' did nothing.")))))
