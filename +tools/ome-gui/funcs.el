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

(defun get-word-boundary ()
  "Return the boundary of the current word.
     The return value is of the form: (cons pos1 pos2).
     "
  (save-excursion
    (let (p1 p2)
      (progn
        (skip-chars-backward "-A-Za-z0-9_.") ;; here you can choose which symbols to use
        (setq p1 (point))
        (skip-chars-forward "-A-Za-z0-9_.") ;; put the same here
        (setq p2 (point)))
      (cons p1 p2)
      ))
  )
(defun select-word ()
  "Mark the url under cursor."
  (interactive)
                                        ;  (require 'thingatpt)
  (let (bds)
    (setq bds (get-word-boundary))

    (set-mark (car bds))
    (goto-char (cdr bds))
    )
  )

(defun insert-datetime ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %l:%M:%S")))

(defun walk-directory (dirname fn &optional directories-p testfn)
  (let ((directories (and directories-p t))
        (test (or (if (functionp testfn) testfn nil) #'(lambda (name) t))))
    (cl-labels
        ((walk (name)
               (let ((fnd (file-name-nondirectory name)))
                 (cond
                  ((file-directory-p name)
                   (if (not (string-match "^\\.+$" fnd))
                       (progn
                         (when (and directories (funcall test name))
                           (funcall fn name))
                         (dolist (x (directory-files name t)) (walk x)))))
                  ((funcall test name) (funcall fn name))))))
      (walk dirname))))

(defun list-to-string (list)
  "thisandthat."
  (interactive)
  (when (consp list)
    (let ((var1 (car list)))
      (if (stringp (car list))
          (cond ((consp (cdr list)) (concat (car list) " " (list-to-string (cdr list))))
                (t (car list)))
        (list-to-string (cdr list)))
      )))

(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (save-excursion (let ((i -1))
                    (insert "ASCII characters 0 thru 127.\n\n")
                    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
                    (while (< i 31)
                      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                                      (setq i (+ 1  i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)
                                      (setq i (+ 32 i)) i (single-key-description i)))
                      (setq i (- i 96))))))

(defun replace-tab-with-space ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max)))
  )
