(defvar tab-control-auto nil)
(defun backspace-whitespace-to-tab-stop ()
      "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
      (interactive)
      (if (or indent-tabs-mode
              (region-active-p)
              (save-excursion
                (> (point) (progn (back-to-indentation)
                                  (point)))))
          (call-interactively 'backward-delete-char-untabify)
                                        ; (call-interactively 'backward-delete-char)
        (let ((movement (% (current-column) tab-width))
              (p (point)))
          (when (= movement 0) (setq movement tab-width))
          ;; Account for edge case near beginning of buffer
          (setq movement (min (- p 1) movement))
          (save-match-data
            (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
                (backward-delete-char (- (match-end 1) (match-beginning 1)))
              (call-interactively 'backward-delete-char))))))
(defun company-abort-and-insert-space ()
  (interactive)
  (company-abort)
  (insert " "))
(defun company-complete-selection-insert-key (company-key)
  (company-complete-selection)
  (insert company-key))
(defun company-complete-selection-insert-key-and-complete (company-key)
  (company-complete-selection-insert-key company-key)
  (company-complete))

;; Company start
(defun company-backspace ()
  (interactive)
  (if (equal company-selection-changed nil)
      (if tab-control-auto (backward-delete-char-untabify 1)
        (backspace-whitespace-to-tab-stop))
    (company-abort)))


(defun ome/ome-python-version ()
  (with-temp-buffer
    (apply #'process-file "python" nil t nil '("--version"))
    (goto-char (point-min))
    (let ((line  (split-string (buffer-string) "\n" t)))
      (dolist (version line)
        (if (string-match-p "^Python" version)
            (let ((ver (substring version 7)))
              (when (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" ver)
                (progn
                  (cl-return (match-string 1 ver))
                  )))
          )))))

(defun string-remove-wildchar (str)
  (if (string-match "*$" str)
      (replace-match "" t t str)
    str))

(defvar completion-at-point-functions-saved nil)

(defun ome-company/init-company ()
  (require 'kotlin-mode)
  (require 'js)
  (require 'cc-mode)
  (define-key company-mode-map [remap indent-for-tab-command]
    'company-indent-for-tab-command)

  (define-key c++-mode-map [remap c-indent-line-or-region]
    'company-indent-for-tab-command)
  (define-key c-mode-map [remap c-indent-line-or-region]
    'company-indent-for-tab-command)

  (define-key java-mode-map [remap c-indent-line-or-region]
    'company-indent-for-tab-command)

  (add-hook 'kotlin-mode-hook (lambda ()
                               (setq indent-line-function 'js-indent-line)
                               ))
  (define-key kotlin-mode-map [remap c-indent-line-or-region]
    'company-indent-for-tab-command)
  )

(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let ((completion-at-point-functions-saved completion-at-point-functions)
        (completion-at-point-functions '(company-complete-common-wrapper)))
    (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
  (let ((completion-at-point-functions completion-at-point-functions-saved))
    (company-complete-common)))

(defun company-yasnippet/disable-after-dot (fun command &optional arg &rest _ignore)
  (if (eq command 'prefix)
      (let ((prefix (funcall fun 'prefix)))
        (when (and prefix (not
                           (eq
                            (char-before (- (point) (length prefix)))
                            ?.)))
          prefix))
    (funcall fun command arg)))
