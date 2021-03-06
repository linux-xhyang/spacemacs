;;; funcs.el --- provide functions about tabnine
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun tabnine//merge-company-tabnine-to-company-lsp (&optional arg)
  (when (memq 'company-lsp company-backends)
    (progn
      (setq-local company-backends (remove 'company-lsp company-backends))
      (add-to-list 'company-backends '(company-lsp :with company-tabnine :separate))))
  )

(defun tabnine//company-box-icons--tabnine (candidate)
  (when (eq (get-text-property 0 'company-backend candidate)
            'company-tabnine)
    'Reference))

(defun tabnine//sort-by-tabnine (candidates)
  "The first two candidates will be from company-lsp, the following two
candidates will be from company-tabnine, others keeping their own origin order."
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))
(defun my-update-env (fn)
  (let ((str
         (with-temp-buffer
           (insert-file-contents fn)
           (buffer-string))) lst)
    (setq lst (split-string str "\000"))
    (while lst
      (setq cur (car lst))
      (when (string-match "^\\(.*?\\)=\\(.*\\)" cur)
        (setq var (match-string 1 cur))
        (setq value (match-string 2 cur))
        (setenv var value))
      (setq lst (cdr lst)))))

(defmacro def-ome-pairs (pairs)
  "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
conses, where NAME is the function name that will be created and
STRING is a single-character string that marks the opening character.

  (def-pairs ((paren . \"(\")
              (bracket . \"[\"))

defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
respectively."
  `(progn
     ,@(cl-loop for (key . val) in pairs
                collect
                `(defun ,(read (concat
                                "wrap-with-"
                                (prin1-to-string key)
                                "s"))
                     (&optional arg)
                   (interactive "p")
                   (sp-wrap-with-pair ,val)))))

(def-ome-pairs ((paren . "(")
                (bracket . "[")
                (brace . "{")
                (single-quote . "'")
                (double-quote . "\"")
                (back-quote . "`")))

(defun my-convert-mail-header-to-org-link ()
  "Assumes an email header in the killring, parses it and returns an org mode link for it."
  (interactive)
  (with-temp-buffer
    (save-match-data

      (yank) ;; yank from clipboard
      (goto-char (point-min)) ;; start from top
      (re-search-forward "^Message-Id:.+<\\(.+\\)>[ ]*$" nil nil 1)
      (setq messageid (match-string 1))

      (goto-char (point-min))
      (re-search-forward "^From:[	 ]+\\(.+?\\)[ ]*$" nil nil 1)
      (setq from (string-replace "\"" "" (match-string 1)))

      (goto-char (point-min))
      (re-search-forward "^Subject:[	 ]+\\(.+?\\)[ ]*$" nil nil 1)
      (setq subject (match-string 1))

      (goto-char (point-min))
      (re-search-forward "^Date:[	 ]+\\(.+?\\)[ ]*$" nil nil 1)
      (setq rawdate (match-string 1))
      (setq date
            (let ((time (date-to-time rawdate)))
              (set-time-zone-rule t) ;; Use Universal time.
              (prog1 (format-time-string "%Y-%m-%d %H:%M" time)
                (set-time-zone-rule nil))))

      ;;(message (concat "MID: " messageid " F:" from " S:" subject "RD:" rawdate " D:" date))
      ))
  (insert (concat "[[messageid:" messageid "][" date " " from ": " subject "]]"))
  )

;;; funcs.el ends here
