(setq ome-gui-packages
      '(
        ov
        (rime :toggle (display-graphic-p)
         :location (recipe :fetcher github :repo "DogLooksGood/emacs-rime"
                                :files ("Makefile" "lib.c" "rime*.el")))
        (clipetty :location (recipe :fetcher github :repo "spudlyo/clipetty"))
        cal-china-x
        ))

(defun ome-gui/init-rime ()
  "rime"
  (use-package rime
    :after posframe
    :bind
    (("C-\\" . 'toggle-input-method))
    :custom
    (default-input-method "rime")
    (rime-show-candidate 'posframe)
    (rime-user-data-dir (file-truename "~/.emacs.d/private/pyim/rime"))
    :config
    (progn
      (defadvice! +rime--posframe-display-result-a (args)
        "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
        :filter-args #'rime--posframe-display-result
        (cl-destructuring-bind (result) args
          (let ((newresult (if (string-blank-p result)
                               result
                             (concat result "　"))))
            (list newresult))))
      (setq rime-disable-predicates '(+rime-english-prober))
      )
    ))

(defun ome-gui/init-clipetty ()
  "docstring"
  (use-package clipetty
    :ensure t
    :config
    (progn
      (require 'clipetty)
      (global-clipetty-mode t)
      (message "enable clipetty")
      )
    )
  )

(defun org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
               (concat (buffer-substring beg end)
                       "\nmouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
                                    (define-key map (kbd "C-c C-x C-l") 'org-toggle-latex-fragment)
                                    (define-key map [mouse-1]
                                      `(lambda ()
                                         (interactive)
                                         (org-remove-latex-fragment-image-overlays ,beg ,end)))
                                    map)))

(defun org-latex-fragment-justify (justification)
  "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
  (interactive
   (list (intern-soft
          (completing-read "Justification (left): " '(left center right)
                           nil t nil nil 'left))))

  (let* ((ov (ov-at))
         (beg (ov-beg ov))
         (end (ov-end ov))
         (shift (- beg (line-beginning-position)))
         (img (overlay-get ov 'display))
         (img (and (and img (consp img) (eq (car img) 'image)
                        (image-type-available-p (plist-get (cdr img) :type)))
                   img))
         space-left offset)
    (when (and img
               ;; This means the equation is at the start of the line
               (= beg (line-beginning-position))
               (or
                (string= "" (s-trim (buffer-substring end (line-end-position))))
                (eq 'latex-environment (car (org-element-context)))))
      (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
            offset (floor (cond
                           ((eq justification 'center)
                            (- (/ space-left 2) shift))
                           ((eq justification 'right)
                            (- space-left shift))
                           (t
                            0))))
      (when (>= offset 0)
        (overlay-put ov 'before-string (make-string offset ?\ ))))))

(defun org-latex-fragment-justify-advice (beg end image imagetype)
  "After advice function to justify fragments."
  (org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'center)))

;; * Fragment overlays
(defun ome-gui/init-ov ()
  (use-package ov
    :defer t
    :init
    (progn
      (with-eval-after-load 'org
        (require 'ov)
        (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-tooltip)
        (advice-add 'org--format-latex-make-overlay :after 'org-latex-fragment-justify-advice))
      ))
  )

(defun ome-gui/init-cal-china-x ()
  "docstring"
  (use-package cal-china-x
    :defer t
    :init
    (progn
      (require 'cal-china-x)
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    cal-china-x-general-holidays
                    ))
      )
    )
  )
