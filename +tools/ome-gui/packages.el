
(setq ome-gui-packages
      '(
        ov
        posframe
        (liberime :location (recipe :fetcher github :repo "merrickluo/liberime"
                                    :files ("CMakeLists.txt" "Makefile" "src" "liberime*.el")))
        (rime :location (recipe :fetcher github :repo "DogLooksGood/emacs-rime"))
        (clipetty :location (recipe :fetcher github :repo "spudlyo/clipetty"))
        magit
        ))

(defun ome-gui/init-posframe ()
  (use-package posframe
    :init
    (require 'posframe)
    ))

(defun ome-gui/init-liberime ()
  "docstring"
  (use-package liberime
    :init
    (progn
      (setq liberime-user-data-dir (file-truename "~/.emacs.d/private/pyim/rime"))
      (setq pyim-page-length 9)
      (setq pyim-default-scheme 'rime-quanpin)
      (setq default-input-method "pyim")
      (require 'liberime)

      (add-hook 'after-liberime-load-hook
                (lambda ()
                  (run-with-timer
                   5 1
                   (liberime-start)
                   (liberime-select-schema "luna_pinyin_fluency"))
                  ;;(liberime-get-schema-list)
                  ))
      (unless (file-exists-p (concat (liberime-get-library-directory)
                                     "build/liberime-core"
                                     module-file-suffix))
        (liberime-build))
      )
    ))

(defun ome-gui/init-rime ()
  "rime"
  (use-package rime
    :bind
    (("C-\\" . 'rime-toggle))
    :init
    (setq rime-show-candidate 'posframe)))

(defun ome-gui/init-clipetty ()
  "docstring"
  (use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode))
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

(defun ome-gui/init-magit ()
  (use-package magit
    :defer t
    :init
    (progn
      (with-eval-after-load 'magit-mode
        (message "hahahahaha")
        (require 'magit-mode)
        (require 'ivy)
        (pretty-magit-setup)
        ))))
