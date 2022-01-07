(setq ome-gui-packages
    '(
         (rime :toggle (display-graphic-p)
             :location (recipe :fetcher github :repo "DogLooksGood/emacs-rime"
                           :files ("Makefile" "lib.c" "rime*.el")))
         (clipetty :location (recipe :fetcher github :repo "spudlyo/clipetty"))
         cal-china-x
         ;;keyfreq
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
            (unless (display-graphic-p)
                (advice-add #'tty-run-terminal-initialization :override #'ignore)
                (add-hook 'window-setup-hook
                    (lambda ()
                        (advice-remove #'tty-run-terminal-initialization #'ignore)
                        (tty-run-terminal-initialization (selected-frame) nil t))))

            (message "enable clipetty")
            )
        )
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

(defun ome-gui/init-keyfreq ()
    (use-package keyfreq
        :defer t
        :init
        (progn
            (require 'keyfreq)
            (keyfreq-mode 1)
            (keyfreq-autosave-mode 1)
            )))
