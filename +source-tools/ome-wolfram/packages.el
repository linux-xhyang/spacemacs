(setq ome-wolfram-packages
    '(
         lsp-mode
         wolfram-mode
         company
         ein
         ))


;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun ome-wolfram/init-wolfram-mode ()
    (use-package wolfram-mode
        :defer t
        :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
        ;;:mode "\\.wl\\'"
        :init
        (progn
            (define-derived-mode wolfram-language-mode wolfram-mode "WolframLanguage")
            (add-to-list 'auto-mode-alist '("\\.m\\'" . wolfram-language-mode))
            (add-to-list 'auto-mode-alist '("\\.wl\\'" . wolfram-language-mode))
            (add-hook 'wolfram-language-mode-hook #'ome-wolfram/wolfram-setup-lsp)
            (add-hook 'wolfram-mode-hook #'ome-wolfram/wolfram-setup-lsp))))

(defun ome-wolfram/post-init-lsp-mode ()

    (with-eval-after-load 'lsp-mode
        (add-to-list 'lsp-language-id-configuration '(wolfram-mode . "Mathematica"))
        (add-to-list 'lsp-language-id-configuration '(wolfram-language-mode . "Mathematica"))

        ;;https://github.com/kenkangxgwe/lsp-wl
        (lsp-register-client
            (make-lsp-client :language-id 'wolfram
                :new-connection (lsp-tcp-server-command
                                    (lambda (port)
                                        `("wolfram" ;; or "wolframscript"
                                             "-script" ;; or "-file"
                                             "~/src/lsp-wl/init.wls"
                                             ,(concat
                                                  "--socket="
                                                  (number-to-string port)
                                                  ))))
                :major-modes '(wolfram-mode wolfram-language-mode)
                :server-id 'lsp-wl
                ))
        ;; https://github.com/WolframResearch/LSPServer
        ;; (lsp-register-client
        ;;     (make-lsp-client :language-id 'wolfram
        ;;         :new-connection (lsp-stdio-connection
        ;;                             (lambda () '("WolframKernel" "-noprompt" "-run" "Get\[\"~/src/LSPServer/server.wl\"\]")))
        ;;         :major-modes '(wolfram-mode)
        ;;         :server-id 'wolfram-lsp
        ;;         ))
        )
    )

(defun ome-wolfram/post-init-company ()
    (spacemacs|add-company-backends :backends company-capf :modes wolfram-mode))

(defun ome-wolfram/post-init-ein ()
    (setq-default ob-ein-languages
        (quote
            (("ein" . python)
                ("ein-python" . python)
                ("ein-R" . R)
                ("ein-r" . R)
                ("ein-julia" . julia)
                ("ein-wolframlanguage12" . wolfram))))
    )
