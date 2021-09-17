(setq ome-wolfram-packages
    '(
         lsp-mode
         wolfram-mode
         company
         ))


;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun ome-wolfram/init-wolfram-mode ()
    (use-package wolfram-mode
        :defer t
        :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
        :mode "\\.wl\\'"
        :init
        (progn
            (add-hook 'wolfram-mode-hook #'ome-wolfram/wolfram-setup-lsp))))

(defun ome-wolfram/post-init-lsp-mode ()

    (with-eval-after-load 'lsp-mode
        (add-to-list 'lsp-language-id-configuration '(wolfram-mode . "Mathematica"))

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
                :major-modes '(wolfram-mode)
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
