(setq ome-wolfram-packages
    '(
         wolfram-mode
         lsp-mode
         ))


;; .m files are not associated because conflict with more common Objective-C and
;; MATLAB/Octave, manually invoke for .m files.
(defun ome-wolfram/init-wolfram-mode ()
    (use-package wolfram-mode
        :defer t
        :interpreter "\\(Wolfram\\|Mathematica\\)Script\\( -script\\)?"
        :mode "\\.wl\\'"))

(defun ome-wolfram/post-init-lsp-mode ()
    (with-eval-after-load 'lsp-mode
        (add-to-list 'lsp-language-id-configuration '(wolfram-mode . "Mathematica"))

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
                )))
    )
