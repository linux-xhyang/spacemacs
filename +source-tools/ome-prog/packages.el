(defconst ome-prog-packages '(
                                 lsp-mode
                                 ccls) "docstring")

(defun ome-prog/post-init-lsp-mode ()
    ;;fix lsp-mode crash bug
	(advice-add 'json-parse-string :around
		(lambda (orig string &rest rest)
			(apply orig (s-replace "\\u0000" "" string)
				rest)))
	;; fix lsp java company hang
	(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

    (setq lsp-auto-execute-action nil
        lsp-auto-guess-root t

        ;; Turn off for better performance
        lsp-enable-dap-auto-configure nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-imenu nil
        lsp-enable-indentation nil           ; no region formatting
        lsp-enable-links nil                 ; no clickable links
        lsp-enable-on-type-formatting nil    ; no formatting on the fly
        lsp-enable-snippet t               ; handle yasnippet manually
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil

        ;; Disable lsp checker
        lsp-diagnostic-package :none
        ;; Lean on flycheck-check-syntax-automatically
        lsp-flycheck-live-reporting nil
        ;; Auto kill lsp server
        lsp-keep-workspace-alive nil

        ;; Prune minibuffer
        lsp-signature-doc-lines 5
        lsp-signature-render-documentation nil
        ;; Clean headerline
        lsp-headerline-breadcrumb-enable nil
        ;; Clean modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil)
    )

(defun ome-prog/post-init-ccls ()
    (with-eval-after-load 'ccls
        (setq ccls-initialization-options
            '(:completion (:detailedLabel t)
			     :index (:initialReparseForDependency :json-false :comments 2 :threads 2)
			     :cache (:retainInMemory 2 :format "binary")
			     :diagnostics (:onChange 1000 :onOpen 0 :onSave 0)
			     :clang
			     (:excludeArgs
			         ;; Linux's gcc options. See ccls/wiki
			         ["-falign-jumps=1" "-falign-loops=1" "-fconserve-stack" "-fmerge-constants"
			             "-fno-code-hoisting" "-fno-schedule-insns" "-fno-var-tracking-assignments" "-fsched-pressure"
			             "-mhard-float" "-mindirect-branch-register" "-mindirect-branch=thunk-inline" "-mpreferred-stack-boundary=2"
			             "-mpreferred-stack-boundary=3" "-mpreferred-stack-boundary=4"
			             "-mrecord-mcount" "-mindirect-branch=thunk-extern" "-mno-fp-ret-in-387"
			             "-mskip-rax-setup" "-fno-strict-aliasing" "-fno-common" "-fno-PIE" "-fno-PIE" "-fno-dwarf2-cfi-asm"
			             "-fno-omit-frame-pointer" "-fno-ipa-sra" "-funwind-tables" "-march=armv8-a" "-march=armv7-a"
			             "--param=allow-store-data-races=0" "-Wa arch/x86/kernel/macros.s"]
			         :extraArgs ["--gcc-toolchain=/usr"])))))
