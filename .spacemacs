;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.



(require 'package)
(setq package-archives '(
       ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
       ("org"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
       ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))

;(setq package-archives
;      '(("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
;        ("org-cn"   . "https://elpa.emacs-china.org/org/")
;        ("gnu-cn"   . "https://elpa.emacs-china.org/gnu/")))

; Apparently needed for the package auto-complete (why?)
;(add-to-list 'package-archives
;             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(setq url-http-attempt-keepalives nil)

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;;nlinum
     debug
     (spell-checking :variables
                     spell-checking-enable-by-default t)
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     better-defaults
     gtags
     dash
     slack
     git
     cmake
     chrome
     confluence
     plantuml
     ansible
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (treemacs :variables treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-collapsed-directories 3)
     ;; markdown
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js t
          ;; org-enable-org-journal-support t
          ;; org-projectile-file "TODOs.org"
          org-want-todo-bindings t)
     (chinese :variables
              chinese-enable-youdao-dict t
              chinese-default-input-method 'pinyin)
     ;;semantic
     dap
     pdf
     (lsp :variables
          lsp-ui-sideline-enable t)
     shell-scripts
     emacs-lisp
     common-lisp
     groovy
     html
     javascript
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (latex :variables
            latex-build-command "Latex"
            latex-enable-auto-fill t
            latex-enable-folding t
            latex-enable-magic t)
     (java :variables
           java-backend 'lsp
           java--ensime-modes nil)
     (python :variables
             spacemacs--python-pyenv-modes '(python-mode)
             python-auto-set-local-pyenv-version 'on-visit ;;'on-project-switch
             python-auto-set-local-pyvenv-virtualenv 'on-visit ;;'on-project-switch
             python-enable-yapf-format-on-save t
             python-fill-column 99
             python-sort-imports-on-save t
             python-backend 'lsp) ;;anaconda
     ipython-notebook
     (go :variables
             go-use-gometalinter t
             gofmt-command "goimports"
             go-backend 'lsp
             godoc-at-point-function 'godoc-gogetdoc)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-rtags-completion nil
            c-c++-backend 'lsp-cquery
            c-c++-adopt-subprojects t
            c-c++-lsp-executable (file-truename "~/src/cquery/build/cquery")
            c-c++-lsp-cache-dir ".cquery_cached_index"
            c-c++-enable-google-style t
            c-c++-lsp-initialization-options '(:index (:comments 2 :threads 1) :discoverSystemIncludes :json-false :cacheFormat "msgpack" :completion (:detailedLabel t)))
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t
                      :disabled-for org markdown)
     ;; version-control ;;not user for git gutter
     android-mode
     ;; ome-projectile
     ;; ome-prepare
     ome
     ;; ome-java
     ;; ome-groovy
     ome-git
     ome-speedbar
     ;; ome-ecb
     ;; ome-ccl
     ;; ome-realgud
     ;; ome-ggtags
     ome-systemtap
     ome-gui
     autopair
     doxymacs
     ;; ome-opengrok
     ;; ome-lsp
     ;; music
     ;; symon
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; dotspacemacs-additional-packages '(
   ;;                                    (ggtags :location (recipe :fetcher github :repo "linux-xhyang/ggtags")))
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         solarized-dark
                         spacemacs-light
                         solarized-light
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
         ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup t
   dotspacemacs-enable-lazy-installation nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq gc-cons-threshold (* 511 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 10 t #'garbage-collect)
  (setq garbage-collection-messages t)
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  ;; set environment coding system
  (set-language-environment "UTF-8")
  ;; auto revert buffer globally
  (global-auto-revert-mode t)
  ;; set TAB and indention
  (setq-default tab-width 8)
  (setq-default indent-tabs-mode nil)
  ;; y or n is suffice for a yes or no question
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; always add new line to the end of a file
  (setq require-final-newline t)
  ;; add no new lines when "arrow-down key" at the end of a buffer
  (setq next-line-add-newlines nil)
  ;; prevent the annoying beep on errors
  (setq ring-bell-function 'ignore)
  ;; enable to support navigate in camelCase words
  (global-subword-mode t)
  (global-set-key "\M-'" 'set-mark-command)
  (global-set-key "\M-r" 'replace-string)
  (setq-default dotspacemacs-large-file-size 10)
  (setq-default header-line-format
                '((which-function-mode ("" which-func-format " "))))
  (which-function-mode 1)
  (setq spaceline-org-clock-p t)
  (defun org-clock-persist-save-file ()
    (if (equal 'windows-nt system-type)
        (if (file-exists-p "D:/note/my-org.el")
            "D:/note/org-clock-save.el"
          "~/.emacs.d/org-clock-save.el"
          )
      (if (file-exists-p "~/note/my-org.el")
          "~/note/org-clock-save.el"
        "~/.emacs.d/org-clock-save.el"
        )))
  ;;debug
  ;;(toggle-debug-on-quit)
  ;;https://github.com/dholm/benchmark-init-el.git
  ;;make
  ;;(add-to-list 'load-path "~/.emacs.d/benchmark-init-el/")
  ;;(require 'benchmark-init-loaddefs)
  ;;(benchmark-init/activate)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (require 'url)
  (unless (file-exists-p "~/.emacs.d/.cache/pyim-bigdict.pyim.gz")
    (url-copy-file "http://tumashu.github.io/pyim-bigdict/pyim-bigdict.pyim.gz" "~/.emacs.d/.cache/pyim-bigdict.pyim.gz" t)
    )
  (require 'pyim-basedict)
  (setq-default pyim-dicts
                (quote
                 ((:name "词库1" :file "/home/xhyang/.emacs.d/private/local/pyim-greatdict.pyim.gz")
                  (:name "词库2" :file "~/.emacs.d/.cache/pyim-bigdict.pyim.gz"))))
  (pyim-basedict-enable)

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english pyim-probe-org-speed-commands pyim-probe-program-mode))
  (global-set-key (kbd "M-i") 'pyim-convert-string-at-point)

  (load-file (concat user-emacs-directory "private/init.el"))
  (add-hook 'org-mode-hook 'spacemacs/toggle-spelling-checking-on)
  (add-hook 'nroff-mode-hook 'spacemacs/toggle-spelling-checking-on)
  (add-hook 'js2-mode-hook 'spacemacs/toggle-spelling-checking-off)
  (add-hook 'clojure-mode-hook 'spacemacs/toggle-spelling-checking-off)
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/toggle-spelling-checking-off)
  ;;(add-hook 'semantic-mode-hook 'spacemacs/toggle-semantic-stickyfunc-globally-off)
  ;;remove all keybinds from insert-state keymap,use emacs-state when editing
  (setcdr evil-insert-state-map nil)
  ;;ESC to switch back normal-state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (setq alert-default-style 'notifications)
  (server-start)
  (dotspacemacs/emacs-custom-settings)
  )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-mode-hook
   (quote
    (spacemacs/load-yasnippet smartparens-mode flyspell-mode evil-matchit-mode turn-on-reftex TeX-PDF-mode TeX-source-correlate-mode LaTeX-math-mode TeX-fold-mode latex/auto-fill-mode spacemacs//init-company-LaTeX-mode company-mode pdf-tools-install)) t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %`%(extraopts) %S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %T" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %`%(extraopts) %S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %s" TeX-run-command nil t :help "Run makeglossaries to create glossary file")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("upMendex" "upmendex %s" TeX-run-index t t :help "Run upmendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-engine (quote xetex))
 '(TeX-source-correlate-start-server t t)
 '(TeX-view-program-list (quote (("pdf-tools" (TeX-pdf-tools-sync-view) ""))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
 '(ahs-default-range (quote ahs-range-display))
 '(calculator-number-digits 6)
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 95 41 46 34 39 60 62)))
 '(company-backends
   (quote
    (company-capf company-files company-semantic
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-dabbrev)))
 '(company-idle-delay 10)
 '(company-minimum-prefix-length 3)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(cquery-cache-dir-function
   (function
    (lambda
      (dir)
      (expand-file-name cquery-cache-dir
                        (if
                            (cquery--suggest-project-root)
                            (cquery--suggest-project-root)
                          (projectile-project-root dir))))))
 '(cquery-extra-args (quote ("--log-file=~/cq.log")))
 '(cquery-project-root-matchers
   (quote
    ("compile_commands.json" ".cquery" "build/compile_commands.json" projectile-project-root)))
 '(default-input-method "pyim")
 '(ein:complete-on-dot t)
 '(ein:completion-backend (quote ein:use-company-backend))
 '(ein:use-auto-complete-superpack t)
 '(eopengrok-ignore-file-or-directory
   ".opengrok:out:*.so:*.a:*.o:*.gz:*.bz2:*.jar:*.zip:*.class:*.elc:GPATH:GRTAGS:GTAGS:.repo:.cquery_cached_index")
 '(gdb-many-windows t)
 '(gdb-show-main t)
 '(ggtags-highlight-tag 1)
 '(ggtags-sort-by-nearness t)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:diff-option "-w")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "  ")
 '(global-semantic-highlight-edits-mode t)
 '(global-semantic-highlight-func-mode t)
 '(global-semantic-idle-local-symbol-highlight-mode t nil (semantic/idle))
 '(global-semantic-stickyfunc-mode t)
 '(helm-grep-default-command "grep --color=always -a -d recurse %e -n%cH -e %p %f")
 '(helm-gtags-path-style (quote relative))
 '(helm-input-idle-delay 0.6)
 '(isend-send-region-function (quote isend--ipython-cpaste))
 '(jit-lock-chunk-size 6000)
 '(jit-lock-context-time 0.1)
 '(jit-lock-contextually t)
 '(jit-lock-defer-time 0.4)
 '(jit-lock-stealth-load 100)
 '(jit-lock-stealth-nice 0.5)
 '(jit-lock-stealth-time 16)
 '(large-file-warning-threshold 10000)
 '(lsp-auto-guess-root t)
 '(lsp-before-save-edits nil)
 '(lsp-enable-indentation nil)
 '(lsp-java-import-gradle-enabled nil)
 '(lsp-java-import-maven-enabled nil)
 '(lsp-log-max nil)
 '(lsp-response-timeout 6)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-sideline-show-flycheck t)
 '(lsp-ui-sideline-show-symbol nil t)
 '(magit-diff-arguments
   (quote
    ("--ignore-all-space" "--no-ext-diff" "--stat" "-- " "--diff-algorithm=default")))
 '(magit-diff-section-arguments
   (quote
    ("--ignore-all-space" "--no-ext-diff" "--diff-algorithm=default")))
 '(mouse-wheel-scroll-amount (quote (2)))
 '(org-clock-persist-file (org-clock-persist-save-file))
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-latex-hyperref-template nil)
 '(org-latex-pdf-process
   (quote
    ("xelatex -synctex=1 -interaction nonstopmode -output-directory %o %f" "bibtex %b" "xelatex -synctex=1 -interaction nonstopmode -output-directory %o %f")))
 '(org-preview-latex-default-process (quote dvisvgm))
 '(org-latex-inputenc-alist '(("utf8" . "utf8x")))
 '(org-preview-latex-process-alist
   (quote
    ((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -output-format=dvi  -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "xdv > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("xelatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O")))))
 '(projectile-enable-caching t)
 '(projectile-project-root-files-bottom-up
   (quote
    (".cquery" "compile_commands.json" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs")))
 '(projectile-project-root-files-top-down-recurring
   (quote
    (".svn" "CVS" "Makefile" ".git" ".cquery" "compile_commands.json")))
 '(projectile-require-project-root nil)
 '(python-shell-completion-native-enable nil)
 '(scroll-conservatively 100)
 '(semantic-idle-scheduler-idle-time 1)
 '(semantic-idle-scheduler-max-buffer-size 100000)
 '(semantic-idle-scheduler-work-idle-time 60)
 '(semantic-idle-summary-function (quote semantic-format-tag-short-doc))
 '(semantic-idle-truncate-long-summaries nil)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(tramp-syntax (quote default) nil (tramp))
 '(vc-handled-backends (quote (git RCS CVS SVN SCCS SRC Bzr Hg Mtn)))
 '(vlf-application (quote ask))
 '(vlf-batch-size 10485760)
 '(vlf-tune-enabled t)
 '(vlf-tune-max 402702600)
 '(which-function-mode t)
 '(which-key-idle-delay 0.6)
 '(which-key-popup-type (quote side-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(which-func ((t (:foreground "white smoke")))))
)
