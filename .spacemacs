;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.



(require 'package)
(setq package-archives '(
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ;("marmalade" . "http://marmalade-repo.org/packages/")
			 ;("melpa" . "http://melpa.milkbox.net/packages/")
			 ))

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
     better-defaults
     emacs-lisp
     gtags
     ycmd
     semantic
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     java
     git
     common-lisp
     python
     ipython-notebook
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup nil
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t
                      auto-completion-complete-with-key-sequence "ja"
                      auto-completion-complete-with-key-sequence-delay 0.1
      )
     ;; markdown
     org
     (chinese :variables
              chinese-enable-youdao-dict t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (python :variables python-enable-yapf-format-on-save t)
     shell-scripts
     spell-checking
     syntax-checking
     html
     chrome
     ;; version-control ;;not user for git gutter
     ome
     ome-git
     ome-speedbar
     ;;ome-ecb
     ome-ccl
     ome-realgud
     ome-ggtags
     ome-systemtap
     ome-gui
     music
     ;;symon
     android-mode
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      (ggtags :location (recipe :fetcher github :repo "linux-xhyang/ggtags")))
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
   dotspacemacs-check-for-update t
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
   dotspacemacs-startup-banner 'official
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
   dotspacemacs-themes '(solarized-dark
                         spacemacs-dark
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
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
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
  ;; remove trailing whitespaces before save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; enable to support navigate in camelCase words
  (global-subword-mode t)
  (global-set-key "\M-'" 'set-mark-command)
  (global-set-key "\M-r" 'replace-string)

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
  (load-file (concat user-emacs-directory "private/init.el"))
  (add-hook 'org-mode-hook 'spacemacs/toggle-spelling-checking-on)
  (add-hook 'nroff-mode-hook 'spacemacs/toggle-spelling-checking-on)
  (add-hook 'js2-mode-hook 'spacemacs/toggle-spelling-checking-off)
  (add-hook 'clojure-mode-hook 'spacemacs/toggle-spelling-checking-off)
  (add-hook 'emacs-lisp-mode-hook 'spacemacs/toggle-spelling-checking-off)
  (setq projectile-global-mode t)
  ;;remove all keybinds from insert-state keymap,use emacs-state when editing
  (setcdr evil-insert-state-map nil)
  ;;ESC to switch back normal-state
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
  ;; Do not write anything past this comment. This is where Emacs will
  ;; auto-generate custom variable definitions.
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (32 95 41 46 34 39 60 62)))
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 3)
 '(company-selection-wrap-around t)
 '(company-show-numbers t)
 '(semantic-idle-scheduler-idle-time 0.3)
 '(ggtags-highlight-tag 0.1)
 '(semantic-idle-summary-function (quote semantic-format-tag-short-doc))
 '(semantic-idle-truncate-long-summaries nil)
 '(ggtags-sort-by-nearness t)
 '(gdb-many-windows t t)
 '(gdb-show-main t t)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:diff-option "-w")
 '(git-gutter:hide-gutter t)
 '(git-gutter:modified-sign "  ")
 '(global-linum-mode t)
 '(helm-grep-default-command "grep --color=always -a -d recurse %e -n%cH -e %p %f")
 '(helm-gtags-path-style (quote relative))
 '(isend-send-region-function (quote isend--ipython-cpaste))
 '(jit-lock-chunk-size 6000)
 '(jit-lock-context-time 0.1)
 '(jit-lock-contextually t)
 '(jit-lock-defer-time 0.2)
 '(jit-lock-stealth-load 100)
 '(jit-lock-stealth-nice 0.5)
 '(jit-lock-stealth-time nil)
 '(ein:use-auto-complete-superpack t)
 '(large-file-warning-threshold 10000)
 '(magit-diff-arguments
   (quote
    ("--ignore-all-space" "--no-ext-diff" "--stat" "-- " "--diff-algorithm=default")))
 '(magit-diff-section-arguments
   (quote
    ("--ignore-all-space" "--no-ext-diff" "--diff-algorithm=default")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (shell . t))))
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
 '(package-selected-packages
   (quote
    (isend-mode realgud test-simple loc-changes load-relative eclim youdao-dictionary xterm-color xgtags ws-butler window-numbering which-key web-mode volatile-highlights vlf vi-tilde-fringe use-package toc-org tagedit stickyfunc-enhance srefactor sr-speedbar spacemacs-theme spaceline solarized-theme smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-yapf popwin pip-requirements persp-mode pcre2el paradox pangu-spacing page-break-lines orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets open-junk-file nlinum neotree multi-term move-text magit-gitflow lorem-ipsum linum-relative leuven-theme less-css-mode jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flyspell helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate golden-ratio gnuplot gmail-message-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe git-emacs ggtags flycheck-ycmd flycheck-pos-tip flx-ido fish-mode find-by-pinyin-dired fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-smartparens evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu eshell-prompt-extras esh-help emms emmet-mode emacs-eclim elogcat elisp-slime-nav ein edit-server ecb dts-mode disaster define-word cython-mode company-ycmd company-web company-statistics company-quickhelp company-c-headers company-anaconda cmake-mode clean-aindent-mode clang-format chinese-pyim buffer-move bracketed-paste auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile android-mode aggressive-indent adaptive-wrap ace-window ace-pinyin ace-link ace-jump-helm-line ac-slime ac-ispell)))
 '(projectile-enable-caching t)
 '(vlf-application (quote dont-ask))
 '(vlf-batch-size 10485760)
 '(vlf-tune-enabled t)
 '(vlf-tune-max 402702600)
 '(which-key-popup-type (quote minibuffer))
 '(which-key-idle-delay 0.6)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(which-function-mode t)
 '(yahoo-weather-location "北京")
 '(yahoo-weather-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  )
