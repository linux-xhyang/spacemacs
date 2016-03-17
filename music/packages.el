(defconst music-packages
  '(emms))

(defun kid-emms-info-simple (track)
  (when (eq 'file (emms-track-type track))
    (let ((regexp "/\\([^/]+\\)/\\([^/]+\\)\\.[^.]+$")
          (name (emms-track-name track)))
      (if (string-match regexp name)
          (progn
            (emms-track-set track 'info-artist (match-string 1 name))
            (emms-track-set track 'info-title (match-string 2 name)))
        (emms-track-set track
                        'info-title
                        (file-name-nondirectory name))))))


;; format current track,only display title in mode line
(defun bigclean-emms-mode-line-playlist-current ()
  "Return a description of the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and title)
                   title)))))

;; my customizable playlist format
(defun bigclean-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title))
        (album (emms-track-get track 'info-album))
        (ptime (emms-track-get track 'info-playing-time)))
    (if title
        (format
         "%-35s %-45s %-40s %5s:%-2s"
         (if artist artist "")
         (if title title "")
         (if album album "")
         (/ ptime 60)
         (% ptime 60)))))

(defun kid-emms-info-track-description (track)
  "Return a description of the current track."
  (let ((artist (emms-track-get track 'info-artist))
        (title (emms-track-get track 'info-title)))
    (format "%-10s +| %s"
            (or artist
                "")
            title)))

(defun my-emms-info-track-description (track)
  "Return a description of the current track."
  (if (and (emms-track-get track 'info-artist)
           (emms-track-get track 'info-title))
      (let ((pmin (emms-track-get track 'info-playing-time-min))
            (psec (emms-track-get track 'info-playing-time-sec))
            (ptot (emms-track-get track 'info-playing-time))
            (art  (emms-track-get track 'info-artist))
            (tit  (emms-track-get track 'info-title)))
        (cond ((and pmin psec) (format "%s - %s [%02s:%02s]" art tit pmin psec))
              (ptot (format  "%s - %s [%02s:%02s]" art tit (/ ptot 60) (% ptot 60)))
              (t (emms-track-simple-description track))))))

(defun my-emms-google-for-lyrics ()
  (interactive)
  (browse-url
   (concat "http://www.google.com/search?q="
           (replace-regexp-in-string " +" "+"
                                     (concat "lyrics "
                                             (delete ?- (emms-track-description
                                                         (emms-playlist-current-selected-track))))))))

(defun xwl-emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (name (emms-track-name track))
         (artist (emms-track-get track 'info-artist))
         (title (emms-track-get track 'info-title)))
    (concat emms-mode-line-icon-before-format
            (emms-propertize "NP:" 'display emms-mode-line-icon-image-cache)
            (format "[ %s ]"
                    (cond
                     ((and artist title)
                      (concat artist " - " title))
                     (title
                      title)
                     ((eq type 'file)
                      (file-name-sans-extension (file-name-nondirectory name)))
                     (t
                      (concat (symbol-name type) ":" name)))))))

(defun music/init-emms ()
  (use-package emms
    :init
    :config
    (progn
      (require 'emms-setup)
      (require 'emms-source-file)
      (require 'emms-source-playlist)
      (require 'emms-player-simple)
      (require 'emms-player-mplayer)
      (require 'emms-playlist-mode)
      (require 'emms-info)
      (require 'emms-cache)
      (require 'emms-mode-line)
      (require 'emms-playing-time)
      (require 'emms-score)
      (require 'emms-volume)
      (require 'emms-playlist-sort)
      (require 'emms-info-libtag)

      (defadvice emms-play-directory-tree (after emms-random-play-1 activate)
        "This advice to make `emms-random' execute after emms-play-directory-tree"
        (emms-random))

      (defadvice emms-play-directory-tree (after emms-playlist-sort-by-natural-order-1 activate)
        "This advice to make `emms-playlist-sort-by-natural-order' execute after emms-play-directory-tree"
        (emms-playlist-sort-by-natural-order))


      (emms-standard)
      (emms-default-players)
      (emms-mode-line 1)
      (emms-score 1)
      (emms-playing-time 1)

      (setq emms-score-file "~/Music/scores")
      ;; show lyrics
      (require 'emms-lyrics)
      ;; (emms-lyrics 1)
      ;; auto identify encode
      (require 'emms-i18n)
      ;; auto save and import playlist
      (require 'emms-history)

      (setq emms-cache-file "~/Music/emms-cache")
      (setq emms-history-file "~/Music/history")
      (setq emms-playlist-buffer-name "EMMS Music Playlist")
      (setq emms-playlist-default-major-mode 'emms-playlist-mode)
      (setq emms-playlist-mode-window-width (* 0.3 (frame-width)))

      ;;wget ftp://ftp.gnu.org/gnu/emms/emms-4.0.tar.gz
      ;;tar xvzf emms-4.0.tar.gz
      ;;sudo apt-get install libtagc0-dev
      ;;make emms-print-metadata
      ;;sudo cp src/emms-print-metadata /usr/bin
      ;;
      (if (executable-find "emms-print-metadata")
          (setq emms-info-functions '(emms-info-libtag))
        (message "not found emms-print-metadata")
        )

      (add-to-list 'emms-track-initialize-functions 'emms-info-initialize-track)
      ;; (add-to-list 'emms-info-functions 'kid-emms-info-simple)
      ;;
      ;; Mode line format
      ;; show info at mode-line
      ;; (setq emms-mode-line-mode-line-function 'bigclean-emms-mode-line-playlist-current)
      (setq emms-mode-line-mode-line-function 'xwl-emms-mode-line-playlist-current)
      (setq emms-mode-line-titlebar-function nil)
      ;; Playlist buffer format
      ;; (setq emms-last-played-format-alist
      ;;       '(((emms-last-played-seconds-today) . "%a %H:%M")
      ;;         (604800                           . "%a %H:%M") ; this week
      ;;         ((emms-last-played-seconds-month) . "%d")
      ;;         ((emms-last-played-seconds-year)  . "%m/%d")
      ;;         (t                                . "%Y/%m/%d")))

      (with-eval-after-load 'emms
        (setq xwl-emms-playlist-last-track nil)
        (setq xwl-emms-playlist-last-indent "\\")
        ;; (setq emms-track-description-function 'kid-emms-info-track-description)
        ;; (setq emms-track-description-function 'my-emms-info-track-description)
        (setq emms-track-description-function 'bigclean-emms-info-track-description)
        ;; (setq emms-track-description-function 'xwl-emms-track-description-function)
        )

      (setq emms-info-asynchronously nil)
      (setq emms-source-file-default-directory "~/Music/")

      (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

      ;; Lyrics
      (setq emms-lyrics-coding-system nil)     ;; let emacs to identify the encode of lyrics
      ;; (setq emms-lyrics-dir "/home/music/lyrics")

      ;; coding settings
      ;; (setq emms-info-mp3info-coding-system 'gbk)
      (setq emms-cache-file-coding-system 'utf-8-emacs)
      (setq emms-history-file-coding-system emms-cache-file-coding-system)
      (setq emms-i18n-default-coding-system '(no-conversion . no-conversion))
      ;; (setq emms-i18n-default-coding-system '(utf-8 . utf-8))

      (add-to-list 'file-coding-system-alist '("/[mM]usic/.*" gbk . gbk))

      (add-hook 'emms-player-started-hook 'emms-show) ; show the coming song
      (setq emms-show-format "Now Playing: %s")
      (setq emms-repeat-playlist t)   ; repeat at the end

      (setq emms-mode-line-format "[ %s "
            emms-playing-time-display-format "%s ]")
      (setq global-mode-string
            '("" emms-mode-line-string " " emms-playing-time-string))

      (setq emms-playing-time-style 'bar)

      (setq emms-player-mplayer-parameters '("-ao" "alsa" "-vo" "null"))
      ;;(setq emms-cache-file "~/.spacemacs.d/layer/music/emms/cache")

      (evil-leader/set-key
        "oo" 'emms-start
        "os" 'emms-stop
        "oc" 'emms-show
        "on" 'emms-next
        "op" 'emms-previous)
      (emms-history-load)
      )))
