
(defun custom-meghanada--start-server-process (orig-fun &rest args)
  "TODO: FIX DOC ."
  (let ((jar (meghanada--locate-server-jar)))
  (if (file-exists-p jar)
      (let ((process-connection-type nil)
            (process-adaptive-read-buffering nil)
            (cmd (format "/usr/local/jdk8/bin/java %s %s -Dfile.encoding=UTF-8 -jar %s -p %d %s"
                         (meghanada--server-options)
                         meghanada-server-jvm-option
                         (shell-quote-argument jar)
                         meghanada-port
                         (if meghanada-debug
                             "-v"
                           "")))
            process)
        (message (format "launch server cmd:%s" cmd))
        (setq process
              (start-process-shell-command
               "meghanada-server"
               meghanada--server-buffer
               cmd))
        (buffer-disable-undo meghanada--server-buffer)
        (set-process-query-on-exit-flag process nil)
        (set-process-sentinel process 'meghanada--server-process-sentinel)
        (set-process-filter process 'meghanada--server-process-filter)
        (message "Meghanada-Server Starting ...")
        process)
    (message "%s"
             (substitute-command-keys
              "Missing server module. Type `\\[meghanada-install-server]' to install meghanada-server")))))

(defun ome/eclim-eclipse-valid ()
  (cl-loop for dir in eclim-eclipse-dirs
           when (and (message dir) (string-or-null-p dir) (file-exists-p dir) (file-exists-p (concat dir "/eclipse")))
               return dir
               ))

(defun ome/eclim-eclipse-directory ()
  (setq eclim-eclipse-dirs (append eclim-eclipse-dirs (list (getenv "HOME"))))
  (unless (ome/eclim-eclipse-valid)
    (let ((eclipse-dir (expand-file-name (read-directory-name "Eclipse Directory: "))))
      (custom-set-variables
       '(eclim-eclipse-dirs (list eclipse-dir))
       '(eclim-executable (concat eclipse-dir "/eclim"))
       ))))
