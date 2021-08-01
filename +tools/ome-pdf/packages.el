(setq ome-pdf-packages
      '(
        (simple-httpd :location (recipe
                                 :fetcher github
                                 :repo "skeeto/emacs-web-server"))
        (web-server :location (recipe
                               :fetcher github
                               :repo "eschulte/emacs-web-server"))
        pdf-tools
        ))

(defun ome-pdf/post-init-pdf-tools ()
  (advice-add 'org-docview-open :around #'my-org-docview-open-hack)
  (setq-default pdf-view-display-size 'fit-height)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotation t)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)

  (defun sync-pdf-in-pdfjs (&optional file)
    "Open current PDF in the corresponding page in PDF.js."
    (interactive)
    (or file
        (setq file (buffer-name))
        (error "Current buffer has no file"))
    (let* ((browse-url-browser-function 'browse-url-chrome) ;; Should match to CORS-enabled server that points to PDF directory
          (port 9005)
          (urlf (format "%s?file=%s#page=%d"
                        "file:///home/xhyang/src/pdf.js/web/viewer.html"
                        (format "http://localhost:%d/%s" port (url-hexify-string file))
                        (pdf-view-current-page))))
      (browse-url urlf)
      (run-hooks 'browse-url-of-file-hook)))

  ;; Start a CORS-enabled web-server from within Emacs, so that PDF.js can synchronize with pdf-tools
  ;; (lexical-let ((docroot (expand-file-name "books/clojure" (concat note-root-dir "/org/"))))
  ;;   (ws-start
  ;;    (lambda (request)
  ;;      (with-slots (process headers) request
  ;;        (let ((path (substring (cdr (assoc :GET headers)) 1)))
  ;;          (if (ws-in-directory-p docroot path)
  ;;              (if (file-directory-p path)
  ;;                  (ws-send-directory-list process
  ;;                                          (expand-file-name path docroot) "^[^\.]")
  ;;                (ws-response-header process 200 '("Access-Control-Allow-Origin" . "*"))
  ;;                (ws-send-file process (expand-file-name path docroot)))
  ;;            (ws-send-404 process)))))
  ;;    9005 nil :name (format "pdfjs-%s" docroot)))
  )

(defun ome-pdf/init-simple-httpd ()
  (use-package simple-httpd

    )
  )

(defun ome-pdf/init-web-server ()
  (use-package web-server

    )
  )
