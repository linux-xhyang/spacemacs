(global-set-key [double-mouse-1] 'select-word)

(defun clerk-show ()
    (interactive)
    (save-buffer)
    (let
        ((filename
             (buffer-file-name)))
        (when filename
            (cider-interactive-eval
                (concat "(nextjournal.clerk/show! \"" filename "\")")))))

(global-set-key [f9] 'clerk-show)
