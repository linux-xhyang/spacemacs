(defconst ome-opengrok-packages
  '(
    eopengrok
    ))

(defun ome-opengrok/init-eopengrok ()
  (use-package eopengrok
    :config
    (progn
      (require 'eopengrok)

      (define-key global-map (kbd "C-c s i") 'eopengrok-create-index)
      (define-key global-map (kbd "C-c s I") 'eopengrok-create-index-with-enable-projects)
      (define-key global-map (kbd "C-c s d") 'eopengrok-find-definition)
      (define-key global-map (kbd "C-c s f") 'eopengrok-find-file)
      (define-key global-map (kbd "C-c s s") 'eopengrok-find-reference)
      (define-key global-map (kbd "C-c s t") 'eopengrok-find-text)
      (define-key global-map (kbd "C-c s h") 'eopengrok-find-history)
      (define-key global-map (kbd "C-c s c") 'eopengrok-find-custom)
      (define-key global-map (kbd "C-c s b") 'eopengrok-resume)
      )))
