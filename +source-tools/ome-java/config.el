(unless (macrop 'spacemacs|add-company-backends)
  (spacemacs|defvar-company-backends java-mode)
  )

(spacemacs|define-jump-handlers java-mode)
