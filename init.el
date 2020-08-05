(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)

(setq create-lockfiles nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
     `((".*" ,temporary-file-directory t)))

(icomplete-mode 1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq x-super-keysym 'meta)

(set-default-font "Source Code Pro 10")

(load-theme 'doom-one t)

(global-set-key (kbd "<f6>")
  (lambda() (interactive) (find-file "~/.emacs.d/init.el")
    ))

;; (defun ensure-packages-installed (&rest packages)
;;   (mapcar (lambda (package)
;;     (unless (package-installed-p package)
;;       (package-install package))) packages))
;; ;; Ensure company mode
;; (ensure-packages-installed
;;   'company)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (scss-mode haskell-mode company-mode company-web web-mode tide ## web-beautify typescript-mode doom-themes))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode )
  (web-mode)

  (eldoc-mode )
;;  (web-mode +1)
)


(defun tsx-mode ()
  (interactive)
  (web-mode)
  (company-mode)
  (tide-mode)
)


(defun setup-css-mode ()
  (interactive)
  (scss-mode)
  (company-mode)
 
)


(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . setup-css-mode))


;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)



(defun save-scss ()
  (when (eq major-mode 'scss-mode)
    (web-beautify-css))
  )

(add-hook 'before-save-hook #'save-scss)


;;(message "This message appears in the echo area!")

;;(add-hook 'before-save-hook 'css-save-hook)
;;(add-hook 'typescript-mode-hook #'setup-tide-mode)


