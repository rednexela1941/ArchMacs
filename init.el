(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(slime-company slime rust-mode clang-format magit multiple-cursors company-jedi company-go go-autocomplete go-complete exec-path-from-shell julia-mode go-eldoc humanoid-themes go-mode gruvbox-theme c-eldoc lsp-mode json-mode yapfify js2-mode tern scss-mode haskell-mode company-mode company-web web-mode tide ## web-beautify typescript-mode doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;-------Display-------
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default tab-width 4)
(set-frame-font "Source Code Pro 10")
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq x-super-keysym 'meta)
(global-unset-key (kbd "C-z"))

;;-------Themes-------
;;https://github.com/hlissner/emacs-doom-themes -- Doom Themes
;;https://github.com/greduan/emacs-theme-gruvbox -- Gruvbox Themes
;;(load-theme 'doom-one t)
(load-theme 'gruvbox-dark-medium t)
;;(load-theme 'humanoid-dark t)
(global-set-key (kbd "<f6>")
  (lambda() (interactive) (find-file "~/.emacs.d/init.el")
    ))

;;-------Magit-------
(global-set-key (kbd "C-x g") 'magit-status)

;;-------Company Mode-------
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)
(icomplete-mode 1)

;;-------Go Lang-------
(global-set-key (kbd"C-c C-c") 'godef-jump)
(add-to-list 'exec-path "~/go/bin") 
;; Automatically format code on save
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)


;; Multiple cursor setiup.
(defun start-multiple-cursors()
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(start-multiple-cursors)


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
)

(defun tsx-mode ()
  (interactive)
  (web-mode)
  (company-mode)
  (tide-mode)
  (flycheck-mode)
  (tide-restart-server)
)

(defun setup-css-mode ()
  (interactive)
  (scss-mode)
  (company-mode)
  )

(defun setup-ts-mode ()
  (interactive)
  (typescript-mode)
  (company-mode)
  (tide-setup)
  (flycheck-mode)
  )

(defun setup-c-mode ()
  (interactive)
  (c-mode)
  (company-mode)
  (c-turn-on-eldoc-mode)
  )

(defun setup-go-mode ()
  (interactive)
  (go-mode)
  ((lambda ()
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)))
  (company-mode)
  (flycheck-mode)
  )

(defun setup-julia-mode ()
  (interactive)
  (julia-mode)
  (company-mode)
  )

(defun setup-python-mode ()
  (interactive)
  (python-mode)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode)
  (yapf-mode)
  )

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . setup-css-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . setup-ts-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . setup-c-mode))
(add-to-list 'auto-mode-alist '("\\.jl\\'" . setup-julia-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . setup-python-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . setup-go-mode))

(add-hook 'python-mode-hook 'yapf-mode)
(add-hook 'python-mode-hook 'company-mode)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(defun save-scss ()
  (when (eq major-mode 'scss-mode)
    (web-beautify-css))
  )

(add-hook 'before-save-hook #'save-scss)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(add-to-list 'load-path "/home/alex/.emacs.d/elpa/")
;;(require 'py-yapf)
;;(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;; Rust.
(setq rust-format-on-save t)



(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
		(c-mode . "k&r")
        (other . "gnu")))

;; tag a line for jumping.
(defun jmp-tag-line () 
  (interactive)
  (setq jmp-tag (line-number-at-pos))
  (message (concat "Set jmp-tag to line: " (number-to-string jmp-tag)))
  )

;; Return to position jmped from.
(defun jmp-back ()
  (interactive)
  (if last-jmp-tag 
	  (progn
		(goto-line last-jmp-tag)
		(message (concat "Jumped back to line: " (number-to-string last-jmp-tag))))
	(message "No last-jmp-tag found.")
	)
  )

;; jmp to tag.
(defun jmp-to-tag ()
  (interactive)
  (if jmp-tag
	  (progn
		(setq last-jmp-tag (line-number-at-pos))
		(goto-line jmp-tag)
		(message (concat "Jumped to line: " (number-to-string jmp-tag))))
	(message "No jmp-tag set.")
	)
  )

;; jump keys.
(global-set-key (kbd "C-x J") 'jmp-back) ;; Tag current line
(global-set-key (kbd "C-x j") 'jmp-to-tag) ;; Jmp to tagged line.
(global-set-key (kbd "M-j") 'jmp-tag-line) ;; Jmp back.

;; Perl regex expressions.

;; Slime
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy slime-company))


;; use (shell-command-to-string "ls") to execute shell commands.
