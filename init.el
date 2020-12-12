(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(monokai-theme sublime-themes nasm-mode slime-company slime rust-mode clang-format magit multiple-cursors company-jedi company-go go-autocomplete go-complete exec-path-from-shell julia-mode go-eldoc humanoid-themes go-mode gruvbox-theme c-eldoc lsp-mode json-mode yapfify tern scss-mode haskell-mode company-mode company-web web-mode tide ## web-beautify typescript-mode doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

;;-------Org-------
(setq org-todo-keywords
'((sequence "TODO" "FEEDBACK" "VERIFY" "TEST" "NOTE" "QUESTION" "DESIGN" "HACKED" "|" "DONE" "DELEGATED" "PASSED" "NOTED" "ANSWERED" "IMPLEMENTED" "CANCELLED" )))


;;-------Display-------
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-frame-font "Source Code Pro-8")
(setq column-number-mode 1)
(line-number-mode)
(setq-default tab-width 2)
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(setq x-super-keysym 'meta)
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x g") 'magit-status)

;;-------Themes-------
;;https://github.com/hlissner/emacs-doom-themes -- Doom Themes
;;https://github.com/greduan/emacs-theme-gruvbox -- Gruvbox Themes
;;(load-theme 'doom-acario-dark t)
;;(load-theme 'doom-one t)
;;(load-theme 'doom-vibrant t)
;;	(load-theme 'doom-outrun-electric t)
(if (display-graphic-p)
	(load-theme 'doom-vibrant t)
    (load-theme 'monokai t)
 )
;;(load-theme 'gruvbox-dark-medium t)
;;(load-theme 'humanoid-dark t)
;;(load-theme 'gruvbox-dark-medium t)
;;(load-theme 'gruvbox-dark-hard t)
;;(load-theme 'monokai t)
;;(load-theme 'humanoid-dark t)
;;(load-theme 'wilson t)

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

;; Perl Formatter: Requires https://github.com/perltidy/perltidy
(defun perltidy ()
  (interactive) 
  (let ((doc (buffer-string)) (tmpfile (concat "/tmp/emacs_perltidy_" (buffer-name) (s-replace ":" "" (s-replace " " "" (current-time-string))))))
	(progn 
	  (with-temp-file tmpfile
		(progn 
		  (write-region doc nil tmpfile)
		  (shell-command (concat "perltidy -ce " tmpfile))
		  )  
		)
	  (insert-file-contents (concat tmpfile ".tdy") nil nil nil t)
	  (shell-command (concat "rm "(concat tmpfile "*")))
	  )
	)
  )

(defun setup-perl-mode()
  (interactive)
  (perl-mode)
  (company-mode)
  (flycheck-mode)
  (add-hook 'before-save-hook #'perltidy 0 'local)
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
  (message "setup-css-mode")
  )

(defun setup-ts-mode ()
  (interactive)
  (typescript-mode)
  (company-mode)
  (tide-setup)
  (flycheck-mode)
  (message "setup-ts-mode")
  )

(defun setup-javascript-mode ()
  (interactive)
  (company-mode)
  (flycheck-mode)
  (javascript-mode)
  (message "setup-javascript-mode")
  )

(defun setup-c-mode ()
  (interactive)
  (c-mode)
  (company-mode)
  (c-turn-on-eldoc-mode)
  (message "setup-c-mode")
  )

(defun setup-go-mode ()
  (interactive)
  (go-mode)
  ((lambda ()
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)))
  (company-mode)
  (flycheck-mode)
  (message "setup-go-mode")
  )

(defun setup-julia-mode ()
  (interactive)
  (julia-mode)
  (company-mode)
  (message "setup-julia-mode")
  )

(defun setup-python-mode ()
  (interactive)
  (python-mode)
  (add-to-list 'company-backends 'company-jedi)
  (company-mode)
  (yapf-mode)
  (message "setup-python-mode")
  )

(add-to-list 'auto-mode-alist '("\\.pl\\'" . setup-perl-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . setup-css-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . setup-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . setup-js-mode))
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

(defun save-javascript ()
  (when (eq major-mode 'javascript-mode)
    (web-beautify-js))
  )

(add-hook 'before-save-hook #'save-scss)
(add-hook 'before-save-hook #'save-javascript)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

(add-to-list 'load-path "/home/alex/.emacs.d/elpa/")
;;(require 'py-yapf)
;;(add-hook 'python-mode-hook 'py-yapf-enable-on-save)

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
	))

(defun jmp-start ()
  (interactive)
  (progn
	(jmp-tag-line)
	(beginning-of-buffer)
	(message (concat "Set jmp-tag to line: " (number-to-string jmp-tag))
			 )))

(defun jmp-end ()
  (interactive)
  (progn
	(jmp-tag-line)
	(end-of-buffer)
	(message (concat "Set jmp-tag to line: " (number-to-string jmp-tag))
			 )))

(defun replace-region-matches ()
  (interactive)
  (if (region-active-p)
	  (let ((startpos (region-beginning)) (qstring (buffer-substring (region-beginning) (region-end))))		
		(let ((rstring (read-string (concat "Replace " qstring ": "))))
		  (progn 
			(replace-string qstring rstring nil (point-min) (point-max))
			(goto-char startpos)
			(message "Replaced %s with %s in buffer." qstring rstring)
		  )))
	(message "No region defined.")
	)
  )


(global-set-key (kbd "C-c C-r") 'replace-region-matches)
;; jump keys.
(global-set-key (kbd "C-x J") 'jmp-back) ;; Jmp back.
(global-set-key (kbd "C-x j") 'jmp-to-tag) ;; Jmp to tag
(global-set-key (kbd "M-j") 'jmp-tag-line) ;; Jmp tag line.
(global-set-key (kbd "M-<") 'jmp-start)
(global-set-key (kbd "M->") 'jmp-end)
;; Perl regex expressions.

;; Slime
(setq inferior-lisp-program "sbcl")
(slime-setup '(slime-fancy slime-company))
;; use (shell-command-to-string "ls") to execute shell commands.

