;; Add MELPA and ublt repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(package-initialize)

;; Ensure use-package is installed and loaded
(unless (package-installed-p 'use-package)
  (message "Installing use-package...")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))
(require 'use-package)

;; Enable global font lock mode
(global-font-lock-mode t)

;; Install and load a theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Enable rainbow-mode for color codes
(use-package rainbow-mode
  :ensure t
  :hook (css-mode . rainbow-mode)
  :hook (html-mode . rainbow-mode))

;; Enable highlight-indent-guides for indentation colors
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

;; Enable lsp-mode for advanced highlighting (excluding emacs-lisp-mode)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :custom
  ;; Define language IDs for specific modes (exclude emacs-lisp-mode)
  (lsp-language-id-configuration
   '((python-mode . "python")
     (js-mode . "javascript")
     (typescript-mode . "typescript")
     (go-mode . "go")
     (rust-mode . "rust")
     (c-mode . "c")
     (c++-mode . "cpp")
     (web-mode . "html"))))

;; Suppress compiler warnings for docstrings
(setq byte-compile-warnings '(not docstrings))

;; Enable native compilation for faster startup
(setq package-native-compile t)
(setq use-package-always-ensure t)

;; GUI Customizations (if running in a window system)
(when (window-system)
  (tool-bar-mode -1)       ;; Disable toolbar
  (scroll-bar-mode -1)     ;; Disable scrollbars
  (tooltip-mode -1))       ;; Disable tooltips

;; Set tab width to 4 spaces and use spaces instead of tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Enable electric pair mode for automatic bracket pairing
(electric-pair-mode 1)

;; Enable line numbering globally
(global-display-line-numbers-mode)

;; Optional: Customize line number appearance
(custom-set-faces
 '(line-number ((t (:foreground "gray40"))))
 '(line-number-current-line ((t (:foreground "white" :weight bold)))))

;; Ensure company-mode is installed and loaded
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2) ;; Delay before showing completions
  (setq company-tooltip-limit 10) ;; Limit the number of candidates shown
  (setq company-minimum-prefix-length 2)) ;; Minimum prefix length for suggestions

;; Enable yasnippet for code snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Load org-mode (no need for org-install in modern Emacs)
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))
(require 'org)

;; Increase max lisp eval depth if necessary
(setq max-lisp-eval-depth 2000)

;; Add all installed packages to load-path
(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Disable swap files completely
(setq auto-save-default nil)          ;; Disable autosave files (e.g., #file#)
(setq create-lockfiles nil)           ;; Disable lock files (e.g., .#file)
(setq make-backup-files nil)          ;; Disable backup files (e.g., file~)

;; Suppress warnings about docstring quoting
(advice-add 'byte-compile-warn-about-docstrings :override #'ignore)

;; Optionally, disable line numbers in specific modes (e.g., org-mode)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode highlight-indent-guides rainbow-mode doom-themes yasnippet company)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
