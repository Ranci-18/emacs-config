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
(eval-when-compile (require 'use-package))

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

;;; init.el ends here
