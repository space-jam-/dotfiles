(setq-default indent-tabs-mode nil)
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;(defadvice keyboard-escape-quit
;;  (around keyboard-escape-quit-dont-close-windows activate)
;;  (let ((buffer-quit-function (lambda () ())))
;;    ad-do-it))

;; https://www.jetbrains.com/lp/mono/
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 100)
(set-frame-font "JetBrains Mono" nil t)

(set-face-attribute 'fixed-pitch nil
  :font "JetBrains Mono"
  :height 100)

(use-package which-key
  :config
  (which-key-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
     :major-modes '(c-mode c++-mode)
     :remote? t
     :server-id 'clangd-remote))

  (lsp-register-custom-settings
    '(("pyls.plugins.pyls_black.enabled" t t)))

  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :bind ("M-RET" . completion-at-point)
  :hook
  ;; ((c-mode c++-mode) . lsp)
  (verilog-mode . lsp))

;; if you are ivy user
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :after lsp
    :hook (lsp-mode . lsp-ui-mode)
    :config
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-show))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package swiper)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist)
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
		     ))))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package prescient
  :after counsel
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after prescient
  :config
  (ivy-prescient-mode 1))

(use-package avy
  :bind (("C-c j" . avy-goto-char))
  :config
  (setq avy-all-windows nil))

(use-package ace-window
  :bind (("M-o" . ace-window)))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;;
;; Language-specific
;;

;; C

(use-package cc-mode :straight nil
  :config
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq c-default-style "K&R"))

;; Python

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

;; elisp

(use-package elisp-mode :straight nil
  :config
  (setq lisp-indent-offset 2))

;; YAML

(use-package yaml-mode)

;; Verilog
(use-package verilog-mode
  :config
  (setq electric-indent-inhibit t)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq verilog-auto-endcomments nil)
  (setq verilog-auto-indent-on-newline nil)
  (setq verilog-auto-newline nil)
  (setq verilog-indent-level 2)
  (setq verilog-indent-level-behavioral 2)
  (setq verilog-indent-level-declaration 2)
  (setq verilog-indent-level-module 0)
  (setq verilog-minimum-comment-distance 9000))

;; rice

(use-package all-the-icons
  :init
  (when (and (not (member "all-the-icons" (font-family-list)))
             (window-system))
    (all-the-icons-install-fonts t)))

(use-package monokai-pro-theme
  :ensure t
  :config
  (load-theme 'monokai-pro-spectrum t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package org
  :config
  (setq org-startup-with-inline-images t))

(use-package org-download)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :commands magit-status
  :bind ("C-c g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )

(load-file "~/.emacs.d/meow_bindings.el")
(use-package meow
  :ensure t
  :init
  (meow-global-mode 1)
  :config
  (meow-setup)
  (setq meow-global-mode 1))
