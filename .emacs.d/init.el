(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
	                      (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
		     Your version of Emacs does not support SSL connections,
		     which is unsafe because it allows man-in-the-middle attacks.
		     There are two things you can do about this warning:
		     1. Install an Emacs version that does support SSL and be safe.
		     2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(when (not package-archive-contents)
    (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-general)
(if (version< emacs-version "24.4")
    (require 'setup-ivy-counsel)

(require 'setup-cedet)
(require 'setup-editing)
(setq c-default-style "linux")
;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zygospore yasnippet ws-butler volatile-highlights use-package undo-tree iedit counsel-projectile company clean-aindent-mode anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(add-to-list 'default-frame-alist '(font . "SF Mono-14"))

;; aggressively highlight trailing whitespace
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))
;; TODO highlight multiple blank lines
;; tabs have always been, and always will be, 8 wide
(setq-default tab-width 8)
;; C/C++ indentation
(defun c-mode-indent-setup-hook ()
  (c-set-offset 'innamespace 0)
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil))
(add-hook 'c-mode-hook 'c-mode-indent-setup-hook)
(add-hook 'c++-mode-hook 'c-mode-indent-setup-hook)

(color-theme-sanityinc-tomorrow-night)

(require 'powerline)
(powerline-default-theme)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :family "Hack")))))

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-unset-key (kbd "C-z"))

(unless (package-installed-p 'elpy)
  (package-install 'elpy))

(elpy-enable)

(unless (package-installed-p 'py-autopep8)
  (package-install 'py-autopep8))
(unless (package-installed-p 'blacken)
  (package-install 'blacken))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(toggle-scroll-bar -1)
