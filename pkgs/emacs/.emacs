;        __    ___ ___      __      ___    ____
;      /'__`\/' __` __`\  /'__`\   /'___\ /',__\
;   __/\  __//\ \/\ \/\ \/\ \L\.\_/\ \__//\__, `\
;  /\_\ \____\ \_\ \_\ \_\ \__/.\_\ \____\/\____/
;  \/_/\/____/\/_/\/_/\/_/\/__/\/_/\/____/\/___/
;
; author: cai <hi@caian.org>
;   code: github.com/caian-org/dots


; list of used packages
(setq package-list
      '(evil org-plus-contrib powerline powerline-evil
        xresources-theme org-bullets))

; list of package repositories
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

; initialize the packages
(require 'package)
(package-initialize)

; fetches the list of available packages
(unless package-archive-contents
  (or (file-exists-p package-user-dir) (package-refresh-contents)))

; iterates through the list of packages and install them
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; require packages
(require 'evil)
(require 'powerline)
(require 'powerline-evil)
(require 'org-bullets)

; set the theme
(require 'xresources-theme)

; enable the org-bullets package on org-mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; add left and right margins on the buffer
(setq scroll-margin 6)

; do not show the truncate line symbol ($)
(setq-default truncate-lines t)

; do not show the startup page
(setq inhibit-startup-screen t)

; set the font type and size
(set-frame-font "xos4 Terminus 14" nil t)

; set transparency (85%)
(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

; initializes evil mode
(evil-mode 1)

; uses the default theme in powerline
(powerline-evil-center-color-theme)

; disables the menu bar
(menu-bar-mode -1)

; disables the tool bar
(tool-bar-mode -1)

; disable the scroll bar
(scroll-bar-mode -1)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(setq scroll-step            1
      scroll-margin          25
      scroll-conservatively  10000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (linum-relative xresources-theme dashboard powerline-evil powerline org-plus-contrib evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
