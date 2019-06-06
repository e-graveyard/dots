;        __    ___ ___      __      ___    ____
;      /'__`\/' __` __`\  /'__`\   /'___\ /',__\
;   __/\  __//\ \/\ \/\ \/\ \L\.\_/\ \__//\__, `\
;  /\_\ \____\ \_\ \_\ \_\ \__/.\_\ \____\/\____/
;  \/_/\/____/\/_/\/_/\/_/\/__/\/_/\/____/\/___/
;
; author: cai <hi@caian.org>
;   code: github.com/caian-org/dots


; my lisp functions & stuff
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

; spaces > tabs
(setq-default indent-tabs-mode nil)

; add spacing between lines
(setq-default line-spacing 5)

; always show line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

; scroll smoothly
(setq scroll-step            1
      scroll-margin          25
      scroll-conservatively  10000)

; "org-bullet" bullet characters
(custom-set-variables
 '(org-bullets-bullet-list (quote ("▶" "✸" "✚" "➔" "◇"))))

; open my notes on startup
(find-file "~/Projs/notes.org/notes/c.org")
