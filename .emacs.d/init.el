;;; Emacs init file

;; use separate file for auto-generated customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Load library functions
(load "~/.emacs.d/wm-lib" t t)

;; set initial window size
(setq initial-frame-alist
      (append initial-frame-alist
              '((width . 120)
                (height . 40))))

;; Enable mouse in terminal
(defun wm/enable-mouse ()
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)))
(add-hook 'after-init-hook #'wm/enable-mouse)

;;; Keybindings

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(keymap-global-set "C-c t" #'ef-themes-toggle)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c e" #'org-toggle-emphasis)
(keymap-global-set "M-#" #'dictionary-lookup-definition)
(keymap-global-set "M-z" #'zap-up-to-char) ; much more useful!
(keymap-global-set "M-Z" #'zap-to-char)    ; but keep this around

;; modify Tools menu a little
(define-key-after global-map [menu-bar tools ede] nil t)
(easy-menu-add-item global-map '(menu-bar tools)
                    ["Automatic Linting (Flymake)"
                     flymake-mode
                     :help "Linting (with LanguageTool in text modes)"
                     :style toggle
                     :selected (bound-and-true-p flymake-mode)]
                     "Spell Checking")
                     
;; left option stays meta, but right option goes back to option!
(setq mac-right-option-modifier "none")

;; use option keys as option, command key as meta
;; (setq mac-option-modifier 'none
;;       mac-command-modifier 'meta)

;;; Default modes & variables

(prefer-coding-system 'utf-8)           ; keep Windows from dorking out
(global-auto-revert-mode 1)             ; keep file in sync w/on-disk
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(customize-set-variable 'dired-dwim-target t) ; make dired smarter
(customize-set-variable 'dired-auto-revert-buffer t)
; scroll on eshell input at bottom
(customize-set-variable 'eshell-scroll-to-bottom-on-input 'this)
; pop to buffers
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
(customize-set-variable 'switch-to-buffer-obey-display-actions t)
(customize-set-variable 'ibuffer-old-time 24) ; delete old ibuffers
(delete-selection-mode)                 ; typing replaces selection
(customize-set-variable 'indent-tabs-mode nil) ; default to spaces
(customize-set-variable 'tab-width 4)   ; default to indent of 4
(customize-set-variable 'js-indent-level 2) ; except for JavaScript
(setq apropos-sort-by-scores t)         ; best match sorting
(setq flymake-no-changes-timeout 2.0)   ; 2 sec timeout
(setq ring-bell-function 'ignore)       ; no bell!
(tool-bar-mode -1)                      ; no tool bar, either
(setq sentence-end-double-space nil)    ; it's not 1983 anymore
(customize-set-variable 'cursor-type '(bar . 2)) ; be modern
; don't duplicate strings in kill ring
(customize-set-variable 'kill-do-not-save-duplicates t)
(global-so-long-mode 1)                 ; handle long lines better
(column-number-mode t)                  ; show column in mode line
(setq dictionary-server "dict.org")     ; dictionary lookup
(add-hook 'after-init-hook #'recentf-mode) ; store recent files
(save-place-mode 1)                     ; save your location in files
(savehist-mode 1)                       ; save the minibuffer history
(customize-set-variable 'bookmark-save-flag 1) ; save bookmark file on exit
(global-visual-line-mode t)             ; wrap text sanely

(setq auto-window-vscroll nil)          ; scrolling changes
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)
(customize-set-variable 'load-prefer-newer t) ; prefer newest ver of file
(add-hook 'after-save-hook              ; make scripts executable on save
	  #'executable-make-buffer-file-executable-if-script-p)
(setq-default                           ; display line #s in prog modes
 display-line-numbers-grow-only t
 display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; configure autosaving to get files saved elsewhere
(setf kill-buffer-delete-auto-save-files t)
(setq backup-directory-alist '(("." . "~/.emacs-saves/"))
      delete-old-versions t
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; keep the Ediff control panel in the same frame
(customize-set-variable 'ediff-window-setup-function
                        'ediff-setup-windows-plain)
(repeat-mode 1)                        ; enable repeat-mode
(add-hook 'js-mode-hook                ; JS/JSON indents
          (lambda () (setq js-indent-level 2)))

;; Set up proportional fonts for specific modes
(defun wm-text-face ()
  (face-remap-add-relative 'default :family "IBM Plex Sans" :height 160)
  (setq-local line-spacing 3))
(add-hook 'markdown-mode-hook 'wm-text-face)
(add-hook 'org-mode-hook 'wm-text-face)

;;; Configure external packages

;; set up package archives and ensure installation
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	    ("elpa" . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Set up EF-themes
(use-package ef-themes
  :custom
  (ef-themes-to-toggle '(ef-reverie ef-dream))
  (ef-themes-variable-pitch-ui t)
  (ef-themes-disable-other-themes nil))
(setq ef-themes-headings
      '((1 bold 1.5)
	    (2 1.2)
	    (t bold)))
(if (wm-is-dark-mode)
    (ef-themes-select 'ef-dream)
  (ef-themes-select 'ef-reverie))

;; Modus themes fallback settings
(setq-default
 modus-themes-to-toggle
 '(modus-operandi-tinted modus-vivendi-tinted)
 modus-themes-variable-pitch-ui t)

;; Vertico (vertical completion) and Marginalia
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

;; Embark: "Emacs Mini-Buffer Actions Rooted in Keymaps"
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)                ; overrides xref-find-definitions
   ("C-h B" . embark-bindings))
  ;; Optionally replace the key help with a completing-read interface
  :custom (prefix-help-command #'embark-prefix-help-command))

;; Consult: improve a whole bunch of commands with completing-read
(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ; repeat-complex-command
         ("C-x b" . consult-buffer)                ; switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ; switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ; bookmark-jump
         ("C-x p b" . consult-project-buffer)      ; project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ; yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ; goto-line
         ("M-g M-g" . consult-goto-line)           ; goto-line
         ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ; isearch-edit-string
         ("M-s e" . consult-isearch-history)       ; isearch-edit-string
         ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ; next-matching-history-element
         ("M-r" . consult-history))                ; previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; use consult's enhanced register preview
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Embark & Consult integration
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Corfu (completion in region function: in-buffer completion popup)
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2)
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

;; orderless (fuzzy completion matching)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-flex orderless-regexp))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Cape (completion at point extensions)
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; press C-c p ? for help
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  (completion-at-point-functions . cape-elisp-block))

;; web mode
(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode))
  :bind (:map web-mode-map
              ("C-c C-v" . browse-url-of-file))
  :custom
  (web-mode-engines-alist '(("django" . "\\.html\\'"))))

;; PHP mode
(use-package php-mode
  :custom
  (php-mode-coding-style 'psr2))

;; eglot
(use-package eglot
  :ensure nil
  :hook ((javascript-mode typescript-mode php-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(php-mode . ,(eglot-alternatives
                              '(("intelephense" "--stdio")))))
  :custom (eglot-autoshutdown t))

;; Markdown
(use-package markdown-mode
  :hook (markdown-mode . wm/markdown-tera-shortcode)
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-open-command "/usr/local/bin/mark"))

(use-package flymake-markdownlint
  :hook (markdown-mode . flymake-markdownlint-setup))

;; yaml
(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode))

;; typescript
(use-package typescript-mode)

;; eldoc-box (prettier documentation popups)
(use-package eldoc-box
  :hook (eglot-managed-mode-hook . eldoc-box-hover-mode)
  :bind (("C-h ." . eldoc-box-help-at-point)))

;; doom-modeline
(use-package nerd-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; editorconfig
(add-hook 'prog-mode-hook #'editorconfig-mode)

;; magit
(use-package magit)
(with-eval-after-load 'project
  (define-key project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-items '(( recents . 8)
                        (projects . 4)))
(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items))


(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Outline indent mode
(use-package outline-indent
  :hook
  (yaml-mode . outline-indent-minor-mode)
  (yaml-ts-mode . outline-indent-minor-mode))

;; rainbow delimiters!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;; Languagetool - this intentionally does NOT autostart
(use-package flymake-languagetool
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :custom
  (flymake-languagetool-server-jar nil)
  (flymake-languagetool-url "https://api.languagetool.org"))

;; Ultrascroll
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;;; transient menus

(defun wm/zola-preview ()
  (interactive)
  (async-shell-command "zola serve --drafts"))

(defun wm/zola-deploy ()
  (interactive)
  (async-shell-command
   (concat "cd " (vc-root-dir) "; make deploy")))

;; Zola tasks menu
(transient-define-prefix wm/zola ()
  ["Zola Tasks"
   ("p" "Preview" wm/zola-preview)
   ("b" "Build"
    (lambda () (interactive) (async-shell-command "zola build")))
   ("c" "Check"
    (lambda () (interactive) (async-shell-command "zola check")))
   ("D" "Deploy (make)" wm/zola-deploy)])
(keymap-global-set "C-c z" #'wm/zola)

;; TODO: investigate Treemacs
;; investigate treesitter modes (Crafted Emacs again?)
;; think about how to mimic tasks from Nova: compile-multi?
;; https://github.com/mohkale/compile-multi
