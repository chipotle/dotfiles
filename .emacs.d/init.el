;;; Emacs init file

;; use separate file for auto-generated customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Enable mouse in temrinal mouse
(defun wm/enable-mouse ()
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)))
(add-hook 'after-init-hook #'wm/enable-mouse)

;; Load zap gremlins
(when (file-exists-p "~/.emacs.d/zap-gremlins.el")
  (load "~/.emacs.d/zap-gremlins"))

;;; Keybindings

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(keymap-global-set "C-c t" #'ef-themes-toggle)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "M-#" #'dictionary-lookup-definition)
; modify Tools menu a little
(define-key-after global-map [menu-bar tools ede] nil t)
(easy-menu-add-item global-map '(menu-bar tools)
                    ["Automatic Linting (Flymake)"
                     flymake-mode
                     :help "Linting with LanguageTool"
                     :style toggle
                     :selected (bound-and-true-p flymake-mode)]
                     "Spell Checking")
                     
; left option stays meta, but right option goes back to option!
(setq mac-right-option-modifier "none")

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
(setq-default indent-tabs-mode nil)     ; default to spaces
(setq-default tab-width 4)              ; default to indent of 4
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
 display-line-numbers-width 2)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; configure autosaving to get files saved elsewhere
(setf kill-buffer-delete-auto-save-files t)
(setq backup-directory-alist '(("." . "~/.emacs-saves/"))
      delete-old-versions t
      version-control t)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)
;; set initial window size
(setq initial-frame-alist
      (append initial-frame-alist
              '((width . 120)
                (height . 40))))
;; Set buffer face for Info manuals to be more interesting
(setq buffer-face-mode-face '(:family "Triplicate T4p" :height 140))
(add-hook 'Info-mode-hook #'buffer-face-mode)

;; (add-to-list 'display-buffer-alist
;;              '("\\*Help\\*"
;;                (display-buffer-reuse-window display-buffer-pop-up-window)))

;; (add-to-list 'display-buffer-alist
;;              '("\\*Completions\\*"
;;                (display-buffer-reuse-window display-buffer-pop-up-window)
;;                (inhibit-same-window . t)
;;                (window-height . 10)))

;;; Configure external packages

;; set up package archives and ensure installation
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Force EF themes (also see custom.el!)
(use-package ef-themes)
(mapc #'disable-theme custom-enabled-themes)
(ef-themes-select 'ef-reverie)

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


;; ligatures
(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("<!---" "--->" "<!--" "<==>" "-->" "->>"
                                       "-<<" "/==" "||>" "||=" "|->" "&=" "&^="
                                       "===" "==>" "=>>" "=<<" "=/=" ">->"
                                       ">=>" ">>-" ">>=" "<--" "<->" "<-<"
                                       "<||" "<|>" "<=" "<==" "<=>" "<=<" "<<-"
                                       "<<=" ">&=" "<&-" "&>>" "&>" "->" "-<"
                                       "!=" "/=" "|=" "|>" "==" "=>" ">-" ">="
                                       "<-" "<|" "<~" "~=" "~>" "'''" "\"\"\""
                                       ":=" ":>" ":<" "?=" "**" "***" "*>"
                                       "*/" "-=" "*=" "+=" "%=" "#:" "#!" "#?"
                                       "#=" "/*" "/>" "///" "//" "/**" "$("
                                       ">&" "<&" "&&" "$>" ".." ".=" "+>" "=:="
                                       "=!=" ">:" ">>" "<(" ">>>" "<(" "<:"
                                       "<*" "<*>" "<$" "<$>" "<+" "<+>" "<>"
                                       "<<" "<<<" "</" "</>" "^=" "%%"))
  (ligature-set-ligatures 't '("##" "###" "####" "++" "+++" "--" "---" "..."
                               "::" ":::" "!!" "!!!" "?:" "??"))
  (global-ligature-mode t))

;; YASnippet
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

;; Polymode, for Tera shortcodes
(use-package poly-markdown)
(define-hostmode poly-tera-md-hostmode :mode 'poly-markdown-mode)
(define-innermode poly-tera-innermode
  :mode 'python-mode
  :head-matcher "{{"
  :tail-matcher "}}"
  :head-mode 'host
  :tail-mode 'host)
(define-polymode poly-tera-md-mode
  :hostmode 'poly-tera-md-hostmode
  :innermodes '(poly-tera-innermode))

;; Ultrascroll
(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; TODO: investigate Treemacs
;; investigate treesitter modes (Crafted Emacs again?)
;; think about how to mimic tasks from Nova: compile-multi?
;; https://github.com/mohkale/compile-multi
