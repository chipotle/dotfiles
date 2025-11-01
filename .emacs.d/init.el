;; Set variable based on OS
(cond
 ((eq system-type 'darwin) (setq os-type "mac"))
 ((eq system-type 'windows-nt) (setq os-type "win"))
 ((eq system-type 'gnu/linux) (setq os-type "linux")))

;; use separate file for auto-generated customizations
(setq custom-file
      (expand-file-name
       (concat "custom-" os-type ".el") user-emacs-directory))
(when (and custom-file
	       (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; load OS-specific functions
(load (concat "~/.emacs.d/init-" os-type))

;; Load library functions
(load "~/.emacs.d/wm-lib")

;; Enable mouse in terminal
(xterm-mouse-mode 1)

;; start the server
(server-start)

;;; Keybindings

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(keymap-global-set "C-c t" #'ef-themes-toggle)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c e" #'org-toggle-emphasis)
(keymap-global-set "M-#" #'dictionary-lookup-definition)
(keymap-global-set "M-z" #'zap-up-to-char) ; much more useful!
(keymap-global-set "M-Z" #'zap-to-char)    ; but keep this around

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
; disable inlayHintProvider
(customize-set-variable 'eglot-ignored-server-capabilities '(:inlayHintProvider))
; org stuff
(customize-set-variable 'org-agenda-files nil)
(customize-set-variable 'org-babel-load-languages '((emacs-lisp . t) (ruby . t)))
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
(customize-set-variable 'load-prefer-newer t) ; prefer newest ver of file
(add-hook 'after-save-hook              ; make scripts executable on save
	  #'executable-make-buffer-file-executable-if-script-p)
(setq-default                           ; display line #s in prog modes
 display-line-numbers-grow-only t
 display-line-numbers-width 3)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(repeat-mode 1)                        ; enable repeat-mode
(add-hook 'js-mode-hook                ; JS/JSON indents
          (lambda () (setq js-indent-level 2)))

;; fiddle around with scrolling
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 2)
(customize-set-variable 'scroll-step 1)
(customize-set-variable 'scroll-preserve-screen-position t)

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
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
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
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         )
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
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . hippie-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

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

;; JSON mode
(use-package json-mode)

;; eglot
(use-package eglot
  :ensure nil
  :hook ((javascript-mode typescript-mode php-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(php-mode . ,(eglot-alternatives
                              '(("intelephense" "--stdio")))))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(dart-mode . ("dart" "language-server"
                              "--protocol" "lsp")))
  :custom (eglot-autoshutdown t))
(setq-default eglot-workspace-configuration
              '(:harper-ls
                (:linters
                 (:AvoidCurses :json-false
                               :BoringWords t
                               :Dashes :json-false))))

(setq eldoc-echo-area-use-multiline-p nil)

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
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-enable-word-count t))

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

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;; Dart configuration
(use-package dart-mode
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

;; Fountain
(use-package fountain-mode)

;; ox-pandoc
(use-package ox-pandoc)

;; org mode configuration
(setq org-export-with-smart-quotes t)
;; Use UTF-8 smart quotes instead of HTML entities in HTML and Markdown export.
(defun dcb/make-utf8-encoding-org-export (args)
  "Override the ARGS for smartquotes to make :html encoded entities use utf-8 instead."
  (if (eq (nth 1 args) :html) ;; if the encoding is html (which both md and gfm derive from)
      (progn (setcar (nthcdr 1 args) :utf-8)
             args)
    args))

(setq org-export-with-smart-quotes t) ;; Can be overridden with ':nil in the OPTIONS
(advice-add 'org-export-activate-smart-quotes :filter-args 'dcb/make-utf8-encoding-org-export)

;; Org novelist
;; (use-package org-novelist
;;   :vc (:url "https://github.com/sympodius/org-novelist"
;;             :branch "main"))

;; imenu-list
(use-package imenu-list
  :custom (imenu-list-position 'left))

;; ligature support, for better or worse?
(use-package ligature
  :config
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; transient menus

(defun wm/zola-deploy ()
  (interactive)
  (shell-command
   (concat "cd "
           (project-root (project-current t)) "; make deploy")))

(defun wm/zola-schedule ()
  (interactive)
  (shell-command
   (concat "cd " (project-root (project-current t))
           "; at "
           (read-string "Schedule deploy at: ")
           " < schedule.sh")))

(transient-define-prefix wm/zola ()
  ["Zola Tasks"
   ("p" "Preview"
    (lambda ()
      (interactive)
      (async-shell-command "zola serve --drafts" "Zola: Preview Console")))
   ("b" "Build"
    (lambda ()
      (interactive)
      (async-shell-command "zola build" "Zola: Build Output")))
   ("c" "Check"
    (lambda ()
      (interactive)
      (async-shell-command "zola check" "Zola: Check Site")))
   ("D" "Deploy (make)" wm/zola-deploy)
   ("S" "Schedule deployment" wm/zola-schedule)])

(keymap-global-set "C-c z" #'wm/zola)
