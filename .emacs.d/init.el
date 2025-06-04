;; use separate file for auto-generated customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
	   (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Force EF themes (also see custom.el!)
(mapc #'disable-theme custom-enabled-themes)
(ef-themes-select 'ef-eagle)

;; === New commands ===

;; if we're running Terminal Emacs, enable the mouse
(defun wm/enable-mouse ()
  "Enable mouse for terminal."
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)))
(add-hook 'after-make-frame-functions #'wm/enable-mouse)
(add-hook 'after-init-hook #'wm/enable-mouse)

;; add BBEdit-ish zap gremlins
;; http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html
(defun zap-gremlins (&optional Begin End)
  "Remove accented letters in current line or selection.
e.g. café → cafe.

URL `http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html'
Version: 2018-11-12 2021-09-17 2022-05-02"
  (interactive)
  (let ((xcharMap
          [
           ["ß" "ss"]
           ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
           ["æ" "ae"]
           ["ç\\|č\\|ć" "c"]
           ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
           ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
           ["ñ\\|ň\\|ń" "n"]
           ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
           ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
           ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
           ["þ" "th"]
           ["ď\\|ð\\|đ" "d"]
           ["ĩ" "i"]
           ["ľ\\|ĺ\\|ł" "l"]
           ["ř\\|ŕ" "r"]
           ["š\\|ś" "s"]
           ["ť" "t"]
           ["ž\\|ź\\|ż" "z"]
           [" " " "]  ; thin space
           ["–" "--"] ; en dash
           ["—" "---"] ; em dash
           ["“" "\""]
           ["”" "\""]
           ["‘" "'"]
           ["’" "'"]
           ])
         (xp1 (if Begin Begin
                (if (region-active-p)
                    (region-beginning)
                  (line-beginning-position))))
         (xp2 (if End End
                (if (region-active-p)
                    (region-end)
                  (line-end-position)))))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region xp1 xp2)
        (mapc
         (lambda (xpair)
           (goto-char (point-min))
           (while (re-search-forward (elt xpair 0) (point-max) t)
             (replace-match (elt xpair 1))))
         xcharMap)))))

;; === Keybindings ===

(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(keymap-global-set "C-c t" #'ef-themes-toggle)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)

;; left option stays meta, but right option goes back to option!
(setq mac-right-option-modifier "none")

;; === Defaults Stuff (mostly boosted from Crafted Emacs) ===

(prefer-coding-system 'utf-8)

(global-auto-revert-mode 1)
(customize-set-variable 'global-auto-revert-non-file-buffers t)

(customize-set-variable 'dired-dwim-target t)
(customize-set-variable 'dired-auto-revert-buffer t)

(customize-set-variable 'eshell-scroll-to-bottom-on-input 'this)

(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
(customize-set-variable 'switch-to-buffer-obey-display-actions t)

(customize-set-variable 'ibuffer-old-time 24)

(delete-selection-mode)

(setq-default indent-tabs-mode nil)

(customize-set-variable 'kill-do-not-save-duplicates t)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(column-number-mode t)

(keymap-set global-map "M-#" #'dictionary-lookup-definition)
(setq dictionary-server "dict.org")

(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(add-hook 'after-init-hook #'recentf-mode)

(save-place-mode 1)

(savehist-mode 1)

(customize-set-variable 'bookmark-save-flag 1)

(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 1)
(customize-set-variable 'scroll-preserve-screen-position t)

(customize-set-variable 'Man-notify-method 'aggressive)

(customize-set-variable 'ediff-window-setup-function
                        'ediff-setup-windows-plain)

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

(customize-set-variable 'load-prefer-newer t)

(add-hook 'after-save-hook
	  #'executable-make-buffer-file-executable-if-script-p)

(setq-default
 display-line-numbers-grow-only t
 display-line-numbers-width 2)

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

;; === Package Stuff ===

;; set up package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; always ensure packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Force EF themes (also see custom.el!)
(use-package ef-themes)
(mapc #'disable-theme custom-enabled-themes)
(ef-themes-select 'ef-eagle)

;; setup vertico and friends
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

;; embark: "Emacs Mini-Buffer Actions Rooted in Keymaps"
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; consult: improve a whole bunch of commands with completing-read
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)
         ("M-I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

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
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Cape (completion at point extensions)
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; press C-c p ? for help
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; silence "pcomplete capf" (so says crafted emacs)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; web mode
(use-package web-mode
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode))
  :bind (:map web-mode-map
              ("C-c C-v" . browse-url-of-file)))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")) ;; for Tera templates
      )

;; php mode
(use-package php-mode
  :custom
  (php-mode-coding-style 'psr2))

;; eglot
(use-package eglot
  :hook ((javascript-mode typescript-mode php-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               `(php-mode . ,(eglot-alternatives
                              '(("intelephense" "--stdio"))))))
(setq eglot-autoshutdown t)

;; markdown
(use-package markdown-mode
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  :custom
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-open-command "/usr/local/bin/mark"))
(use-package flymake-markdownlint
  :init
  (add-hook 'markdown-mode-hook 'flymake-markdownlint-setup)
  (add-hook 'markdown-mode-hook #'flymake-mode))

;; yaml
(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode))

;; typescript
(use-package typescript-mode)

;; eldoc-box (prettier documentation popups)
(use-package eldoc-box
  :hook (eglot-managed-mode-hook #'eldoc-box-hover-mode)
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
(setq dashboard-items '(( recents . 10)
                        (projects . 5)))
(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items))


;; ligatures, maybe
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
                                       ":=" ":>" ":<" ";;" "?=" "**" "***" "*>"
                                       "*/" "-=" "*=" "+=" "%=" "#:" "#!" "#?"
                                       "#=" "/*" "/>" "///" "//" "/**" "$("
                                       ">&" "<&" "&&" "$>" ".." ".=" "+>" "=:="
                                       "=!=" ">:" ">>" "<(" ">>>" "<(" "<:"
                                       "<*" "<*>" "<$" "<$>" "<+" "<+>" "<>"
                                       "<<" "<<<" "</" "</>" "^=" "%%"))
  (ligature-set-ligatures 't '("##" "###" "####" "++" "+++" "--" "---" "..."
                               "::" ":::" "!!" "!!!" "?:" "??"))
  (global-ligature-mode t))

;; visual-fill-column -- currently disabled
;; (use-package visual-fill-column
;;   :init
;;   (add-hook 'text-mode-hook #'visual-fill-column-mode))
;; (setq-default fill-column 80)

;; YASnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Outline indent mode
(use-package outline-indent
  :custom
  (outline-indent-ellipsis " ▼ "))
(add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
(add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode)

;; rainbow delimiters!
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; expand region
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C-+" . er/contract-region))

;; TODO: investigate Treemacs
;; investigate treesitter modes (Crafted Emacs again?)
;; think about how to mimic tasks from Nova: compile-multi?
;; https://github.com/mohkale/compile-multi
