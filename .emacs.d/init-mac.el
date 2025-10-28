;; path magic: match shell on macOS
(defun set-exec-path-from-shell-PATH ()
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
		  "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; Swap option back to option, left command to meta, right command
;; to command
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)
(setq mac-right-command-modifier 'super)

;; Set up proportional font for specific modes
(defun wm-text-face ()
  (face-remap-add-relative 'default :family "IBM Plex Sans" :height 160)
  (setq-local line-spacing 3))
(add-hook 'markdown-mode-hook 'wm-text-face)
(add-hook 'org-mode-hook 'wm-text-face)

;; hook into system appearance change
(add-hook 'ns-system-appearance-change-functions #'wm/theme-hook)

;; add Swift to Eglot
(with-eval-after-load "eglot"
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))))
