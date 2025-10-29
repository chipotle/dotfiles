;; load theme
;; TODO switch based on system theme?
(ef-themes-select 'ef-reverie)

;; Set up proportional font for specific modes
(defun wm-text-face ()
  (face-remap-add-relative 'default :family "IBM Plex Sans" :height 140)
  (setq-local line-spacing 3))
(add-hook 'markdown-mode-hook 'wm-text-face)
(add-hook 'org-mode-hook 'wm-text-face)
