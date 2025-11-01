;;; -*- lexical-binding: t -*-
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :family "MonoLisa"))))
 '(ef-themes-fixed-pitch ((t (:family "IBM Plex Mono" :height 0.9))) t)
 '(eldoc-box-body ((t (:family "SF Pro"))))
 '(imenu-list-entry-face ((t (:inherit variable-pitch))))
 '(variable-pitch ((t (:height 120 :family "SF Pro")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-support-shift-select t)
 '(package-selected-packages
   '(cape corfu dart-mode dashboard doom-modeline ef-themes eldoc-box
          embark-consult expand-region flutter flymake-markdownlint
          fountain-mode imenu-list json-mode ligature magit marginalia
          markdown-mode orderless ox-pandoc php-mode
          rainbow-delimiters swift-mode typescript-mode vertico
          web-mode yaml-mode yasnippet)))
