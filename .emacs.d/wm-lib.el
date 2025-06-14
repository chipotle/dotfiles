;;; Library functions

;; Smarter keyboard quit
;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(defun er-keyboard-quit ()
  "Smarter version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))
(global-set-key [remap keyboard-quit] #'er-keyboard-quit)

;; Adapted from Xah Lee's zap gremlins
;; http://xahlee.info/emacs/emacs/emacs_zap_gremlins.html
(defun zap-gremlins (&optional Begin End)
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
           ["…" "..."]
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

;; test if Emacs is in dark mode on Mac
(defun wm-is-dark-mode ()
  (if (eq system-type 'darwin)
      (string-equal "true" (string-trim (shell-command-to-string "osascript -e 'tell application \"System Events\" to tell appearance preferences to return dark mode'")))
    nil))

;;; Add Org mode emphasis toggle
(defun org-toggle-emphasis ()
  "Toggle visibility of Org mode emphasis markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))
