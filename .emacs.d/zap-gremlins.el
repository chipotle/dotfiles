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
