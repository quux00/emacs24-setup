;; From: http://java.dzone.com/articles/emacs-lisp-toggle-between?utm_source=dlvr.it&utm_medium=twitter&utm_campaign=Feed%3A+javalobby%2Ffrontpage+%28Javalobby+%2F+Java+Zone%29
(defun char-at-point ()
  (interactive)
  (buffer-substring-no-properties (point) (+ 1 (point))))

(defun clj-string-name (s)
  (substring s 1 -1))

(defun clj-keyword-name (s)
  (substring s 1))

(defun delete-and-extract-sexp ()
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun toggle-clj-keyword-string ()
  (interactive)
  (save-excursion
    (if (equal 1 (point))
        (message "beginning of file reached, this was probably a mistake.")
      (cond ((equal "\"" (char-at-point))
             (insert ":" (clj-string-name (delete-and-extract-sexp))))
            ((equal ":" (char-at-point))
             (insert "\"" (clj-keyword-name (delete-and-extract-sexp)) "\""))
            (t (progn
                 (backward-char)
                 (toggle-keyword-string)))))))

(global-set-key (kbd "C-:") 'toggle-clj-keyword-string)
