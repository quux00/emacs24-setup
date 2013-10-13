(require 'json)

(defun json-reformat:indent (level tabsz)
  (make-string (* level tabsz) ? ))

(defun json-reformat:p-of-number (val)
  (number-to-string val))

(defun json-reformat:newline (tabsz)
    (if (zerop tabsz) "" "\n"))

;; (defun json-reformat:p-of-list (val level)
;;   (concat "{\n" (json:list-to-string val (1+ level)) (json-reformat:indent level) "}"))

(defun json-reformat:p-of-list (val level tabsz)
  (concat "{" (json-reformat:newline tabsz) (json-reformat:list-to-string (reverse val) (1+ level) tabsz) (json-reformat:indent level tabsz) "}"))

;; (defun json-reformat:p-of-vector (val level)
;;   (if (= (length val) 0) "[]"
;;     (concat "[\n"
;;             (mapconcat
;;              'identity
;;              (loop for v across val
;;                    collect (concat
;;                             (json-reformat:indent (1+ level))
;;                             (json-reformat:print-value v (1+ level))
;;                             ))
;;              (concat ",\n"))
;;             "\n" (json-reformat:indent level) "]"
;;             )))

(defun json-reformat:p-of-vector (val level tabsz)
  (if (= (length val) 0) "[]"
    (concat "["
            (json-reformat:newline tabsz)
            (mapconcat
               'identity
               (mapcar (function (lambda (v)
                   (concat
                            (json-reformat:indent (1+ level) tabsz)
                            (json-reformat:print-value v (1+ level) tabsz)
                            ))) val)
               (concat "," (json-reformat:newline tabsz))
               )
            (json-reformat:newline tabsz) (json-reformat:indent level tabsz) "]"
            )))

(defun json-reformat:p-of-symbol (val)
  (cond ((equal 't val) "true")
        ((equal json-false val) "false")
        (t (symbol-name val))))

(defun json-reformat:print-value (val level tabsz)
  (cond ((consp val) (json-reformat:p-of-list val level tabsz))
        ((numberp val) (json-reformat:p-of-number val))
        ((vectorp val) (json-reformat:p-of-vector val level tabsz))
        ((null val) "\"\"")
        ((symbolp val) (json-reformat:p-of-symbol val))
        (t (concat "\"" val "\""))))


;; (defun json:list-to-string (root level)
;;   (let (key val str)
;;     (while root
;;       (setq key (car root)
;;             val (cadr root)
;;             root (cddr root))
;;       (setq str
;;             (concat str (json-reformat:indent level)
;;                     "\"" key "\""
;;                     ": "
;;                     (json-reformat:print-value val level)
;;                     (when root ",")
;;                     "\n"
;;                     )))
;;     str))

(defun json-reformat:list-to-string (root level tabsz)
  (let (key val str)
    (while root
      (setq key (caar root)
            val (cdar root)
            root (cdr root))
      (setq str
            (concat str (json-reformat:indent level tabsz)
                    "\"" key "\""
                    (if (zerop tabsz) ":" ": ")
                    (json-reformat:print-value val level tabsz)
                    (when root ",")
                    (json-reformat:newline tabsz)
                    )))
    str))

;; (defun json-reformat-region (begin end)
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region begin end)
;;       (goto-char (point-min))
;;       (let* ((json-key-type 'string)
;;              (json-object-type 'plist)
;;              (before (buffer-substring (point-min) (point-max)))
;;              (json-tree (json-read-from-string before))
;;              after)
;;         (setq after (json-reformat:p-of-list json-tree 0))
;;         (delete-region (point-min) (point-max))
;;         (insert after)))))

(defun json-expand-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let* ((json-key-type 'string)
             (tabsz 4)
             (before (buffer-substring (point-min) (point-max)))
             (json-tree (json-read-from-string before))
             after)
        (setq after (json-reformat:p-of-list json-tree 0 tabsz))
        (delete-region (point-min) (point-max))
        (insert after)))))


(defun json-collapse-region (begin end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let* ((json-key-type 'string)
             (tabsz 0)
             (before (buffer-substring (point-min) (point-max)))
             (json-tree (json-read-from-string before))
             after)
        (setq after (json-reformat:p-of-list json-tree 0 tabsz))
        (delete-region (point-min) (point-max))
        (insert after)))))

(provide 'json-reformat-region)
