;; ------------------------------------------------------ ;;
;; -------------- My preferred key bindings ------------- ;;
;; ------------------------------------------------------ ;;
(global-set-key [(meta g)]          'goto-line)
(global-set-key [(control shift l)] 'goto-line)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key "\C-c\C-u" 'uncomment-region)
;(global-set-key "\M-;"     'comment-or-uncomment-region)
(global-set-key "\C-c;"    'comment-indent)
(global-set-key "\C-x\C-d" 'electric-buffer-list) ; one of my favorite things in emacs ...
(global-set-key "\C-c\C-k" 'kill-whole-line)      ; delete whole line (including newline) from anywhere
(global-set-key "\C-x\C-k" 'kill-region)          ; normally mapped to \C-w
(global-set-key "\C-w"     'backward-kill-word)   ; make emacs more like bash shell
(global-set-key "\C-z"     'undo)                 ; too ingrained from years of Windoze ...
(global-set-key "\C-x\C-z" 'shell)
(global-set-key [(control \;)] 'dabbrev-expand)   ; I find the M-/ binding awkward
(global-set-key (kbd "RET") 'newline-and-indent)  ; indent previous line after
(global-set-key (read-kbd-macro "M-s") 'query-replace)
(global-set-key (read-kbd-macro "C-x w") 'write-words)
(global-set-key (read-kbd-macro "C-x c") 'write-code)
(global-set-key "\C-x\C-v" 'scroll-up)            ; to align with my Eclipse settings
(global-set-key "\C-c o" 'occur)                  ; occur takes regex to show all occurances in file

;; keybindings for moving the cursor between windows/frames
(global-set-key (kbd "C-S-<up>") 'windmove-up)
(global-set-key (kbd "C-S-<down>") 'windmove-down)
(global-set-key (kbd "C-S-<right>") 'windmove-right)
(global-set-key (kbd "C-S-<left>") 'windmove-left)

(require 'expand-region)  ; https://github.com/magnars/expand-region.el
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'mark-multiple)  ; https://github.com/magnars/mark-multiple.el
(require 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two,
                                                    ; but takes an argument (negative is previous)
(global-set-key [(control meta \;)]
                'delete-horizontal-space-forward)   ; pers function


(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(define-key global-map (kbd "C-c C-v") 'wrap-region-mode)

;; override the default keybindings in paredit
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<M-right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "<M-left>")  'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<C-left>")  nil)))

(define-key global-map (kbd "C-|") 'toggle-windows-split)


;; Use control-arrow keys for window resizing
;; <<MP note: turned off since these don't seem to work>>
;; (global-set-key (kbd "<C-f11>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "<C-f12>") 'shrink-window-horizontally)


;; ----------------------------------------------------- ;;
;; ------------- Mine goes to 11: chords! -------------- ;;
;; ----------------------------------------------------- ;;
(require 'key-chord)
(key-chord-mode 1)

;; General purpose chords
(key-chord-define-global "jk"    'dabbrev-expand)
(key-chord-define-global "90"    "()")
(key-chord-define-global ".,"    "()\C-b")   ;; probably don't need with electric-pair-mode now?
(key-chord-define-global ",,"    'indent-for-comment)
(key-chord-define-global "a\;"   "@")
;;(key-chord-define-global "s\;"   "$")
(key-chord-define-global "df"    "\C-b")
(key-chord-define-global "c\;"   "console.log();\C-b\C-b")
(key-chord-define-global "fj"    "\C-f")     ;; ahead one space
(key-chord-define-global "<>"    "<>\C-b")

;; chords for Ruby coding
(key-chord-define-global "hh"    " => ")     ;; hash rocket

;; chords for Clojure/Ruby coding
(key-chord-define-global "d\;"   "#{}\C-b")  ;; \C-b is "backspace">?<>

;; chords for JavaScript coding
(key-chord-define-global "jl"    'jslambda)  ;; jslambda is a macro I defined
(key-chord-define-global "f\;"   'jsfunc)         
(key-chord-define-global "jq"    "$()\C-b")  ;; for jquery

