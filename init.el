;; ---------------------------------------------------------- ;;
;; ------------- Load Path && General Settings -------------- ;;
;; ---------------------------------------------------------- ;;

(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/elpa/key-chord-0.5.20080915")
(add-to-list 'load-path "~/.emacs.d/elpa/js2-mode-20090814/")

(transient-mark-mode)          ; region between mark and point is highlighted only when 'active'
(global-linum-mode)            ; puts line numbers on left side of screen
(column-number-mode)           ; puts column numbers on the taskbar
(fset 'yes-or-no-p 'y-or-n-p)  ; changes 'yes' 'no' prompts to 'y' and 'n'(
(delete-selection-mode)        ; the active region can be replaced just be starting to type
(tool-bar-mode -1)             ; turn off the toolbar
(scroll-bar-mode -1)           ; turn off the scrollbar
(mouse-avoidance-mode 'exile)  ; if cursor nears mouse, make the cursor move away automatically
(global-auto-revert-mode 1)    ; auto refresh buffers
(setq global-auto-revert-non-file-buffers t)  ; Also auto refresh dired

;; remove if this becomes a problem
(add-hook 'before-exit-hook 'delete-trailing-whitespace)

;; default size of frame
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 118))

(setq scroll-step 1
      mouse-yank-at-point 't       ; mouse will paste at point, not where you click
      require-final-newline t)

;; hide or get rid of annoying backups and auto-saves
(setq backup-by-copying t             ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/saves"))   ; don't litter my fs tree
      ;; backup-inhibited t           ; uncomment this one?
      auto-save-default nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)              ; use versioned backups


;; UTF-8 - probably not necessary anymore, but just in case
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Only one window on startup
;; (add-hook 'emacs-startup-hook
;;           (lambda () (delete-other-windows)) t)

(add-hook 'emacs-startup-hook 'delete-other-windows t)

;; keep previous windows configurations
;; after typing C-x 1 to close other windows
;; use C-c <left> to go back to the previous window config
(winner-mode 1)

;; Toggle between split windows and a single window
(defun toggle-windows-split()
  "Switch back and forth between one window and whatever split of windows we might have in the frame. The idea is to maximize the current buffer, while being able to go back to the previous split of windows in the frame simply by calling this command again."
  (interactive)
  (if (not(window-minibuffer-p (selected-window)))
      (progn
        (if (< 1 (count-windows))
            (progn
              (window-configuration-to-register ?u)
              (delete-other-windows))
          (jump-to-register ?u))))
  (my-iswitchb-close))


;; ------------------------------- ;;
;; ----- Package.el settings ----- ;;
;; ------------------------------- ;;
(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; -------------------------- ;;
;; ----- Pomodoro Timer ----- ;;
;; -------------------------- ;;
;; installed via MELPA
;; instructions at: https://github.com/docgnome/pomodoro.el
(require 'pomodoro)

;; ---------------------------------------------------------- ;;
;; ----- Experimental => try out and document or delete ----- ;;
;; ---------------------------------------------------------- ;;
; I don't know what this does - research or remove!!
(autoload 'filladapt-mode "filladapt" nil t)
; Show keystrokes in progress - not sure what this means
(setq echo-keystrokes 0.1)


;; ----------------------------------------------------------- ;;
;; ----- start the emacs server - so can use emacsclient ----- ;;
;; ----------------------------------------------------------- ;;
(require 'server)
(unless (server-running-p)
  (server-start))


;; -------------------------------------- ;;
;; ---------- Helper Functions ---------- ;;
;; -------------------------------------- ;;

(defun delete-horizontal-space-forward() ; adapted from 'delete-horizontal-space'
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))


;; edot - Edit the .emacs file
(defun edot ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; ldot - Load the .emacs file (to apply changes)
(defun ldot ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


;; ---------------------------------------------------- ;;
;; ---------- Kill/Delete Whole Line Section ---------- ;;
;; ---------------------------------------------------- ;;

(defun delete-whole-line ()
  "Delete a line without retaining it in kill-ring"
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)                         ; kill the whole line
    (setq kill-ring (cdr kill-ring))))  ; and then erase it from the kill-ring

(defun copy-whole-line ()
  "Do a kill-line but copy rather than kill"
  (interactive)
  (let ((kill-whole-line t))
    (beginning-of-line)
    (kill-line)
    (yank)))

;; TODO: need to decide between these two ...
(global-set-key "\C-cd"    'delete-whole-line)
(global-set-key "\C-c\C-d" 'delete-whole-line)
(setq kill-whole-line t)       ; make the C-k kill line also remove the newline char(s)
                                        ;=> Note: this "kill-whole-line" does not refer to the function
                                        ;=> normally assigned to C-S-backspace, but rather to what C-k
                                        ;=> points to
(global-set-key "\C-c\C-k" 'kill-whole-line)  ; by default it is C-S-backspace
                                        ; this will delete the entire line no matter where the
                                        ; cursor is and it will put the line in the kill-ring
(global-set-key "\C-c\C-j" 'copy-whole-line)
;; ---------- END Kill/Delete Whole Line Section ---------- ;;



;; -------------------------------------------------------- ;;
;; ---------------- Tab and Spaces Settings --------------- ;;
;; -------------------------------------------------------- ;;
(setq-default indent-tabs-mode nil)     ; insert spaces instead of tab chars
(setq indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)         ; make programming-langs use an indent of 2 spaces
(setq c-basic-indent 2)                 ; this sets the basic indent for c-based major modes
(setq js-indent-level 2)                ; works for the basic javascript mode (not js2-mode)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30))

;; smart indenting and pairing for all
(electric-pair-mode t)
(electric-indent-mode t)
;;(electric-layout-mode t)


;; ------------------------------------------------------- ;;
;; ------------- Make emacs use the clipboard ------------ ;;
;; ------------------------------------------------------- ;;
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


;; ------------------------------------------------------- ;;
;; -------- Save point position between sessions --------- ;;
;; ------------------------------------------------------- ;;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/.places")


;; --------------------------------------------------- ;;
;; -------------- Adjust 'writing modes' ------------- ;;
;; --------------------------------------------------- ;;
;; Adapted from: https://github.com/maryrosecook/emacs/blob/master/init.el

;; narrower window, better line wrapping for prose
(defun write-words ()
  (interactive)
  (set-frame-width nil 90)
  (global-visual-line-mode t)
  (setq mode-line-format nil)
  (show-paren-mode nil))

;; widescreen, no line-wrap
(defun write-code ()
  (interactive)
  (set-frame-width nil 126)
  (global-visual-line-mode 0)
  (show-paren-mode)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              'mode-line-buffer-identification
              " "
              'mode-line-position
              '(vc-mode vc-mode)
              " "
              'mode-line-modes
              '(which-func-mode ("" which-func-format))
              '(global-mode-string (global-mode-string))
              )))


;; --------------------------------------------------------------- ;;
;; -------------- TODO: JUST ADDED - need to test !!! ------------- ;;
;; ---------------------------------------------------------------- ;;
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
;; from: http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; --------------------------------------------------------- ;;
;; ------------------ Color Theme Support ------------------ ;;
;; --------------------------------------------------------- ;;

(add-to-list 'load-path "/usr/share/emacs/plugins/emacs-goodies-el/color-theme.el")
(require 'color-theme)

;; (add-to-list 'load-path "~/.emacs.d/plugins/color-themes")
;; (load-file "~/.emacs.d/plugins/color-themes/zenburn-theme.el")
;; (require 'color-theme-zenburn)
;; (color-theme-zenburn)

;; uncomment this if you want color-theme to autoload
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-calm-forest)))

;; my own theme loading functions
;; these will open a new frame and put the theme in just that frame
(defun theme-char ()
  (interactive)
  (select-frame (make-frame))
  (set-variable 'color-theme-is-global nil)
  (color-theme-charcoal-black))

(defun theme-calm ()
  (interactive)
  (select-frame (make-frame))
  (set-variable 'color-theme-is-global nil)
  (color-theme-calm-forest))

(defun theme-law ()
  (interactive)
  (select-frame (make-frame))
  (set-variable 'color-theme-is-global nil)
  (color-theme-lawrence))

;; don't use - doesn't work => error reported to author 15-Jan-2012
(defun theme-zen ()
  (add-to-list 'load-path "~/.emacs.d/plugins/color-themes")
  (interactive)
  (select-frame (make-frame))
  (require 'color-theme-zenburn)
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  ;; custom theme from: https://github.com/bbatsov/zenburn-emacs
  (color-theme-charcoal-black)
  (color-theme-zenburn))

;; not interactive - intended to be called by interactive methods
;; thsub and thsubx
(defun theme-subdued (new-frame)
  (add-to-list 'load-path "~/.emacs.d/plugins/color-themes")
  ;; (interactive)
  (if new-frame
      (select-frame (make-frame)))
  (require 'color-theme-subdued)
  (color-theme-initialize)
  (set-variable 'color-theme-is-global nil)
  ;; custom theme from: http://jblevins.org/projects/emacs-color-themes/
  (color-theme-charcoal-black)
  (color-theme-subdued))

;; load the subdued color theme in the current frame
(defun thsub ()
  (interactive)
  (theme-subdued nil))

;; load the subdued color theme in (and only in) a new frame
(defun thsubx ()
  (interactive)
  (theme-subdued t))


;; --------------------------------------------------------- ;;
;; ---------- Programming/Markup Language Support ---------- ;;
;; --------------------------------------------------------- ;;

;; Steve Yegge's JavaScript major mode: http://code.google.com/p/js2-mode
;; (autoload 'js2-mode "js2" nil t)

;; use the regular js-mode by default - Yegge's is too opinionated
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Scala mode
(add-to-list 'load-path "~/.emacs.d/plugins/scala-mode")
(require 'scala-mode-auto)

;; yaml support
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Clojure mode
;; git clone git://github.com/technomancy/clojure-mode.git
(add-to-list 'load-path "~/.emacs.d/plugins/clojure-mode")
(require 'clojure-mode)
;;(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; Mark .ant files as xml-mode
(add-to-list 'auto-mode-alist '("\\.xml$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.ant$" . xml-mode))

;; Groovy mode
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
;; (add-hook 'groovy-mode-hook
;;           '(lambda ()
;;              (require 'groovy-electric)
;;              (groovy-electric-mode)))

;; Windows batch file mode, for those sad moments when it's required ...
;; Get from: http://ftp.gnu.org/old-gnu/emacs/windows/contrib/bat-mode.el
(require 'bat-mode)
(add-to-list 'auto-mode-alist '("\\.bat$" . bat-mode))

;; web-mode.el for mixed-mode HTML plus scripting lang and css
;; http://web-mode.org/
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset 2)
  (setq web-mode-css-offset 2)
  (setq web-mode-script-offset 2)
)
(add-hook 'web-mode-hook 'web-mode-hook)

    
;; Haml mode
(require 'haml-mode)
(add-hook 'haml-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;; PHP support, for those sad moments I have to use it ...
;; workaround from usual (require 'php-mode), since there is a bug in emacs23 around this
;; From: http://beyondteck.blogspot.com/2010/05/making-php-mode-on-emacs-23-work.html
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; CoffeeScript support
;; git clone git://github.com/defunkt/coffee-mode.git
(add-to-list 'load-path "~/.emacs.d/plugins/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Ruby support
(add-to-list 'auto-mode-alist '("[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; yasnippet
;; git clone git://github.com/capitaomorte/yasnippet.git
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas/global-mode 1)

;; cucumber mode and support
;; git clone git://github.com/michaelklishin/cucumber.el.git
(add-to-list 'load-path "~/.emacs.d/plugins/cucumber.el")
(require 'feature-mode)
(require 'cucumber-mode)
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))

;; mumamo (multiple major modes) support -> mostly good for html and php, so disabled by default
;; (load "~/.emacs.d/plugins/nxhtml/autostart.el")


;; html mode
(defun my-html-mode-hooks ()
  (auto-fill-mode -1))

(add-hook 'html-mode-hook 'my-html-mode-hooks)

;; --------------------------------------------------- ;;
;; --------------- Dirtree and Friends --------------- ;;
;; --------------------------------------------------- ;;

;; (require 'tree-mode)
;; (require 'windata)
;; (require 'dirtree)
;; ;(autoload 'dirtree "dirtree" "Add directory to tree view" t)
;; (global-set-key "\C-o" 'dirtree-show)



;; ----------------- ruby-debug support ------------------- ;;
;;(require 'rdebug)   ; ~TODO: need to figure this out - not working


;; Highlight FIXME:, TODO: and DEBUG: keywords in c-mode-common and ruby-mode
;;note that these keywords must be followed by a colon ...
(add-hook 'c-mode-common-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\):" 1 font-lock-warning-face t)))))

(add-hook 'ruby-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\|lambda\\)" 1 font-lock-warning-face t)))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'scala-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'groovy-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

(add-hook 'js-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil '(("\\<\\(\\$\\.\\|FIXME:\\|TODO:\\|DEBUG:\\)" 1 font-lock-warning-face t)))))

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords
;;              nil '(("\\<\\(lambda\\)" 1 font-lock-warning-face t)))))


;; (global-set-key "\C-c\C-o" 'ace-jump-mode)
;; (define-key global-map "\C-c\C-o" 'ace-jump-mode)
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two,
;; (global-set-key "\C-x\C-v" 'scroll-up)            ; to align with my Eclipse settings
;; (global-set-key [(control c) (space)] 'ace-jump-mode)


;; --------------------------------------------------- ;;
;; ----------------- paredit mode -------------------- ;;
;; --------------------------------------------------- ;;
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;; always enable paredit for clojure
(add-hook 'clojure-mode-hook    'enable-paredit-mode)
(add-hook 'lisp-mode-hook       'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(add-hook 'clojure-mode-hook    'hs-minor-mode)


;; paredit in slime
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; (eval-after-load "slime"
;;   '(progn (slime-setup '(slime-repl))
;; 	(defun paredit-mode-enable () (paredit-mode 1))
;; 	(add-hook 'slime-mode-hook 'paredit-mode-enable)
;; 	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
;; 	(setq slime-protocol-version 'ignore)))


;; ---------------------------------------------------------- ;;
;; ------------------------ SLIME --------------------------- ;;
;; ---------------------------------------------------------- ;;
;; (eval-after-load "slime"
;;   '(progn (slime-setup '(slime-repl))))

;; (add-to-list 'load-path "~/.emacs.d/plugins/slime")
;; (require 'slime)
;; (slime-setup)

;; ---------------------------------------------------- ;;
;; ------------------ ERC settings -------------------- ;;
;; ---------------------------------------------------- ;;

(setq erc-hide-list '("JOIN" "PART" "QUIT"))


;; ---------------------------------------------------- ;;
;; --------------- org-mode settings ------------------ ;;
;; ---------------------------------------------------- ;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-folded nil)
(setq word-wrap t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ck" 'org-agenda)  ; note this is C-ca in the org-mode doc
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "QUESTION(q)" "DONE(d)" "REMINDER" "DEFERRED" "LEFTOFF" "DATEDONE")))
(setq org-todo-keyword-faces
           '(("DONE"     . (:foreground "#94ff94"))
             ("QUESTION" . (:foreground "#ff65ff"))
             ("REMINDER" . (:foreground "#c8c1f1"))
             ("DEFERRED" . (:foreground "#ffc358"))
             ("LEFTOFF"  . (:foreground "#f333d8"))
             ))
(setq org-log-done t)
(org-remember-insinuate)  ; use remember.el in org-mode
(setq org-default-notes-file (concat org-directory "/remember.org"))
(define-key global-map "\C-cr" 'org-remember)  ; note this is C-cc in the org-mode doc

(defun no-electric-indent-mode-hook ()
  (electric-indent-mode -1))

(add-hook 'org-mode-hook 'no-electric-indent-mode-hook)


;; ---------------------------------------------------------- ;;
;; --------------- Printing and PDF support ----------------- ;;
;; ---------------------------------------------------------- ;;
;; Use the Printing Package
(require 'printing)
(pr-update-menus)

;; set the PDF printer as default
;;(setq printer-name "PDF_file_generator")
;;(setq printer-name t)
(setq ps-printer-name "PDF_file_generator")
(setq ps-printer-name t)

(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "~/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf"))
  )


;; --------------------------------------------------------- ;;
;; ------------- Newline conversion functions -------------- ;;
;; --------------------------------------------------------- ;;
;; Convert CR-LF to LF
;;  => TODO: not sure I need these, see note near end of this file
(defun dos-to-unix (file-path)
  (interactive "fFile name: ")
  (save-excursion
    (let (dos-buffer)
      (setq dos-buffer (find-file file-path))
      (set-buffer-file-coding-system 'unix)
      (save-buffer)
      (kill-buffer dos-buffer))))

;; Convert LF to CR-LF
(defun unix-to-dos (file-path)
  (interactive "fFile name: ")
  (save-excursion
    (let (unix-buffer)
      (setq unix-buffer (find-file file-path))
      (set-buffer-file-coding-system 'dos)
      (save-buffer)
      (kill-buffer unix-buffer))))


;; ----------------------------------------------------- ;;
;; ---------------- Shell support / use ---------------- ;;
;; ----------------------------------------------------- ;;
;; load my aliases
(add-to-list 'auto-mode-alist '("\\.bash_aliases$" . sh-mode))

;; Make colours in Emacs' shell look normal
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Don't auto-truncate lines in shell mode
(add-hook 'shell-mode-hook '(lambda () (toggle-truncate-lines 1)))

;; start with shell closed - find I don't use it much
;; (shell)


;; --------------------------------------------- ;;
;; ----------------- Ido mode ------------------ ;;
;; --------------------------------------------- ;;
;; => ido:  http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; enable ido mode
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; --------------------------------------------- ;;
;; ------------ Reminders and Notes ------------ ;;
;; --------------------------------------------- ;;
;; Replace spaces with tabs using M-x tabify.
;; Replace tabs with spaces using M-x untabify.
;; change 'file coding system' with 'set-buffer-file-coding-system RET unix or dos
;; or C-x RET f unix or dos
;; Use C-x <left> and C-x <right> to rapidly switch between buffers


;; --------------------------------------------------- ;;
;; ----------------- ace-jump-mode ------------------- ;;
;; --------------------------------------------------- ;;
(require 'ace-jump-mode)


;; ---------------------------------------- ;;
;; ------------- Load Files --------------- ;;
;; ---------------------------------------- ;;
(load-file "~/.emacs.d/macros.el")        ; load my macros
(load-file "~/.emacs.d/key-bindings.el")  ; load my key-bindings


;; ---------------------------------------------------- ;;
;; ---------- Colors, Faces and Auto-settings --------- ;;
;; ---------------------------------------------------- ;;
(set-background-color "black")
(set-foreground-color "yellow")
(set-cursor-color "white")

;;------ Automatic settings: Do Not Edit These ------- ;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(backup-inhibited t t)
 '(column-number-mode t)
 '(custom-safe-themes (quote ("71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(echo-keystrokes 0.01)
 '(fill-column 78)
 '(frame-title-format (quote ("%f - " user-real-login-name "@" system-name)) t)
 '(ido-auto-merge-work-directories-length nil)
 '(ido-create-new-buffer (quote always))
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-everywhere t)
 '(ido-ignore-extensions t)
 '(ido-max-prospects 8)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format " %d ")
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(puppet-indent-level tab-width)
 '(recentf-max-saved-items 75)
 '(require-final-newline t)
 '(ruby-indent-level tab-width)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)))
 '(tab-width 4)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "yellow" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "SkyBlue" :background "black")))))

;;'(show-paren-mode t nil (paren))
;;note: Cornflower Blue is also good font color

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
