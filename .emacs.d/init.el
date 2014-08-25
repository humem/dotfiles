;; Local Emacs-Lisp Repository ;;
(setq load-path (cons "~/.emacs.d/lisp" load-path))
;(setq load-path (cons "~/.emacs.d/lisp/ess/lisp" load-path))
;(setq load-path (cons "~/.emacs.d/lisp/sicstus" load-path))

(load "extra-autoloads" t)

;; Language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
;(set-keyboard-coding-system 'utf-8)
;(set-clipboard-coding-system 'utf-8)
;(set-terminal-coding-system 'utf-8)
;; (set-file-name-coding-system 'utf-8m) ; already set
(prefer-coding-system 'utf-8-unix)

;; Settings Japanese fonts for CocoaEmacs (version 23)
(when (>= emacs-major-version 23)
  (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     '("osaka" . "iso10646-1"))
;     '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
    (setq face-font-rescale-alist
	  '(("^-apple-hiragino.*" . 1.2)
	    (".*osaka-bold.*" . 1.0)
  	    (".*osaka-medium.*" . 1.0)
  	    (".*courier-bold-.*-mac-roman" . 1.0)
  	    (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
  	    (".*monaco-bold-.*-mac-roman" . 0.9)
  	    ("-cdac$" . 1.3)))

    ;; Encoding settings.
;    (require 'ucs-normalize)
;    (setq file-name-coding-system 'utf-8-hfs))
;    (prefer-coding-system 'utf-8-hfs))

;    (setq my-font
;	  "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-osaka")
;    (setq mac-allow-anti-aliasing t)
;    (set-default-font my-font)
;    (add-to-list 'default-frame-alist `(font . ,my-font))
;    (set-fontset-font
;     (frame-parameter nil 'font)
;     'japanese-jisx0208
;     '("osaka" . "iso10646-1"))
;    (setq face-font-rescale-alist
;	  '(("^-apple-hiragino.*" . 1.2)
;	    (".*osaka-bold.*" . 1.0)
;  	    (".*osaka-medium.*" . 1.0)
;  	    (".*courier-bold-.*-mac-roman" . 1.0)
;  	    (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
;  	    (".*monaco-bold-.*-mac-roman" . 0.9)
;  	    ("-cdac$" . 1.3))))


;; Auto chmod+x for scripts
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;Dired
;(setq dired-load-hook '(lambda () (load "dired-x"))) 
(setq dired-guess-shell-alist-user
      '(("\\.app" "open")
        ("\\.csv" "open")
        ("\\.dmg" "open")
        ("\\.doc" "open")
        ("\\.jpg" "open")
        ("\\.htm" "open")
        ("\\.html" "open")
        ("\\.pdf" "open")
        ("\\.pkg" "open")
        ("\\.ppg" "open")
        ("\\.ppt" "open")
        ("\\.rtf" "open")
        ("\\.tif" "open")
        ("\\.tiff" "open")
        ("\\.xdw" "open")
        ("\\.xls" "open")))

;; ESS
;(require 'ess-site)

;; Ruby mode
;(setq ruby-insert-encoding-magic-comment nil)
; http://d.hatena.ne.jp/akm/20080605#1212644489
;;(require 'ruby-mode)
;;(defun ruby-mode-set-encoding () ())
(add-to-list 'auto-mode-alist '("\\.rake"     . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile" . ruby-mode))

;; Ruby debugger (Rubydb)
(autoload 'rubydb "rubydb3x"
  "Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
(global-set-key "\C-cd" 'rubydb)

(add-to-list 'auto-mode-alist '("\\.xhtml" . html-mode))

;; Sdic-mode for lookuping up dictionaries
(autoload 'sdic-describe-word "sdic"
  "look up English or Japanese words" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
;(autoload 'sdic-describe-word-at-point "sdic"
;  "look up a word at the cursor" t nil)
;(global-set-key "\C-cW" 'sdic-describe-word-at-point)

;; launch Dictionary.app
;; http://d.hatena.ne.jp/tunefs/20130212/p1
(global-set-key
 "\C-cW"
 (lambda ()
   (interactive)
   (let ((url (concat "dict://" (read-from-minibuffer "" (current-word)))))
     (browse-url url))))

;; Subversion (svn)
(require 'psvn)
(add-hook 'dired-mode-hook
          '(lambda ()
             (require 'dired-x)
             ;;(define-key dired-mode-map "V" 'cvs-examine)
             (define-key dired-mode-map "V" 'svn-status)
             (turn-on-font-lock)
             ))
(setq svn-status-hide-unmodified t)
(setq process-coding-system-alist
      (cons '("svn" . utf-8) process-coding-system-alist))

;; Tcl-mode for MacPort portfile.
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))

;; Auto-updating time stamp
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "Last Modified: ")
(setq time-stamp-format "%02m/%02d/%02y")
(setq time-stamp-end " \\|$")

;; Velocity template engine
;(autoload 'vtl-mode "vtl" "fontify velocity template language code" t)
;(add-to-list 'auto-mode-alist '("\\.vm"    . vtl-mode))
(add-to-list 'auto-mode-alist '("\\.vm"    . html-mode))

;; Word count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)


;; Syntax highlighting
;(if window-system
;    (progn
;      (global-font-lock-mode t)   ; Always turn on syntax highlighting
;      (require 'font-lock) ; Without this line, emacs throws an error on OS X.
;      (set-face-foreground 'font-lock-comment-face "MediumSeaGreen")
;      (set-face-foreground 'font-lock-string-face  "purple")
;;      (set-face-foreground 'font-lock-keyword-face "blue")
;;      (set-face-foreground 'font-lock-function-name-face "blue")
;      (set-face-bold-p 'font-lock-function-name-face t)
;;      (set-face-foreground 'font-lock-variable-name-face "black")
;;      (set-face-foreground 'font-lock-type-face "LightSeaGreen")
;;      (set-face-foreground 'font-lock-builtin-face "purple")
;;      (set-face-foreground 'font-lock-constant-face "black")
;;      (set-face-foreground 'font-lock-warning-face "blue")
;;      (set-face-bold-p 'font-lock-warning-face nil)
;      ))

;; Personal Preferences
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(setq migemo-isearch-enable-p nil)
(display-time)
(setq dabbrev-case-fold-search nil)
(setq dired-dwim-target t)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'what-line)
(custom-set-variables '(indent-tabs-mode nil))
(setq visible-bell t)
(tool-bar-mode 0)
(setq truncate-partial-width-windows nil)

;; Disable to color the selected region
(setq transient-mark-mode nil)

;; Fullscreen
(global-set-key "\C-c\C-f" 'ns-toggle-fullscreen)

;; pdf-preview-buffer-with-faces
;(require 'pdf-preview)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Key settings

;; Switch the Command-Key and the Option-Key
;(setq ns-command-modifier   'meta)
;(setq ns-alternate-modifier 'super)

;; Linum mode.
(global-set-key "\M-n" 'linum-mode)

;; Magnify and demagnify texts.
(global-set-key [(control ?+)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(control ?=)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(control ?-)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))

;; Let SPC complete path names
(when (boundp 'minibuffer-local-filename-completion-map)
  (setq minibuffer-local-filename-completion-map minibuffer-local-completion-map
	minibuffer-local-must-match-filename-map minibuffer-local-must-match-map
	))

;; Move to the virtually same column by next or previous line commands
(global-set-key "\C-p" 'previous-window-line)
(global-set-key "\C-n" 'next-window-line)
(global-set-key [up] 'previous-window-line)
(global-set-key [down] 'next-window-line)
(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
)


(when (>= emacs-major-version 24)
  (require 'cl)

  ;; package managenment; http://sakito.jp/emacs/emacs24.html
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  ;; http://qiita.com/hottestseason/items/1e8a46ad1ebcf7d0e11c
  (defvar installing-package-list
    '(
      auto-complete
      evil
      exec-path-from-shell
      helm
      magit
      powerline
      popwin
      ))
  (let ((not-installed (loop for x in installing-package-list
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))

  ;; evil
  (evil-mode 1)

  ;; exec-path-from-shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  ;; helm (anything); https://github.com/emacs-helm/helm
  (global-set-key (kbd "C-c h") 'helm-mini)
;  (helm-mode 1)
  ;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
  (when (require 'helm-config nil t)
    (helm-mode 1)
    ;; Key bindings
    (define-key global-map (kbd "M-x")     'helm-M-x)
    (define-key global-map (kbd "C-x C-f") 'helm-find-files)
    (define-key global-map (kbd "C-x C-r") 'helm-recentf)
    (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
    (define-key global-map (kbd "C-c i")   'helm-imenu)
    (define-key global-map (kbd "C-x b")   'helm-buffers-list)
    ;; Delete with C-h
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    ;; Emulate `kill-line' in helm minibuffer
    (setq helm-delete-minibuffer-contents-from-point t)
    (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
      "Emulate `kill-line' in helm minibuffer"
      (kill-new (buffer-substring (point) (field-end)))))

  ;; markdown-mode
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  (setq markdown-command "rdiscount")

  ;; auto-complete; http://cx4a.org/software/auto-complete/manual.ja.html
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-use-menu-map t) ; C-n, C-p
;  ;; yasnippet; 
;  (yas-global-mode 1)

;  ;; ruby-mode, ruby-electric
;  ;; flymake-ruby
;  (require 'ruby-mode)
;  (define-key ruby-mode-map "\C-cd" 'flymake-display-err-menu-for-current-line)


  ;; http://d.hatena.ne.jp/eiel/20101106
  (defun ruby-mode-hook-init ()
    "encodingを自動挿入しないようにする"
    (remove-hook 'before-save-hook 'ruby-mode-set-encoding))
  (add-hook 'ruby-mode-hook 'ruby-mode-init)

  (defun my-ruby-mode-set-encoding ()
    "set-encoding ruby-mode"
    (interactive)
    (ruby-mode-set-encoding))
  (require 'ruby-mode)
  (define-key ruby-mode-map "\C-ce" 'my-ruby-mode-set-encoding)


;  ;; expand-region
;  (global-set-key (kbd "C-;") 'er/expand-region)

  ;; web-mode
  (add-to-list 'auto-mode-alist '("\\.?html$" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-engines-alist
        '(("\\.xhtml$" . "smarty")))

  ;; popwin; http://sleepboy-zzz.blogspot.jp/2012/09/anythinghelm.html
;  (require 'popwin)
;  (setq display-buffer-function 'popwin:display-buffer)
;  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  ;;https://github.com/m2ym/popwin-el/blob/master/README.md

  ;; powerline; http://safx-dev.blogspot.jp/2012/08/emacspower-line.html; https://gist.github.com/3366866
;  (load "my-powerline" t)
  (powerline-default-theme)

  ;; dsvn
;  (autoload 'svn-status "dsvn" "Run `svn status'." t)
;  (autoload 'svn-update "dsvn" "Run `svn update'." t)

  ;; https://gist.github.com/3105914

  ;;色
;  (custom-set-variables
;   '(custom-enabled-themes (quote (tango-dark)))
;   '(inhibit-startup-screen t))
;  (custom-set-faces
;   (set-frame-parameter nil 'alpha 95)
;   )

  ;; solarized-theme
  ;(load-theme 'solarized-dark t)
  ;(load-theme 'solarized-light t)

  ;;外観
  ;;スクロールバーを消す
;  (set-scroll-bar-mode 'nil)

  ;;対応する括弧を光らせる
  (setq show-paren-delay 0)
  (setq show-paren-style 'single)
  (show-paren-mode t)
  
  ;;行番号の表示
  (global-linum-mode t)
  (setq linum-format "%4d ")

  )

;; http://d.hatena.ne.jp/rubikitch/20100210/emacs 
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-;") 'other-window-or-split)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(indent-tabs-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "../local" t)
