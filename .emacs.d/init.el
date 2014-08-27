;; local emacs-lisp repository
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "extra-autoloads" t)

;; Japanese language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Auto chmod+x for scripts
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;Dired
(defvar suffix-for-open-list
  '(app csv dmg doc jpg htm html pdf pkg ppg ppt rtf tif tiff xdw xls))
(let ((alist ()))
  (setq dired-guess-shell-alist-user
        (dolist (suffix suffix-for-open-list alist)
          (push (list (concat "\\." (symbol-name suffix)) "open") alist))))

(add-to-list 'auto-mode-alist '("\\.rake"     . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm"    . html-mode))
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))

;; recentf
(recentf-mode 1)

;; Ruby debugger (Rubydb)
(autoload 'rubydb "rubydb3x"
  "Run rubydb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger." t nil)
(global-set-key "\C-cd" 'rubydb)

;; Sdic-mode for lookuping up dictionaries
(autoload 'sdic-describe-word "sdic"
  "look up English or Japanese words" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)

;; launch Dictionary.app
;; http://d.hatena.ne.jp/tunefs/20130212/p1
(global-set-key
 "\C-cW"
 (lambda ()
   (interactive)
   (let ((url (concat "dict://" (read-from-minibuffer "" (current-word)))))
     (browse-url url))))

;; Subversion (svn): use patched version instead of HEAD trunk stored in packages to work with Subversion 1.7.
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

;; Word count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; Personal Preferences
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(setq migemo-isearch-enable-p nil)
(setq dabbrev-case-fold-search nil)
(setq dired-dwim-target t)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
(setq visible-bell t)
(setq truncate-partial-width-windows nil)
(custom-set-variables '(indent-tabs-mode nil))
(display-time)
(tool-bar-mode 0)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'what-line)
(global-set-key "\M-n" 'linum-mode)

;; Disable to color the selected region
(setq transient-mark-mode nil)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Key settings

;; Switch the Command-Key and the Option-Key
;(setq ns-command-modifier   'meta)
;(setq ns-alternate-modifier 'super)

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
      ess
      evil
      evil-leader
      exec-path-from-shell
      helm
      magit
      powerline
      powerline-evil
      popwin
;      psvn
      ))
  (let ((not-installed (loop for x in installing-package-list
                             when (not (package-installed-p x))
                             collect x)))
    (when not-installed
      (package-refresh-contents)
      (dolist (pkg not-installed)
        (package-install pkg))))

  ;; evil-leader
  ;; http://stackoverflow.com/questions/8483182/evil-mode-best-practice
  (global-evil-leader-mode)
  ;; Note: You should enable `global-evil-leader-mode' before you enable
  ;;       `evil-mode', otherwise `evil-leader' won't be enabled in initial
  ;;       buffers (*scratch*, *Messages*, ...).
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "SPC" 'set-mark-command
    "x" 'helm-M-x
    "b" 'switch-to-buffer-other-window
    "f" 'find-file-other-window
    "d" 'dired-other-window
    "j" 'dired-jump-other-window
    "k" 'kill-buffer
    "q" 'View-quit
    "w" 'evil-window-prev
    "p" 'comint-previous-input
    "n" 'comint-next-input)

  ;; evil
  (evil-mode 1)
  ;; https://lists.ourproject.org/pipermail/implementations-list/2011-September/001140.html
  ;(setq evil-emacs-state-cursor '("red" box))
  ;; Movements
  (define-key evil-motion-state-map " " 'evil-scroll-down)
  (define-key evil-motion-state-map (kbd "S-SPC") 'evil-scroll-up)
  (define-key evil-motion-state-map "H" 'evil-first-non-blank)
  (define-key evil-motion-state-map "L" 'evil-end-of-line)

  ;; ノーマルステートになったら IME をオフにする
  ;; http://ichiroc.hatenablog.com/entry/2013/09/06/075832
  (add-hook 'evil-normal-state-entry-hook
            '(lambda ()
               (mac-toggle-input-method nil)))

  ;; exec-path-from-shell
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

  ;; helm (anything); https://github.com/emacs-helm/helm
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

;; 日本語フォント設定
;; https://gist.github.com/mitukiii/4365568
;; Monaco 12pt をデフォルトにする
(set-face-attribute 'default nil
                    :family "Menlo"
;                    :family "Monaco"
                    :height 140)
;; 日本語をヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("osaka"))
;                  '("Hiragino Kaku Gothic ProN"))
;; 半角カナをヒラギノ角ゴProNにする
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("osaka"))
;                  '("Hiragino Kaku Gothic ProN"))

(load "../local" t)
