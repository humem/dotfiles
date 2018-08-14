;; local emacs-lisp repository
(setq load-path (cons "~/.emacs.d/lisp" load-path))
(load "extra-autoloads" t)

;; load local setting e.g. PATH, http_proxy, ALL_PROXY
(load "../local" t)

;; Japanese language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setenv "LANG" "C.UTF-8")

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

;;; wdired.el
(require 'dired-x)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; file suffix and mode for programming languages
(add-to-list 'auto-mode-alist '("\\.rake"     . ruby-mode))
(add-to-list 'auto-mode-alist '("[Rr]akefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm"    . html-mode))
(add-to-list 'auto-mode-alist '("Portfile" . tcl-mode))

;; w3m http://www.emacswiki.org/emacs/emacs-w3m
;(setq browse-url-browser-function 'w3m-browse-url)
;(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;(global-set-key "\C-xm" 'browse-url-at-point)

;; IPython: invoke with run-python
;; http://stackoverflow.com/questions/17817019/how-to-open-ipython-interpreter-in-emacs
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args "--pylab"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

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
(when (require 'psvn nil t)
  (add-hook 'dired-mode-hook
            '(lambda ()
               (require 'dired-x)
               (define-key dired-mode-map "V" 'svn-status)
               (turn-on-font-lock)
               ))
  (setq svn-status-hide-unmodified t)
  (setq process-coding-system-alist
        (cons '("svn" . utf-8) process-coding-system-alist)))

;; Word count
(autoload 'word-count-mode "word-count"
  "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

;; Personal Preferences
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(setq migemo-isearch-enable-p nil)
(setq dabbrev-case-fold-search nil)
(setq dired-dwim-target t)
(setq dired-use-ls-dired t)
(setq kill-whole-line t)
(setq make-backup-files nil)
(setq next-line-add-newlines nil)
;;; Work around for El Capitan
;(setq visible-bell t)
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq truncate-partial-width-windows nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (csv-mode clojure-mode typescript typescript-mode evil-magit web-mode powerline popwin matlab-mode markdown-mode magit lua-mode helm exec-path-from-shell evil-surround evil-leader ess dockerfile-mode cmake-mode auto-complete)))
 '(safe-local-variable-values (quote ((checkdoc-minor-mode . t) (mangle-whitespace . t)))))
(menu-bar-mode -1)
(display-time)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'what-line)
(global-set-key "\M-n" 'linum-mode)

;; Set tab width
(setq default-tab-width 4)
(add-hook 'c-mode-common-hook '(lambda () (setq tab-width 4)))

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


(when (>= emacs-major-version 23)
  (require 'cl)

  ;; package managenment; http://sakito.jp/emacs/emacs24.html
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  ;; http://qiita.com/hottestseason/items/1e8a46ad1ebcf7d0e11c
  (defvar installed-package-list
    '(
      auto-complete
      cmake-mode
      dockerfile-mode
      ess
      evil
      evil-leader
      evil-surround
      exec-path-from-shell
      helm
      lua-mode
      magit
      markdown-mode
      matlab-mode
      powerline
      popwin
      typescript-mode
      web-mode
      ))
  (let ((not-installed (loop for x in installed-package-list
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
;    "x" 'execute-extended-command
    "x" 'helm-M-x
    "b" 'switch-to-buffer
    "f" 'find-file
    "d" 'dired
    "j" 'dired-jump
    "k" 'kill-buffer
    "q" 'View-quit
    "w" 'evil-window-prev
    "p" 'comint-previous-input
    "n" 'comint-next-input)

  ;; Evil
  ;; https://lists.ourproject.org/pipermail/implementations-list/2011-September/001140.html
  (evil-mode 1)
  (define-key evil-motion-state-map " " 'evil-scroll-page-down)
  (define-key evil-motion-state-map (kbd "S-SPC") 'evil-scroll-page-up)
  (define-key evil-motion-state-map "H" 'evil-first-non-blank)
  (define-key evil-motion-state-map "L" 'evil-end-of-line)

  ;; ノーマルステートになったら IME をオフにする
  ;; http://ichiroc.hatenablog.com/entry/2013/09/06/075832
  (when (fboundp 'mac-toggle-input-method)
    (add-hook 'evil-normal-state-entry-hook
              '(lambda ()
                 (mac-toggle-input-method nil))))

  ;; evil-surround
  (global-evil-surround-mode 1)

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
  (setq markdown-command "multimarkdown")

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
  (setq web-mode-engines-alist '(("\\.xhtml$" . "smarty")))
  ;; http://qiita.com/kwappa/items/6bde1fe2bbeedc85023e
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq web-mode-attr-indent-offset nil)
               (setq web-mode-markup-indent-offset 2)
               (setq web-mode-css-indent-offset 2)
               (setq web-mode-code-indent-offset 2)
               (setq web-mode-sql-indent-offset 2)
               (setq indent-tabs-mode nil)
               (setq tab-width 2)
               ))
  (custom-set-faces
   '(web-mode-doctype-face           ((t (:foreground "#4A8ACA"))))
;;   '(web-mode-html-tag-face          ((t (:foreground "#4A8ACA"))))
   '(web-mode-html-tag-face          ((t (:foreground "#6AAAEA"))))
   '(web-mode-html-attr-name-face    ((t (:foreground "#87CEEB"))))
   '(web-mode-html-attr-equal-face   ((t (:foreground "#FFFFFF"))))
   '(web-mode-html-attr-value-face   ((t (:foreground "#D78181"))))
;;   '(web-mode-comment-face           ((t (:foreground "#587F35"))))
   '(web-mode-comment-face           ((t (:foreground "#98BF75"))))
   '(web-mode-server-comment-face    ((t (:foreground "#98BF75"))))
   '(web-mode-css-at-rule-face       ((t (:foreground "#DFCF44"))))
   '(web-mode-css-selector-face      ((t (:foreground "#DFCF44"))))
   '(web-mode-css-pseudo-class       ((t (:foreground "#DFCF44"))))
   '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
   '(web-mode-css-string-face        ((t (:foreground "#D78181"))))
   )

  ;; JavaScript
  (setq js-indent-level 2)

  ;; powerline
  ;; http://shibayu36.hatenablog.com/entry/2014/02/11/160945
  (require 'powerline)
  (set-face-attribute 'mode-line nil
                      :foreground "#fff"
                      :background "#000066"
                      :box nil)
  (set-face-attribute 'powerline-active1 nil
                      :foreground "#fff"
                      :background "#666699"
                      :inherit 'mode-line)
  (set-face-attribute 'powerline-active2 nil
                      :foreground "#000"
                      :background "#aeaeb6"
                      :inherit 'mode-line)
  (powerline-center-evil-theme)
  ;; fix for 24.4
  ;; https://github.com/milkypostman/powerline/issues/58
  (add-hook 'desktop-after-read-hook 'powerline-reset)

  ;; バッファ自動再読み込み
  ;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
  (global-auto-revert-mode 1)

  ;;対応する括弧を光らせる
  (setq show-paren-delay 0)
  (setq show-paren-style 'single)
  (show-paren-mode t)
  
  ;;行番号の表示
  (global-linum-mode t)
  (setq linum-format "%4d ")
  )

;; uniquify
;; http://www.clear-code.com/blog/2012/3/20.html
;;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ediff
;; http://qiita.com/l3msh0@github/items/97909d6e2c92af3acc00
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; Saving Emacs Sessions
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

;; http://d.hatena.ne.jp/rubikitch/20100210/emacs 
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-;") 'other-window-or-split)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "color-236"))))
 '(web-mode-comment-face ((t (:foreground "#98BF75"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
 '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
 '(web-mode-css-string-face ((t (:foreground "#D78181"))))
 '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
 '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
 '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
 '(web-mode-html-tag-face ((t (:foreground "#6AAAEA"))))
 '(web-mode-server-comment-face ((t (:foreground "#98BF75")))))

(when (memq window-system '(mac ns))
  ;; 日本語フォント設定
  ;; https://gist.github.com/mitukiii/4365568
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :height 140)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("osaka"))
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("osaka"))

  ;; 現在行をハイライト: http://keisanbutsuriya.blog.fc2.com/blog-entry-91.html
  (global-hl-line-mode t)

  ;; ツールバーを非表示
  (tool-bar-mode 0))
