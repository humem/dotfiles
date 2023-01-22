;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Faster startup (start)
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Load private settings
(let ((my-private (locate-user-emacs-file "my-private.el")))
  (when (file-exists-p my-private)
    (load my-private)))

;; Initialize local emacs-lisp repository
;; https://qiita.com/tadsan/items/431899f76f3765892abd
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; Setup tracker
(defvar my/enable-setup-tracker nil)
(when my/enable-setup-tracker
  (message "Running setup tracker")
  (defvar setup-tracker--level 0)
  (defvar setup-tracker--parents nil)
  (defvar setup-tracker--times nil)

  (when load-file-name
    (push load-file-name setup-tracker--parents)
    (push (current-time) setup-tracker--times)
    (setq setup-tracker--level (1+ setup-tracker--level)))

  (add-variable-watcher
   'load-file-name
   (lambda (_ v &rest __)
     (cond ((equal v (car setup-tracker--parents))
            nil)
           ((equal v (cadr setup-tracker--parents))
            (setq setup-tracker--level (1- setup-tracker--level))
            (let* ((now (current-time))
                   (start (pop setup-tracker--times))
                   (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                               (/ (- (nth 2 now) (nth 2 start)) 1000))))
              (with-current-buffer (get-buffer-create "*setup-tracker*")
                (save-excursion
                  (goto-char (point-min))
                  (dotimes (_ setup-tracker--level) (insert "> "))
                  (insert
                   (file-name-nondirectory (pop setup-tracker--parents))
                   " (" (number-to-string elapsed) " msec)\n")))))
           (t
            (push v setup-tracker--parents)
            (push (current-time) setup-tracker--times)
            (setq setup-tracker--level (1+ setup-tracker--level)))))))

;; Profiler (start)
(defvar my/enable-profiler nil)
(when my/enable-profiler
  (require 'profiler)
  (profiler-start 'cpu))

;;
;; Initialize leaf package manager
;; https://github.com/conao3/leaf.el
;; https://emacs-jp.github.io/tips/emacs-in-2020
;;

;; Prevent custom from updating init file
(customize-set-variable 'custom-file
                        (locate-user-emacs-file "custom.el"))

(eval-and-compile
  (customize-set-variable
   'package-archives
   '(("org"   . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra    :ensure t)
    ;; (leaf el-get   :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree :ensure t))

(leaf use-package :ensure t)

;;
;; Initialize Evil
;;

;; set before loading evil
(defvar evil-respect-visual-line-mode t)
(defvar evil-want-keybinding nil)

(leaf evil
  :ensure t
  :custom (;; restore TAB functionality in org mode
           (evil-want-C-i-jump   . nil)
           (evil-want-fine-undo  . t)
           (evil-want-minibuffer . t))
  :global-minor-mode evil-mode
  :defun evil-global-set-key
  :config
  (evil-global-set-key 'motion (kbd "SPC")   'evil-scroll-page-down)
  (evil-global-set-key 'motion (kbd "S-SPC") 'evil-scroll-page-up)
  (evil-global-set-key 'motion (kbd "<home>") 'evil-goto-first-line)
  (evil-global-set-key 'motion (kbd "<end>") 'evil-goto-line)
  (evil-global-set-key 'motion (kbd "C-]") 'other-window-or-split)
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  ;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;   (evil-set-initial-state 'dashboard-mode 'normal))

  (leaf evil-collection
    :ensure t
    :custom (evil-collection-setup-minibuffer . t)
    :defun evil-collection-define-key
    :config
    (evil-collection-init)
    ;; fix evil-collection-dired-setup
    (evil-collection-define-key 'normal 'dired-mode-map
      ;; move
      (kbd "SPC")   'evil-scroll-page-down
      (kbd "S-SPC") 'evil-scroll-page-up
      "gg" 'evil-goto-first-line
      "G"  'evil-goto-line
      ;; file
      "e"  'dired-find-file
      "o"  'dired-find-file-other-window
      "v"  'dired-view-file
      ;; sort
      "s"  'dired-sort-toggle-or-edit
      )
    ;;(evil-collection-view-setup)
    (evil-collection-define-key 'normal 'view-mode-map
      "0" 'evil-beginning-of-line ; 'text-scale-adjust
      ))

  (leaf evil-surround
    ;; https://blog.3qe.us/entry/2020/05/12/012958
    ;; b B r a: ) } ] >
    ;; yswb: 単語を丸括弧で囲む
    ;; csbB: 丸括弧を大括弧に書き換える
    ;; dsb: 丸括弧を削除する
    ;; v Sb: 選択文字列を丸括弧で囲む
    ;; v Sf: 選択文字列を関数形式で囲む
    :ensure t
    :global-minor-mode global-evil-surround-mode)

  (leaf undo-tree
    :ensure t
    :after evil
    :bind ((undo-tree-visualizer-mode-map
            ([remap evil-previous-visual-line] . 'undo-tree-visualize-undo)
            ([remap evil-next-visual-line]     . 'undo-tree-visualize-redo)))
    :custom ((undo-tree-auto-save-history . nil)
             (evil-undo-system . 'undo-tree))
    :hook (dired-mode-hook . undo-tree-mode)
    :global-minor-mode global-undo-tree-mode))

(leaf general
  :ensure t
  :require t
  :config
  (declare-function my/leader-keys "init")
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,")
  (my/leader-keys
    "a" 'avy-goto-char-timer
    "b" 'switch-to-buffer
    "e" 'eval-region
    "d" 'dired
    "f" 'find-file
    "g" 'magit-status
    "j" 'dired-jump
    "k" 'kill-buffer
    "n" 'display-line-numbers-mode
    "o" 'other-window-or-split
    "q" 'delete-next-window
    "s" 'save-buffer
    "t" 'gts-do-translate
    "u" 'undo-tree-visualize
    "x" 'execute-extended-command
    "v" 'visual-line-mode
    "w" 'evil-window-prev
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-vertically
    "3" 'split-window-horizontally
    "4" 'switch-to-buffer-other-window
    "5" 'split-side-window
    "=" 'er/expand-region
    ";" 'comment-dwim
    "'" 'ace-window
    "," 'xref-pop-marker-stack
    "." 'xref-find-definitions
    "/" 'xref-find-references
    "SPC" 'set-mark-command
    ))

(leaf neotree
  :ensure t
  :defvar neotree-mode-map
  :config
  (declare-function evil-define-key "evil-core")
  (evil-define-key 'normal neotree-mode-map
    "g" 'neotree-refresh
    "n" 'neotree-next-line
    "p" 'neotree-previous-line
    "q" 'neotree-hide
    "v" 'neotree-quick-look
    "A" 'neotree-stretch-toggle
    "H" 'neotree-hidden-file-toggle))

;;
;; Personal enhancements
;;

(leaf japanese
  :config
  (set-language-environment "Japanese")
  (set-default-coding-systems 'utf-8-unix)
  (prefer-coding-system       'utf-8-unix)
  (setenv "LANG" "C.UTF-8")
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))
  ;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
  ;; ※ 設定値の car を "undecided-dos" にしておくと、Windows コマンドの出力にも柔軟に対応できます。
  ;; デフォルト フォント
  ;; "PlemolJP35Console"
  ;; "UDEV Gothic"
  ;; "HackGen35Nerd Console"
  (defvar my/fonts-family "HackGen35Nerd Console")
  (defvar my/fonts-height 120)
  (set-face-attribute
   'default nil :family my/fonts-family :height my/fonts-height)
)

(leaf mozc
  :doc "日本語入力"
  :url "https://w.atwiki.jp/ntemacs/pages/48.html"
  :ensure t
  ;; :require t
  :init
  (advice-add 'toggle-input-method
              :before (lambda (&rest _args) (require 'mozc)))
  :custom ((default-input-method . "japanese-mozc-im")
           (mozc-leim-title . "あ")
           (mozc-candidate-style . 'popup))
  :bind ((("C-\\"  . 'toggle-input-method)
          ("C-SPC" . 'toggle-input-method)
          ("<f2>"  . 'toggle-input-method)))
         ;; (isearch-mode-map
         ;;  :package isearch
         ;;  ("C-\\"  . 'toggle-input-method)
         ;;  ("C-SPC" . 'toggle-input-method)
         ;;  ("<f2>"  . 'toggle-input-method))
         ;; (wdired-mode-map
         ;;  :package wdired
         ;;  ("C-\\"  . 'toggle-input-method)
         ;;  ("C-SPC" . 'toggle-input-method)
         ;;  ("<f2>"  . 'toggle-input-method)))
  :defun mozc-session-sendkey
  :defer-config
  (leaf mozc-im :ensure t :require t)
  (leaf mozc-popup :ensure t :require t)

  ;; (defvar mozc-im-mode nil)
  ;; (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  ;; (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
  ;; ;; isearch を利用する前後で IME の状態を維持するための対策
  ;; (add-hook 'isearch-mode-hook (lambda () (setq im-state mozc-im-mode)))
  ;; (add-hook 'isearch-mode-end-hook
  ;;           (lambda ()
  ;;             (unless (eq im-state mozc-im-mode)
  ;;               (if im-state
  ;;                   (activate-input-method default-input-method)
  ;;                 (deactivate-input-method)))))
  ;; ;; wdired 終了時に IME を OFF にする
  ;; (advice-add 'wdired-finish-edit
  ;;             :after (lambda (&rest args)
  ;;                      (deactivate-input-method)))
  ;; Windows の mozc では、セッション接続直後 directモード になるので
  ;; hiraganaモード にする
  (when (/= (length (getenv "WSL_DISTRO_NAME")) 0)
    (advice-add
     'mozc-session-execute-command
     :after (lambda (&rest args)
              (when (eq (nth 0 args) 'CreateSession)
                ;; (mozc-session-sendkey '(hiragana)))))
                (mozc-session-sendkey '(Hankaku/Zenkaku))))))

  (leaf mozc-cursor-color
    :disabled t
    :el-get iRi-E/mozc-el-extensions
    :config
    (setq mozc-cursor-color-alist
          '((direct        . "black") ;"white")
            (read-only     . "dim gray") ;"yellow")
            (hiragana      . "light green")
            (full-katakana . "goldenrod")
            (half-ascii    . "dark orchid")
            (full-ascii    . "orchid")
            (half-katakana . "dark goldenrod")))
    ;; mozc-cursor-color を利用するための対策
    ;; (defvar-local mozc-im-mode nil)
    ;; (make-variable-buffer-local 'mozc-im-mode)
    (defvar mozc-im-mode nil)
    (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
    (advice-add 'mozc-cursor-color-update
                :around (lambda (orig-fun &rest args)
                          (let ((mozc-mode mozc-im-mode))
                            (apply orig-fun args))))))

(leaf mode-line
  :custom
  ((display-time-string-forms
    . '(year "-" month "-" day " " dayname " " 24-hours ":" minutes)))
  :config
  (column-number-mode)
  (display-time))

;; ;; Type backslash instead of yen mark
;; (define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す

(leaf startup-performance
  ;; The default is 800 kilobytes.  Measured in bytes.
  :custom (gc-cons-threshold . `,(* 50 1000 1000)) ;; 50 megabytes
  :init
  (defun my/display-startup-time ()
    "Report startup performance."
    (message
     "Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
     gcs-done))
  (declare-function my/display-startup-time "init")
  (add-hook 'emacs-startup-hook #'my/display-startup-time))

(leaf terminal-emacs
  :doc "settings for terminal emacs"
  :unless (display-graphic-p)
  :init
  ;; Mouse scrolling in terminal emacs
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  ;; scroll bar
  ;(global-yascroll-bar-mode 1)
  ;(setq yascroll:delay-to-hide nil)
  ;; sync with x clipboard
  (when (file-exists-p "/usr/bin/xsel")
    (defvar env-display "")
    (when (getenv "TMUX")
      (setq env-display "env $(tmux showenv DISPLAY) "))

    ;;(setq interprogram-cut-function   'xsel-cut-function)
    ;;(setq interprogram-paste-function 'xsel-paste-function)
    ;; use Ctrl+Shift+V to paste with system clipboard

    (defun xsel-cut-function (text &optional _push)
      (with-temp-buffer
        (insert text)
        (call-shell-region (point-min) (point-max)
                           (concat env-display "xsel -bi"))))

    (defun xsel-paste-function ()
      (let ((xsel-output
             (shell-command-to-string
              (concat env-display "xsel --clipboard --output"))))
        (unless (string= (car kill-ring) xsel-output)
          xsel-output )))

    (defun toggle-cut-function ()
      (interactive)
      (if (eq interprogram-cut-function 'xsel-cut-function)
          (setq interprogram-cut-function 'gui-select-text)
        (setq interprogram-cut-function 'xsel-cut-function)))

    (defun toggle-paste-function ()
      (interactive)
      (if (eq interprogram-paste-function 'xsel-paste-function)
          (setq interprogram-paste-function 'gui-selection-value)
        (setq interprogram-paste-function 'xsel-paste-function))))

  (leaf evil-terminal-cursor-changer
    :doc "Change cursor shape and color by evil state in terminal"
    :ensure t
    :defun evil-terminal-cursor-changer-activate
    :config
    (evil-terminal-cursor-changer-activate)))

 (leaf web-browser-for-wsl
    :init
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-browser-function 'browse-url-generic
              browse-url-generic-program cmd-exe
              browse-url-generic-args cmd-args))))

(leaf window
  :bind (("C-;"    . 'other-window-or-split)
         ("C-]"    . 'other-window-or-split)
         ("<f8>"   . 'other-window-or-split)
         ("C-<f8>" . 'other-window-or-split))
  :preface
  (defun other-window-or-split ()
    "Select other window or split window horizontally."
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1)
    ;; IMEをオフにする
    (when (fboundp 'mac-toggle-input-method)
      (mac-toggle-input-method nil)))

  (defun split-side-window ()
    "Split side window."
    (interactive)
    (split-window-horizontally)
    (split-window-horizontally)
    (other-window 2)
    (delete-window))

  (defun delete-next-window ()
    "Delete the next window."
    (interactive)
    (unless (one-window-p)
      (other-window 1)
      (delete-window))))

;;
;; Customize builtin packages
;;

(leaf cus-start
  :doc "define customization properties of builtins"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom
  '((auto-save-list-file-prefix . nil) ;; startup.el
    (create-lockfiles . nil)
    ;; (debug-on-error . t)
    (enable-recursive-minibuffers . t)
    (frame-resize-pixelwise . t)
    (garbage-collection-messages . t)
    (history-delete-duplicates . t)
    (history-length . 1000)
    (indent-tabs-mode . nil)
    (inhibit-startup-screen . t) ;; startup.el
    (init-file-debug . t) ;; startup.el
    (menu-bar-mode . nil)
    (ring-bell-function . 'ignore)
    ;; (scroll-bar-mode . nil) ;; scroll-bar
    (scroll-conservatively . 100)
    (scroll-preserve-screen-position . t)
    (tab-width . 2)
    (text-quoting-style . 'straight)
    (tool-bar-mode . nil)
    (transient-mark-mode . nil))
    ;; (truncate-lines . t)
    ;; (use-dialog-box . nil)
    ;; (use-file-dialog . nil)
    (visible-bell . nil)
  :init
  ;; (keyboard-translate ?\C-h ?\C-?)
  (defalias 'yes-or-no-p 'y-or-n-p))

;; startup.el
(eval '(setq inhibit-startup-echo-area-message "umemoto"))

(leaf ansi-color
  :hook ((compilation-filter-hook . ansi-color-apply-on-buffer)
         (eshell-preoutput-filter-functions . ansi-color-apply))
  :defun ansi-color-apply-on-region
  :preface
  (defun ansi-color-apply-on-buffer ()
    "Enable colors in the buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

(leaf bytecomp
  :custom
   (byte-compile-warnings . '(not cl-functions obsolete)))

(leaf dabbrev
  :custom (dabbrev-case-fold-search . nil))

(leaf desktop
  :custom
  ((desktop-save-mode . t)
   (desktop-files-not-to-save
    . "\\(\\`/[^/:]*:\\|(ftp)\\'\\|\\.gz\\'\\|\\.jpg\\'\\|\\.png\\'\\)")
   (desktop-restore-eager . 0)))

(leaf dired
  :custom
  ((dired-dwim-target . t)
   (dired-listing-switches . "-aho")
   (dired-use-ls-dired . t)
   ;; dired-x
   (dired-guess-shell-alist-user . '(("\\.*" "open")))))

(leaf display-line-numbers
  :global-minor-mode global-display-line-numbers-mode)

(leaf ediff-wind
  :custom
  ((ediff-split-window-function . 'split-window-horizontally)
   (ediff-window-setup-function . 'ediff-setup-windows-plain)))

(leaf files
  :custom ((find-file-visit-truename . t)
           (make-backup-files . nil)
           (require-final-newline . t))
  :config
  (leaf save
    :init
    (defun execute-rsync ()
      "Execute .rsync script file in the current directory if exists."
      (interactive)
      (let ((rsync-file
             (file-name-concat default-directory ".rsync")))
        (when (file-exists-p rsync-file)
          (async-shell-command rsync-file))))
    (declare-function execute-rsync "init")
    ;; hide the buffer *Async Shell Command*
    (add-to-list 'display-buffer-alist
                 `(,shell-command-buffer-name-async
                   display-buffer-no-window))
    (add-hook 'before-save-hook
              ;; simple.el
              #'delete-trailing-whitespace)
    (add-hook 'after-save-hook
              #'execute-rsync
              ;; executable.el
              #'executable-make-buffer-file-executable-if-script-p)))

(leaf frame
  :custom (blink-cursor-mode . nil))

(leaf hl-line
  :disabled t
  :global-minor-mode global-hl-line-mode)

(leaf js
  :custom (js-indent-level . 2))

(leaf mouse
  :custom (mouse-drag-copy-region . t))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0))
  :global-minor-mode show-paren-mode)

(leaf pixel-scroll
  :global-minor-mode pixel-scroll-mode)

(leaf recentf
  :custom
  (recentf-exclude . '("\\.gz\\'" "\\.jpg\\'" "\\.png\\'"))
  :global-minor-mode recentf-mode)

(leaf simple
  :custom ((kill-whole-line  . t)
           (next-line-add-newlines . nil))
  :global-minor-mode global-visual-line-mode)

(leaf text-scale
  :doc "Magnify and demagnify texts"
  :custom (text-scale-mode-step . 1.05) ;; 1.2 ;; face-remap.el
  :preface
  (defun text-scale-reset ()
    "Reset text scale."
    (interactive)
    (text-scale-increase 0))
  :bind (("C-+" . text-scale-increase)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("C-0" . text-scale-reset)))

(defvar my/modus-themes 'modus-operandi) ;; modus-vivendi
(leaf themes/modus-themes
  :doc "highly accessible and customizable themes"
  :emacs>= 28
  :custom
  ((modus-themes-bold-constructs   . nil)
   (modus-themes-italic-constructs . nil)
   (modus-themes-region            . '(bg-only no-extend)))
  :config
  (require-theme 'modus-themes)
  (load-theme my/modus-themes)
  (global-set-key (kbd "<f5>") 'modus-themes-toggle))

(leaf tramp
  :custom ((tramp-remote-path . '(tramp-own-remote-path))
           (tramp-verbose . 1)
           ;; tramp-sh.el
           (tramp-inline-compress-start-size . 1000)
           ;; ~/.ssh/config
           (tramp-use-ssh-controlmaster-options . nil)
           ;; files.el
           (remote-file-name-inhibit-locks . t)))

(leaf uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)))

(leaf vc-hooks
  :custom ((vc-follow-symlinks . t)
           (vc-handled-backends . '(Git))))

;;
;; Initialize completions
;;

(leaf company
  :disabled t
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :config
  ;; (add-to-list 'company-backends 'company-yasnippet)
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf corfu
  :disabled nil
  :ensure t
  :custom (corfu-auto . t)
  :global-minor-mode global-corfu-mode)

(leaf consult
  :ensure t
  :bind (([remap goto-line] . consult-goto-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap isearch-forward] . my-consult-line))
  :defvar xref-show-xrefs-function xref-show-definitions-function
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :preface
  ;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
  (defun my-consult-line (&optional at-point)
    "Consult-line thing-at-point if AT-POINT is non-nil."
    (interactive "P")
    (if at-point
        (consult-line (thing-at-point 'symbol))
      (consult-line))))

;; ;; embark-consultを読み込む
;; (with-eval-after-load 'consult
;;   (with-eval-after-load 'embark
;;     (require 'embark-consult)))

(leaf marginalia
  :ensure t
  :hook ((after-init-hook . marginalia-mode)))

(leaf orderless
  :ensure t
  :custom
  ((completion-styles . '(orderless))))

(leaf vertico
  :ensure t
  :custom
  ;; 補完候補を最大20行まで表示する
  (vertico-count . 20)
  :bind
  (:vertico-map
   (("C-l" . filename-upto-parent)
    ;; ;; C-s/C-rで行を移動できるようにする
    ;; ("C-r" . vertico-previous)
    ;; ("C-s" . vertico-next)
    ;; evil-want-minibuffer対応
    ;; C-n/p, jkで選択移動を有効化
    ([remap next-window-line]     . vertico-next)
    ([remap previous-window-line] . vertico-previous)
    ([remap evil-complete-next]     . vertico-next)
    ([remap evil-complete-previous] . vertico-previous)
    ([remap evil-paste-pop]     . vertico-previous)
    ([remap evil-next-line]     . vertico-next)
    ([remap evil-previous-line] . vertico-previous)
    ([remap evil-next-visual-line]     . vertico-next)
    ([remap evil-previous-visual-line] . vertico-previous)
    ;; C-f/bで選択ページ送りを有効化
    ([remap forward-char]  . vertico-scroll-up)
    ([remap backward-char] . vertico-scroll-down)
    ([remap evil-jump-forward]  . vertico-scroll-up)
    ([remap evil-jump-backward] . vertico-scroll-down)
    ([remap evil-scroll-page-down] . vertico-scroll-up)
    ([remap evil-scroll-page-up]   . vertico-scroll-down)
    ;; RETで選択確定を有効化
    ([remap evil-ret] . vertico-exit)
    ;; TAB, C-i, C-j: 補完
    ("C-j" . vertico-insert)
    ))
  :hook ((after-init-hook . vertico-mode)
         ;; savehist-modeを使ってVerticoの順番を永続化する
         (after-init-hook . savehist-mode))
  :preface
  (defun filename-upto-parent ()
    "Move to parent directory like \"cd ..\" in find-file."
    (interactive)
    (let ((sep (eval-when-compile (regexp-opt '("/" "\\")))))
      (save-excursion
        (left-char 1)
        (when (looking-at-p sep)
          (delete-char 1)))
      (save-match-data
        (when (search-backward-regexp sep nil t)
          (right-char 1)
          (filter-buffer-substring
           (point)
           (save-excursion (end-of-line) (point))
           #'delete))))))

;;
;; lsp: select among eglot, lsp-bridge and lsp-mode
;;

(leaf python
  :config
  (leaf python-mode
    :ensure t
    :custom (python-indent-guess-indent-offset-verbose . nil))

  (leaf blacken :ensure t)

  (leaf py-isort :ensure t)

  (leaf pyvenv
    :disabled nil
    :ensure t
    :hook (python-mode-hook . pyvenv-mode)))

;; ??Note: pyrightconfig.json is required for venv
(leaf eglot
  :disabled nil
  :ensure t
  :hook ((python-mode-hook . eglot-ensure))
  :custom ((eldoc-echo-area-use-multiline-p . nil)))

;; Note: company and corfu should be disabled; acm is used
(leaf lsp-bridge
  :disabled t
  :config
  (defvar root_dir (file-name-concat (getenv "HOME") "distfiles"))
  (defvar lsp-bridge-path (file-name-concat root_dir "lsp-bridge"))
  (when (file-directory-p lsp-bridge-path)
    (add-to-list 'load-path lsp-bridge-path)
    ;; (require 'yasnippet)
    ;; (yas-global-mode 1)
    (require 'lsp-bridge)
    (global-lsp-bridge-mode)
    (define-key acm-mode-map (kbd "RET") 'newline))

  (unless (display-graphic-p)
    (defvar acm-terminal-path (file-name-concat root_dir, "acm-terminal"))
    (when (file-directory-p lsp-bridge-path)
      (add-to-list 'load-path acm-terminal-path)
      (with-eval-after-load 'acm
        (require 'acm-terminal))))
  ;; (add-hook 'python-mode-hook 'lsp-bridge-mode)
  ;; (setq lsp-bridge-enable-mode-line nil)
  ;; (define-key acm-mode-map [remap evil-complete-next] 'acm-select-next)
  ;; (define-key acm-mode-map [remap evil-complete-previous] 'acm-select-prev)
  ;; (setq acm-candidate-match-function 'orderless-flex)
  ;; (setq lsp-bridge-complete-manually t)
  ;; (add-to-list 'lsp-bridge-completion-stop-commands "evil-complete-next")
  ;; (add-to-list 'lsp-bridge-completion-stop-commands "evil-complete-previous")
  ;; (add-to-list 'lsp-bridge-completion-stop-commands "dabbrev-expand")
  )

;; Note: work with corfu instead company
(leaf lsp-mode
  :disabled t
  :ensure t
  :commands (lsp lsp-deferred)
  :config
  :custom ((lsp-keymap-prefix        . "C-c l")
           (lsp-log-io               . t)
           (lsp-keep-workspace-alive . nil)
           (lsp-document-sync-method . 2)
           (lsp-response-timeout     . 5)
           (lsp-enable-file-watchers . nil)
           ;; use corfu instead of company
           (lsp-completion-provider . :none))
  :hook (lsp-mode-hook . lsp-headerline-breadcrumb-mode)
  :init
  (leaf lsp-ui
    :ensure t
    :after lsp-mode
    :custom ((lsp-ui-doc-enable            . t)
             (lsp-ui-doc-position          . 'at-point)
             (lsp-ui-doc-header            . t)
             (lsp-ui-doc-include-signature . t)
             (lsp-ui-doc-max-width         . 150)
             (lsp-ui-doc-max-height        . 30)
             (lsp-ui-doc-use-childframe    . nil)
             (lsp-ui-doc-use-webkit        . nil)
             (lsp-ui-peek-enable           . t)
             (lsp-ui-peek-peek-height      . 20)
             (lsp-ui-peek-list-width       . 50))
    :bind ((lsp-ui-mode-map ([remap xref-find-definitions] .
                             lsp-ui-peek-find-definitions)
                            ([remap xref-find-references] .
                             lsp-ui-peek-find-references))
           (lsp-mode-map ("C-c s" . lsp-ui-sideline-mode)
                         ("C-c d" . lsp-ui-doc-mode)))
    :hook ((lsp-mode-hook . lsp-ui-mode)))
  :config
  (leaf lsp-treemacs :ensure t)

  (leaf lsp-pyright
    :ensure t
    :custom (lsp-pyright-venv-path . `,(getenv "HOME"))
    :hook (python-mode-hook . (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))

  (leaf python-mode
    :ensure t
    :hook (python-mode-hook . lsp-deferred)
    :custom
    ;; NOTE: Set these if Python 3 is called "python3" on your system!
    ;; (python-shell-interpreter "python3")
    ;; (dap-python-executable "python3")
    (dap-python-debugger . 'debugpy)
    :config
    (require 'dap-python))

  (leaf dap-mode
    :ensure t
    :commands dap-debug
    :custom
    (lsp-enable-dap-auto-configure . nil)
    :config
    (dap-ui-mode 1)
    (require 'dap-node)
    (dap-node-setup)
    (general-define-key
     :keymaps 'lsp-mode-map :prefix lsp-keymap-prefix "d"
     '(dap-hydra t :wk "debugger"))))

;;
;; Initialize other external packages
;;

(leaf ace-window
  :ensure t
  :custom
  ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
  :custom-face
  (aw-leading-char-face . '((t (:height 2.0)))))

(leaf avy
  :ensure t
  :bind
  (("C-:"     . avy-goto-char-timer)
   ("C-*"     . avy-resume)
   ("M-g M-g" . avy-goto-line))
  :config
  (leaf avy-zap
    :ensure t
    :bind
    ([remap zap-to-char] . avy-zap-to-char)))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(leaf doom-modeline
  :ensure t
  :hook (after-init-hook . doom-modeline-mode)
  :custom
  (;; (doom-modeline-bar-width . 3) ;; 4
   (doom-modeline-height . 20) ;; 25
   (doom-modeline-major-mode-color-icon . t)
   (doom-modeline-minor-modes . t) ;; nil
   ;; (doom-modeline-github . nil)
   ;; (doom-modeline-mu4e . nil)
   ;; (doom-modeline-irc . nil)
   ))

(leaf emmet-mode :ensure t)

(leaf google-this :ensure t)

(leaf expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(leaf flycheck
  :disabled t
  :doc "On-the-fly syntax checking"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t))
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :config
  (leaf flycheck-package
    :doc "A Flycheck checker for elisp package authors"
    :ensure t
    :config
    (flycheck-package-setup))

  (leaf flycheck-elsa
    :doc "Flycheck for Elsa."
    :emacs>= 25
    :ensure t
    :config
    (flycheck-elsa-setup)))

(leaf flymake
  ;; :ensure t
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :defer-config
  (leaf flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :hook (flymake-mode-hook)))

(leaf gcmh
  :ensure t
  :custom (gcmh-verbose . t)
  :global-minor-mode gcmh-mode
  :preface
  (defun my/gc-debug-function (str)
  "Display the memory size used after garbage coolection."
    (let ((sum 0))
      (dolist (x str)
        (setq sum (+ sum (* (cl-second x) (cl-third x)))))
      (message "Used Memory: %d MB" (/ sum (* 1024 1024)))))
  (declare-function my/gc-debug-function "init")
  (advice-add 'garbage-collect :filter-return #'my/gc-debug-function))

(leaf git-gutter
  :ensure t
  :global-minor-mode global-git-gutter-mode)

(defvar my/deepl-auth-key nil)
(leaf go-translate
  :ensure t
  :bind (("C-c t" . gts-do-translate))
  :custom ((gts-translate-list . '(("en" "ja") ("ja" "en"))))
  :defvar gts-default-translator gts-translate-list
  :defun gts-translator gts-noprompt-picker gts-deepl-engine gts-google-engine gts-bing-engine gts-buffer-render
  :preface
  (defun gts-do-translate-zh-ja ()
    "Translate zh to ja."
    (interactive)
    (let ((tlist gts-translate-list))
      (setq gts-translate-list '(("zh" "ja")))
      (gts-do-translate)
      (setq gts-translate-list tlist)))
  :config
  (defvar gts-engins (list (gts-google-engine) (gts-bing-engine)))
  (setq gts-default-translator
	      (gts-translator
	       :picker (gts-noprompt-picker)
	       :engines (list
		               ;; (gts-deepl-engine
                   ;;  :auth-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx:xx" :pro nil)
		               (gts-google-engine)
		               (gts-bing-engine))
 	       :render (gts-buffer-render))))

(leaf helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-key]      . helpful-key)
  ([remap describe-variable] . helpful-variable))

(leaf hide-mode-line
  :disabled t
  :ensure t
  :hook (neotree-mode-hook imenu-list-minor-mode-hook minimap-mode-hook))

(leaf highlight-indent-guides
  :ensure t
  :blackout t
  :hook (((prog-mode-hook yaml-mode-hook)
          . highlight-indent-guides-mode))
  :custom ((highlight-indent-guides-method . 'character)
           (highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-character . ?\|)))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :defvar magit-mode-map
  :config
  (define-key magit-mode-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key magit-mode-map
    [remap magit-diff-show-or-scroll-up] 'evil-scroll-page-up))

(leaf minions
  :ensure t
  :custom (minions-mode-line-lighter . "[+]")
  :config
  (minions-mode))

;; Warning: load-theme hangs!
(leaf moody
  :disabled t ;; load-theme hangs
  :doc "Tabs and ribbons for the mode line"
  :ensure t
  :custom ((x-underline-at-descent-line . t)
           (moody-mode-line-height      . nil))
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(leaf mwim
  :ensure t
  :bind (("C-a" . 'mwim-beginning)
         ("C-e" . 'mwim-end))
  :init
  (evil-global-set-key 'motion (kbd "C-e") 'mwim-end))

;; Note: slower startup
(leaf org-modern
  :disabled t
  :ensure t
  :global-minor-mode global-org-modern-mode)

(leaf paradox
  :doc "wrapper of package.el"
  :ensure t
  :config
  (let ((inhibit-message t)
        (message-log-max nil))
    (paradox-enable)))

;; powerline
;; http://shibayu36.hatenablog.com/entry/2014/02/11/160945
(leaf powerline
  :disabled t
  :ensure t
  :after minions
  :config
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
  ;; http://ytsk.hatenablog.com/entry/2015/09/23/021856
  (setq ns-use-srgb-colorspace nil)
  (powerline-center-evil-theme)
  ;; fix for 24.4
  ;; https://github.com/milkypostman/powerline/issues/58
  ;; (add-hook 'desktop-after-read-hook 'powerline-reset)
  (defpowerline powerline-major-mode "")
  (defpowerline powerline-process "")
  (defpowerline powerline-minor-modes minions-mode-line-modes)
  )

(leaf rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode-hook       . rainbow-delimiters-mode)))

(leaf restart-emacs :ensure t)

(leaf rust-mode
  :ensure t
  :custom ((rust-format-on-save . t))
  :config
  (leaf cargo
    :ensure t
    :commands cargo-minor-mode
    :hook ((rust-mode-hook . cargo-minor-mode))))

(leaf smartparens
  :disabled nil
  :ensure t
  ;; :hook (after-init-hook . smartparens-global-strict-mode) ; strictモードを有効化
  :require smartparens-config
  :custom ((electric-pair-mode . nil))) ; electirc-pair-modeを無効化

(leaf super-save
  :ensure t
  ;; :require t
  ;; :config
  ;; (super-save-mode 1)
  )

(leaf topsy
  :ensure t
  :hook (prog-mode-hook . topsy-mode))

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(leaf tree-sitter
  :disabled t
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (leaf tree-sitter-langs :ensure t)

;; (leaf tree-sitter
;;   :ensure (t tree-sitter-langs)
;;   :require tree-sitter-langs
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
;;   ;; TSXの対応
;;   (tree-sitter-require 'tsx)
;;   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
;;   ;; ハイライトの追加
;;   (tree-sitter-hl-add-patterns 'tsx
;;     [
;;      ;; styled.div``
;;      (call_expression
;;       function: (member_expression
;;                  object: (identifier) @function.call
;;                  (.eq? @function.call "styled"))
;;       arguments: ((template_string) @property.definition
;;                   (.offset! @property.definition 0 1 0 -1)))
;;      ;; styled(Component)``
;;      (call_expression
;;       function: (call_expression
;;                  function: (identifier) @function.call
;;                  (.eq? @function.call "styled"))
;;       arguments: ((template_string) @property.definition
;;                   (.offset! @property.definition 0 1 0 -1)))
;;      ])
;;   )

(leaf typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :custom (typescript-indent-level . 2)
  :mode (("\\.tsx\\'" . typescript-tsx-mode)))

(leaf yaml-mode :ensure t)

(leaf yasnippet
  :ensure t)
  ;; :init (yas-global-mode 1))
  ;; :custom
  ;; ((yas-snippet-dirs . '("`/.emacs.d/yasnippets"))))

;; ;; web-mode
;; (add-to-list 'auto-mode-alist '("\\.?html$" . web-mode))
;; (setq web-mode-engines-alist '(("\\.xhtml$" . "smarty")))
;; ;; http://qiita.com/kwappa/items/6bde1fe2bbeedc85023e
;; (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
;; (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
;; (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
;; (add-hook 'web-mode-hook
;;           '(lambda ()
;;              (setq web-mode-attr-indent-offset nil)
;;              (setq web-mode-markup-indent-offset 2)
;;              (setq web-mode-css-indent-offset 2)
;;              (setq web-mode-code-indent-offset 2)
;;              (setq web-mode-sql-indent-offset 2)
;;              (setq web-mode-script-padding 0)
;;              (setq web-mode-style-padding 0)
;;              (setq indent-tabs-mode nil)
;;              (setq tab-width 2)
;;              ))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(hl-line ((t (:background "#3F3F3F"))))
;;  '(web-mode-comment-face ((t (:foreground "#98BF75"))))
;;  '(web-mode-css-at-rule-face ((t (:foreground "#DFCF44"))))
;;  '(web-mode-css-property-name-face ((t (:foreground "#87CEEB"))))
;;  '(web-mode-css-pseudo-class ((t (:foreground "#DFCF44"))))
;;  '(web-mode-css-selector-face ((t (:foreground "#DFCF44"))))
;;  '(web-mode-css-string-face ((t (:foreground "#D78181"))))
;;  '(web-mode-doctype-face ((t (:foreground "#4A8ACA"))))
;;  '(web-mode-html-attr-equal-face ((t (:foreground "#FFFFFF"))))
;;  '(web-mode-html-attr-name-face ((t (:foreground "#87CEEB"))))
;;  '(web-mode-html-attr-value-face ((t (:foreground "#D78181"))))
;;  '(web-mode-html-tag-face ((t (:foreground "#6AAAEA"))))
;;  '(web-mode-server-comment-face ((t (:foreground "#98BF75")))))

(leaf which-key
  :ensure t
  :require t
  :config
  (which-key-mode))

;; Profiler (end)
(when my/enable-profiler
  (declare-function profiler-report "subr")
  (declare-function profiler-stop "subr")
  (profiler-report)
  (profiler-stop))

;; Faster startup (end)
(setq file-name-handler-alist my-saved-file-name-handler-alist)

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; indent-tabs-mode: nil
;; End:
