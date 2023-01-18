;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Load private settings
(let ((my-private (locate-user-emacs-file "my-private.el")))
  (when (file-exists-p my-private)
    (load my-private)))

;; Profile startup
(defvar my/enable-profiler nil)
(when my/enable-profiler
  (require 'profiler)
  (profiler-start 'cpu))

;; Initialize local emacs-lisp repository
;; https://qiita.com/tadsan/items/431899f76f3765892abd
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

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
    (leaf el-get   :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size     . 30)
             (imenu-list-position . 'left))))

(leaf use-package :ensure t)

;;
;; Initialize Evil
;;

;; set before loading evil
(defvar evil-want-keybinding nil)

(leaf evil
  :ensure t
  :custom (;; restore TAB functionality in org mode
           (evil-want-C-i-jump   . nil)
           (evil-want-fine-undo  . t)
           (evil-want-minibuffer . t))
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion (kbd "SPC")   'evil-scroll-page-down)
  (evil-global-set-key 'motion (kbd "S-SPC") 'evil-scroll-page-up)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion (kbd "C-]") 'other-window-or-split)
  ;;   (evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;   (evil-set-initial-state 'dashboard-mode 'normal))

  (leaf evil-collection
    :ensure t
    :custom ((evil-collection-setup-minibuffer . t))
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
    :bind ((:undo-tree-visualizer-mode-map
            ([remap evil-previous-visual-line] . 'undo-tree-visualize-undo)
            ([remap evil-next-visual-line]     . 'undo-tree-visualize-redo)))
    :custom ((undo-tree-auto-save-history . nil)
             (evil-undo-system . 'undo-tree))
    :config
    (global-undo-tree-mode)))

(leaf general
  :ensure t
  ;; :after evil magit
  :config
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix        ","
    :global-prefix "C-,")

  (my/leader-keys
    "SPC" 'set-mark-command
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
    "w" 'evil-window-prev
    "0" 'delete-window
    "1" 'delete-other-windows
    "2" 'split-window-vertically
    "3" 'split-window-horizontally
    "4" 'switch-to-buffer-other-window
    "5" 'split-side-window
    ";" 'comment-dwim
    "'" 'ace-window
    "," 'xref-pop-marker-stack
    "." 'xref-find-definitions
    "/" 'xref-find-references
    ))

(leaf neotree
  :ensure t
  :config
  (add-hook
   'neotree-mode-hook
   (lambda ()
     (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
     (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
     (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
     (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
     (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
     (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
     (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
     (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
     (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))))

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
(leaf *mozc
  :disabled t
  :ensure t
  :bind* ("<f2>" . toggle-input-method)
  :bind ("<f8>" . my:select-mozc-tool)
  :config
  (setq default-input-method "japanese-mozc"
        mozc-helper-program-name "mozc_emacs_helper"
        mozc-leim-title "♡かな")
  :preface
  (defadvice toggle-input-method (around toggle-input-method-around activate)
    "Input method function in key-chord.el not to be nil."
    (let ((input-method-function-save input-method-function))
      ad-do-it
      (setq input-method-function input-method-function-save)))
  (defun mozc-insert-str (str)
    "If punctuation marks, immediately confirm."
    (mozc-handle-event 'enter)
    (toggle-input-method)
    (insert str)
    (toggle-input-method))
  (add-hook 'mozc-mode-hook
            (lambda ()
              (define-key mozc-mode-map "?" #'(lambda () (interactive) (mozc-insert-str "？")))
              (define-key mozc-mode-map "," #'(lambda () (interactive) (mozc-insert-str "、")))
              (define-key mozc-mode-map "." #'(lambda () (interactive) (mozc-insert-str "。")))))
  :init
  (leaf mozc-temp
    :ensure t
    :bind* ("<henkan>" . mozc-temp-convert))
  (leaf mozc-cursor-color
    :el-get iRi-E/mozc-el-extensions
    :require t
    :config
    (setq mozc-cursor-color-alist
          '((direct . "#BD93F9")
            (read-only . "#84A0C6")
            (hiragana . "#CC3333"))))
  (leaf mozc-cand-posframe
    :ensure t
    :require t
    :config
    (setq mozc-candidate-style 'posframe)
    :init
    (leaf posframe :ensure t)))


(leaf *user-mozc-tool
  :init
  (defun my:select-mozc-tool ()
    "Narrow the only espy command in M-x."
    (interactive)
    (counsel-M-x "^my:mozc "))

  (defun my:mozc-config-dialog ()
    "Run the mozc-tool in the background."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=config_dialog")
    (delete-other-windows))

  (defun my:mozc-dictionary-tool ()
    "Run the mozc-tool in the background."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=dictionary_tool")
    (delete-other-windows))

  (defun my:mozc-word-regist ()
    "Run the mozc-tool in the background."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
    (delete-other-windows))

  (defun my:mozc-hand-writing ()
    "Run the mozc-tool in the background."
    (interactive)
    (compile "/usr/lib/mozc/mozc_tool --mode=hand_writing")
    (delete-other-windows)))

(leaf mozc
  :doc "日本語入力 https://w.atwiki.jp/ntemacs/pages/48.html"
  :require t
  :custom ((default-input-method . "japanese-mozc-im")
           (mozc-leim-title . "あ")
           (mozc-candidate-style . 'popup))
  :bind ((("C-\\"  . 'toggle-input-method)
          ("C-SPC" . 'toggle-input-method)
          ("<f2>"  . 'toggle-input-method))
         (isearch-mode-map
          :package isearch
          ("C-\\"  . 'toggle-input-method)
          ("C-SPC" . 'toggle-input-method)
          ("<f2>"  . 'toggle-input-method))
         (wdired-mode-map
          :package wdired
          ("C-\\"  . 'toggle-input-method)
          ("C-SPC" . 'toggle-input-method)
          ("<f2>"  . 'toggle-input-method)))
  :config
  (leaf mozc-im :ensure t :require t)
  (leaf mozc-popup :ensure t :require t)

  (defvar-local mozc-im-mode nil)
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
  ;; isearch を利用する前後で IME の状態を維持するための対策
  (add-hook 'isearch-mode-hook (lambda () (setq im-state mozc-im-mode)))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (unless (eq im-state mozc-im-mode)
                (if im-state
                    (activate-input-method default-input-method)
                  (deactivate-input-method)))))
  ;; wdired 終了時に IME を OFF にする
  (advice-add 'wdired-finish-edit
              :after (lambda (&rest args)
                       (deactivate-input-method)))
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
    ;; (defvar mozc-im-mode nil)
    ;; (make-variable-buffer-local 'mozc-im-mode)
    (defvar-local mozc-im-mode nil)
    (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
    (advice-add 'mozc-cursor-color-update
                :around (lambda (orig-fun &rest args)
                          (let ((mozc-mode mozc-im-mode))
                            (apply orig-fun args))))))

(leaf help
  :disabled t
  :preface
  (defun my/advice--describe-variable (fn &rest _args)
    (apply fn (variable-at-point)))
  :advice (:around describe-variable
                   my/advice--describe-variable))

(leaf mode-line
  :custom
  ((display-time-string-forms
    . '(year "-" month "-" day " " dayname " " 24-hours ":" minutes)))
  :config
  (column-number-mode)
  (display-time))

(leaf save-files
  :hook
  ((before-save-hook . delete-trailing-whitespace)
   (after-save-hook . executable-make-buffer-file-executable-if-script-p))
  :preface
  (defun execute-rsync () ;; &optional _PRED _ARG)
    "Execute .rsync script file in the current directory if exists."
    (interactive)
    (let ((rsync-file (file-name-concat default-directory ".rsync")))
      (when (file-exists-p rsync-file)
        (async-shell-command rsync-file))))

  (advice-add #'save-buffer :after #'execute-rsync)
  (add-to-list 'display-buffer-alist
               ;; '("*Async Shell Command*" display-buffer-no-window)))
               `(,shell-command-buffer-name-async display-buffer-no-window)))

;; Key settings
(global-set-key (kbd "<f5>") 'save-buffer)
;; Type backslash instead of yen mark
(define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す

(leaf startup-performance
  ;; The default is 800 kilobytes.  Measured in bytes.
  :custom (gc-cons-threshold . 50000000) ;; 50 megabytes
  :hook ((emacs-startup-hook . my/display-startup-time))
  :preface
  (defun my/display-startup-time ()
    "Report startup performance."
    (message
     "Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time)))
     gcs-done)))

(leaf text-scale
  :doc "Magnify and demagnify texts"
  :custom ((text-scale-mode-step . 1.05)) ;; 1.2
  :bind (("C-+" . text-scale-increase)
         ("C-=" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("C-0" . text-scale-reset))
  :preface
  (defun text-scale-reset ()
    "Reset the text scale."
    (interactive)
    (text-scale-increase 0)))

(leaf terminal-emacs
  :doc "settings for terminal emacs"
  :unless (display-graphic-p)
  :config
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
    :config
    (evil-terminal-cursor-changer-activate)))

(leaf tramp
  disabled t
  :config
  ;; tramp hanging
  ;; https://gongo.hatenablog.com/entry/2011/11/14/195912
  (setq vc-handled-backends ())

  ;; https://holidays-l.hatenadiary.org/entry/20101020/p1
  (defadvice tramp-handle-vc-registered
      (around tramp-handle-vc-registered-around activate)
    ;; '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)
    (let ((vc-handled-backends '(SVN Git))) ad-do-it)))

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
  '(;; (create-lockfiles . nil)
    ;; (debug-on-error . t)
    (init-file-debug . t) ;; startup
    (frame-resize-pixelwise . t)
    (enable-recursive-minibuffers . t)
    (history-length . 1000)
    (history-delete-duplicates . t)
    (scroll-preserve-screen-position . t)
    (scroll-conservatively . 100)
    (ring-bell-function . 'ignore)
    (visible-bell . nil)
    (text-quoting-style . 'straight)
    ;; (truncate-lines . t)
    ;; (use-dialog-box . nil)
    ;; (use-file-dialog . nil)
    (menu-bar-mode . nil)
    (tool-bar-mode . nil)
    ;; (scroll-bar-mode . nil) ;; scroll-bar
    (indent-tabs-mode . nil)
    (tab-width . 2)
    ;; disable to color the selected region
    (transient-mark-mode . nil))
  :config
  ;; (keyboard-translate ?\C-h ?\C-?)
  (defalias 'yes-or-no-p 'y-or-n-p))

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
  :custom (desktop-save-mode . t))

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
  ((ediff-window-setup-function . 'ediff-setup-windows-plain)
   (ediff-split-window-function . 'split-window-horizontally)))

(leaf files
  :custom (make-backup-files . nil))

(leaf frame
  :custom (blink-cursor-mode . nil))

(leaf hl-line
  :disabled t
  :global-minor-mode global-hl-line-mode)

(leaf js
  :custom (js-indent-level . 2))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0))
  :global-minor-mode show-paren-mode)

(leaf simple
  :custom ((kill-whole-line  . t)
           (next-line-add-newlines . nil)))

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

(leaf uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)))

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

(leaf consult :ensure t)

;; 標準コマンドをconsultコマンドに差し替える
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap goto-line] 'consult-goto-line)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
(defun my-consult-line (&optional at-point)
  "Consult-line thing-at-point if AT-POINT is non-nil."
  (interactive "P")
  (if at-point
      (consult-line (thing-at-point 'symbol))
    (consult-line)))

;; C-s（isearch-forward）をmy-consult-lineコマンドに割り当てる
(global-set-key (kbd "C-s") 'my-consult-line)

;; C-s/C-rで行を移動できるようにする
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "C-r") 'vertico-previous)
  (define-key vertico-map (kbd "C-s") 'vertico-next))

;; embark-consultを読み込む
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)))

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
  ((vertico-count . 20))
  :bind
  (:vertico-map
   (("C-l" . filename-upto-parent)
    ;; evil-want-minibuffer対応
    ;; C-n/p, jkで選択移動を有効化
    ([remap next-window-line] . vertico-next)
    ([remap previous-window-line] . vertico-previous)
    ([remap evil-complete-next] . vertico-next)
    ([remap evil-complete-previous] . vertico-previous)
    ([remap evil-paste-pop] . vertico-previous)
    ([remap evil-next-line] . vertico-next)
    ([remap evil-previous-line] . vertico-previous)
    ;; C-f/bで選択ページ送りを有効化
    ([remap forward-char] . vertico-scroll-up)
    ([remap backward-char] . vertico-scroll-down)
    ([remap evil-jump-forward] . vertico-scroll-up)
    ([remap evil-jump-backward] . vertico-scroll-down)
    ([remap evil-scroll-page-down] . vertico-scroll-up)
    ([remap evil-scroll-page-up] . vertico-scroll-down)
    ;; RETで選択確定を有効化
    ([remap evil-ret] . vertico-exit)
    ;; TAB, C-i, C-j: 補完
    ("C-j" . vertico-insert)
    ))
  :hook ((after-init-hook . vertico-mode)
         ;; savehist-modeを使ってVerticoの順番を永続化する
         (after-init-hook . savehist-mode)))

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
                                 #'delete)))))

;;
;; lsp: select among eglot, lsp-bridge and lsp-mode
;;

(leaf python-mode
  :ensure t
  :custom (python-indent-guess-indent-offset-verbose . nil))

(leaf pyvenv
  :disabled t
  :ensure t
  :after python-mode
  :config
  (pyvenv-mode 1))

;; Note: pyrightconfig.json is required for venv
(leaf eglot
  :disabled nil
  :ensure t
  :hook ((python-mode-hook . eglot-ensure))
  :custom ((eldoc-echo-area-use-multiline-p . nil)))

;; Note: disable company, corfu
(leaf lsp-bridge
  :disabled t
  :config
  (defvar-local root_dir "/home/umemoto/distfiles")
  (defvar-local lsp-bridge-path (file-name-concat root_dir "lsp-bridge"))
  (when (file-directory-p lsp-bridge-path)
    (add-to-list 'load-path lsp-bridge-path)
    (require 'yasnippet)
    (yas-global-mode 1)
    (require 'lsp-bridge)
    (global-lsp-bridge-mode)
    (define-key acm-mode-map (kbd "RET") 'newline))

  (unless (display-graphic-p)
    (defvar-local acm-terminal-path (file-name-concat root_dir, "acm-terminal"))
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

;; Note: work with corfu, not company
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
    :custom (lsp-pyright-venv-path . "/home/umemoto")
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
  :ensure t
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :config
  (leaf flymake-diagnostic-at-point
    :ensure t
    :after flymake
    :hook (flymake-mode-hook)))

(leaf git-gutter
  :ensure t
  :global-minor-mode global-git-gutter-mode)

(leaf go-translate
  :ensure t
  :bind (("C-c t" . gts-do-translate))
  :custom ((gts-translate-list . '(("en" "ja") ("ja" "en"))))
  :defvar (gts-default-translator gts-translate-list)
  :preface
  (defun gts-do-translate-zh-ja ()
    "Translate zh to ja."
    (interactive)
    (let ((tlist gts-translate-list))
      (setq gts-translate-list '(("zh" "ja")))
      (gts-do-translate)
      (setq gts-translate-list tlist)))
  :config
  (setq gts-default-translator
	      (gts-translator
	       :picker (gts-noprompt-picker)
	       :engines (list
		               ;; (gts-deepl-engine
                   ;;  :auth-key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx:xx" :pro nil) ;; CHANGEME
		               (gts-google-engine)
		               (gts-bing-engine))
 	       :render (gts-buffer-render))))

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
  :config
  (define-key magit-mode-map (kbd "SPC") 'evil-scroll-page-down)
  (define-key magit-mode-map
    [remap magit-diff-show-or-scroll-up] 'evil-scroll-page-up))

(leaf minions
  :ensure t
  :custom ((minions-mode-line-lighter . "[+]"))
  :config
  (minions-mode))

(leaf modus-themes
  :emacs< 28
  :ensure t
  :bind (("<f5>" . modus-themes-toggle))
  :custom
  ((modus-themes-bold-constructs   . nil)
   (modus-themes-italic-constructs . t)
   (modus-themes-region            . '(bg-only no-extend)))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

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

(leaf paradox
  :doc "wrapper of package.el"
  :ensure t
  :config
  (paradox-enable))

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
  :disabled t
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

(leaf tree-sitter :ensure t :leaf-defer t)

(leaf tree-sitter-langs :ensure t :leaf-defer t)

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
  :config
  (add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))
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

;;
;; Profiler (end)
;;
(when my/enable-profiler
  (profiler-report)
  (profiler-stop))

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; indent-tabs-mode: nil
;; End:
