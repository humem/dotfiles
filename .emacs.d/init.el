;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Faster startup (start)
(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Load private settings
(let ((my-private (locate-user-emacs-file "my-private.el")))
  (when (file-exists-p my-private)
    (load my-private)))

(defvar my/dynabook-p nil)
(when my/dynabook-p
  (defvar my/battery t)
  (defvar my/modus-themes 'modus-vivendi)
  (defvar my/accent-modus-themes t)
  (defvar my/skip t))

;; Setup tracker
(defvar my/enable-setup-tracker t)
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

;; Initialize local emacs-lisp repository
;; https://qiita.com/tadsan/items/431899f76f3765892abd
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (when (file-exists-p default-directory)
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

;; Define utilities
(defun wsl-p ()
  "Return t if running on WSL."
  (/= (length (getenv "WSL_DISTRO_NAME")) 0))

;; Prevent custom from updating init file
(customize-set-variable 'custom-file
                        (locate-user-emacs-file "custom.el"))

;;
;; Initialize leaf package manager
;;

(eval-and-compile
  (customize-set-variable
   'package-archives
   '(("gnu"    . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa"  . "https://melpa.org/packages/")
     ("org"    . "https://orgmode.org/elpa/")))

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
  :hook (image-minor-mode-hook . turn-off-evil-mode)
  :defun evil-global-set-key evil-set-initial-state
  :config
  (evil-global-set-key 'motion (kbd "SPC")   'evil-scroll-page-down)
  (evil-global-set-key 'motion (kbd "S-SPC") 'evil-scroll-page-up)
  (evil-global-set-key 'motion (kbd "<home>") 'evil-goto-first-line)
  (evil-global-set-key 'motion (kbd "<end>") 'evil-goto-line)
  (evil-global-set-key 'motion (kbd "C-]") 'other-window-or-split)
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'messages-buffer-mode 'normal)

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

;;
;; Personal enhancements
;;

(defvar my/fonts-family "HackGen35Nerd Console")
(defvar my/fonts-height 120)
(leaf japanese
  :init
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
  :config
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
  (when (wsl-p)
    (advice-add
     'mozc-session-execute-command
     :after (lambda (&rest args)
              (when (eq (nth 0 args) 'CreateSession)
                ;; (mozc-session-sendkey '(hiragana)))))
                (mozc-session-sendkey '(Hankaku/Zenkaku)))))))

(leaf mozc-im :ensure t :require t)

(leaf mozc-popup :ensure t :require t)

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
                          (apply orig-fun args)))))

(defvar my/battery nil)
(leaf battery
  :if my/battery
  :init
  (defun battery-linux-sysfs-wsl ()
    "Get ACPI status information from Linux kernel.
This function works only with the Windows Subsystem for Linux.

The following %-sequences are provided:
%c Current capacity (mAh)
%r Current rate
%B Battery status (verbose)
%b Battery status (charging:'+' discharging:'')
%d Temperature (in degrees Celsius)
%p Battery load percentage
%L AC line status (verbose)
%m Remaining time (to charge or discharge) in minutes
%h Remaining time (to charge or discharge) in hours
%t Remaining time (to charge or discharge) in the form `h:min'"
    (let (charging-state
          ac-state
          temperature
          hours
          energy-now
          energy-now-rate
          power-now
          current-now
          voltage-now
          (dir "/sys/class/power_supply/battery"))
      (with-temp-buffer
        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "capacity" dir)))
        (setq energy-now-rate (or (thing-at-point 'number) "N/A"))

        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "status" dir)))
        (setq charging-state (or (thing-at-point 'word) "N/A"))

        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "temp" dir)))
        (setq temperature (or (thing-at-point 'number) "N/A"))
        (setq temperature (if (numberp temperature) (* temperature 0.1)))

        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "charge_counter" dir)))
        (setq energy-now (or (thing-at-point 'number) "N/A"))

        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "current_now" dir)))
        (setq current-now (or (thing-at-point 'number) "N/A"))
        (unless (or   (stringp energy-now) (stringp current-now)
                      (stringp energy-now-rate) (zerop current-now))
          (if (string= charging-state "Discharging")
              (setq hours (/ energy-now current-now))
            (setq hours (/ (* energy-now (- 100.0 energy-now-rate))
                           energy-now-rate current-now ))))

        (erase-buffer)
        (ignore-errors (insert-file-contents
                        (expand-file-name "voltage_now" dir)))
        (setq voltage-now (or (thing-at-point 'number) "N/A"))
        (setq power-now (if (and (numberp current-now) (numberp voltage-now))
                            (* (/ current-now 1000.0) (/ voltage-now 1000000.0))))
        ;; current-now[mA]->[A] voltage-now[uV]->[V]

        (erase-buffer)
        (setq dir "/sys/class/power_supply/ac")
        (ignore-errors (insert-file-contents
                        (expand-file-name "online" dir)))
        (setq ac-state (cond ((eq (thing-at-point 'number) 1) "AC")
                             ((eq (thing-at-point 'number) 0) "BAT")
                             (t "N/A")))
        )
      ;; set return value
      (list (cons ?c (number-to-string energy-now))
            (cons ?r (if hours (number-to-string power-now) "N/A"))
            (cons ?B charging-state)
            (cons ?b (if (string= charging-state "Charging") "+" ""))
            (cons ?d (number-to-string temperature))
            (cons ?p (number-to-string energy-now-rate))
            (cons ?L ac-state)
            (cons ?m (if hours (format "%d" (* hours 60)) "N/A"))
            (cons ?h (if hours (format "%d" hours) "N/A"))
            (cons ?t (if hours (format "%d:%02d" hours
                                       (* (- hours (floor hours)) 60)) "N/A")))))
  (when (wsl-p)
    (defvar battery-status-function)
    (declare-function battery-linux-sysfs-wsl "init")
    (setq battery-status-function #'battery-linux-sysfs-wsl))
  (display-battery-mode 1))

(leaf mode-line
  :custom
  ((display-time-string-forms
    . '(year "-" month "-" day " " dayname " " 24-hours ":" minutes)))
  :init
  (column-number-mode)
  (display-time)
  (setq frame-title-format
        '(multiple-frames
          "%b" ("" "%b - GNU Emacs at " system-name " " display-time-string))))

;; ;; Type backslash instead of yen mark
;; (define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す

(leaf profiler)
;; (profiler-start 'cpu)
;; ...
;; (profiler-report)
;; (profiler-stop)

(leaf startup-performance
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

(defvar my/use-xsel nil)
(leaf terminal-emacs
  :doc "settings for terminal emacs"
  :unless (display-graphic-p)
  :init
  ;; Mouse scrolling in terminal emacs
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)
  ;; no blink cursor
  (customize-set-variable 'visible-cursor nil)
  ;; scroll bar
  ;(global-yascroll-bar-mode 1)
  ;(setq yascroll:delay-to-hide nil)
  ;; sync with x clipboard
  (when (file-exists-p "/usr/bin/xsel")
    (defvar env-display "")
    (when (getenv "TMUX")
      (setq env-display "env $(tmux showenv DISPLAY) "))

    (when (or (wsl-p) my/use-xsel)
      (setq interprogram-cut-function   'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function))
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
        (setq interprogram-paste-function 'xsel-paste-function)))))

(leaf evil-terminal-cursor-changer
  :doc "Change cursor shape and color by evil state in terminal"
  :ensure t
  :defun evil-terminal-cursor-changer-activate
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

(leaf web-browser-for-wsl
  :if (wsl-p)
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
    ;; (garbage-collection-messages . t)
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
  :custom (auto-revert-verbose . nil)
  :global-minor-mode global-auto-revert-mode)

(leaf bytecomp
  :custom
   (byte-compile-warnings . '(not cl-functions obsolete)))

(leaf dabbrev
  :custom (dabbrev-case-fold-search . nil))

(leaf desktop
  :custom
  ((desktop-files-not-to-save
    . "\\(\\`/[^/:]*:\\|(ftp)\\'\\|\\.gz\\'\\|\\.jpg\\'\\|\\.png\\'|\\.tif\\'\\)")
   (desktop-lazy-verbose . nil))
   ;; (desktop-restore-eager . 0))
  :config
  (desktop-save-mode 1))

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
  :custom ((enable-local-variables . :safe)
           (find-file-visit-truename . t)
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

(leaf recentf
  :custom
  ((recentf-exclude . '("\\.gz\\'" "\\.jpg\\'" "\\.png\\'" "\\.tif\\'"))
   (recentf-max-saved-items . 100))
  :global-minor-mode recentf-mode)

(leaf simple
  :custom ((kill-whole-line  . t)
           (next-line-add-newlines . nil))
  :global-minor-mode global-visual-line-mode
  :hook
  ((dired-mode-hook
    imenu-list-major-mode-hook
    magit-status-mode-hook
    neotree-mode-hook
    package-menu-mode-hook
    paradox-menu-mode-hook
    yaml-mode-hook)
   . (lambda () (progn
                  (visual-line-mode -1)
                  (toggle-truncate-lines +1)))))

(leaf tab-bar
  :global-minor-mode tab-bar-mode)

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

(defvar my/accent-modus-themes nil)
(when my/accent-modus-themes
  (defvar my/modus-themes-region '(accented no-extend))
  (custom-set-faces
   '(fill-column-indicator ((t (:foreground "dim gray"))))
   ;; '(mode-line-inactive ((t (:box (:line-width (1 . 1) :color "dim gray"))))))
   '(mode-line-inactive ((t (:background "gray30"))))
   '(mode-line ((t (:background "gray35")))))
  (custom-set-variables
   '(highlight-indent-guides-auto-character-face-perc 30)
   '(highlight-indent-guides-auto-top-character-face-perc 50))
  )
(defvar my/modus-themes 'modus-operandi) ;; modus-vivendi
(defvar my/modus-themes-region '(bg-only no-extend)) ;; accented

(leaf themes/modus-themes
  :doc "highly accessible and customizable themes"
  :emacs>= 28
  :custom (modus-themes-region . my/modus-themes-region)
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
;; lsp: eglot lsp-bridge lsp-mode
;;

(defvar my/lsp 'lsp-mode)
(defvar my/lsp-rename
  (cond ((eq my/lsp 'eglot) 'eglot-rename)
        ((eq my/lsp 'lsp-mode) 'lsp-rename)
        (t nil)))

(leaf eglot
  :defvar my/lsp
  :disabled (not (eq my/lsp 'eglot))
  :ensure t
  :hook ((python-mode-hook . eglot-ensure))
  :custom ((eldoc-echo-area-use-multiline-p . nil)))

;; Note: company and corfu should be disabled; acm is used
(leaf lsp-bridge
  :disabled (not (eq my/lsp 'lsp-bridge))
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
  :disabled (not (eq my/lsp 'lsp-mode))
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
  (when (display-graphic-p)
    (customize-set-variable
     'lsp-headerline-breadcrumb-icons-enable nil))
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
    :hook (python-mode-hook . lsp-deferred))

  (leaf dap-mode
    :ensure t
    :commands dap-debug
    ;; :custom
    ;; (lsp-enable-dap-auto-configure . nil)
    :config
    (unless (display-graphic-p)

      (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))
      ;; (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features))
    (dap-ui-mode 1)
    (require 'dap-node)
    (dap-node-setup)
    (require 'dap-python)
    (setq dap-python-debugger 'debugpy)
    (general-define-key
     :keymaps 'lsp-mode-map :prefix lsp-keymap-prefix "d"
     '(dap-hydra t :wk "debugger"))
    (unless (display-graphic-p)
      ;;  An orange background for the line to execute
      (set-face-background 'dap-ui-marker-face "orange") ;; "color-166")
      ;; Do not inherit other styles
      (set-face-attribute 'dap-ui-marker-face nil :inherit nil)
      ;; Blue background for breakpoints line
      (set-face-background 'dap-ui-pending-breakpoint-face "light blue")
      (set-face-attribute 'dap-ui-verified-breakpoint-face nil
                          :inherit 'dap-ui-pending-breakpoint-face))))

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
  :disabled (eq my/lsp 'lsp-bridge)
  :ensure t
  :custom (corfu-auto . t)
  :global-minor-mode global-corfu-mode
  :config
  (leaf corfu-terminal
    :ensure t
    :config
    (unless (display-graphic-p)
      (corfu-terminal-mode +1))))

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
  :custom (completion-styles . '(orderless)))

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
;; Keybindings
;;

(leaf general
  :ensure t
  :require t
  :config
  (declare-function my/leader-keys "init")
  (general-create-definer my/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,")
  `(my/leader-keys
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
     "r" ',my/lsp-rename
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
  :custom (neo-show-hidden-files . t)
  :defvar neotree-mode-map
  :config
  (declare-function evil-define-key "evil-core")
  (evil-define-key 'normal neotree-mode-map
    "n" 'neotree-next-line
    "p" 'neotree-previous-line
    "q" 'neotree-hide
    "r" 'neotree-refresh
    "v" 'neotree-quick-look
    "A" 'neotree-stretch-toggle
    "H" 'neotree-hidden-file-toggle))

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
   ("M-g M-g" . avy-goto-line)))

(leaf avy-zap
  :ensure t
  :bind
  ([remap zap-to-char] . avy-zap-to-char))

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

(leaf expand-region :ensure t)

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
  :global-minor-mode gcmh-mode
  :defvar gcmh-verbose
  :preface
  (defun my/gc-debug-function (str)
  "Display the memory size used after garbage coolection."
    (let ((sum 0))
      (dolist (x str)
        (setq sum (+ sum (* (cl-second x) (cl-third x)))))
      (when gcmh-verbose
        (message "Used Memory: %d MB" (/ sum (* 1024 1024))))))
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
  (if my/deepl-auth-key
      (setq gts-default-translator
	          (gts-translator
	           :picker (gts-noprompt-picker)
	           :engines (list
                       (gts-deepl-engine :auth-key my/deepl-auth-key :pro nil)
                       (gts-google-engine) (gts-bing-engine))
             :render (gts-buffer-render)))
    (setq gts-default-translator
	        (gts-translator
	         :picker (gts-noprompt-picker)
	         :engines (list
                     (gts-google-engine) (gts-bing-engine))
           :render (gts-buffer-render)))
    ))

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
(leaf org-modern :ensure t)
  ;; :global-minor-mode global-org-modern-mode)

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

(leaf python
  ;; pip install black debugpy isort pyright
  :custom (display-fill-column-indicator-column . 79)
  :hook (python-mode-hook . display-fill-column-indicator-mode)
  :config
  (leaf python-mode
    :ensure t
    :custom (python-indent-guess-indent-offset-verbose . nil))

  (leaf blacken
    :ensure t
    :custom ((blacken-line-length . 79)
             (blacken-skip-string-normalization . t)))

  (leaf py-isort :ensure t)

  (leaf pyvenv
    :disabled t
    :ensure t
    :hook (python-mode-hook . pyvenv-mode)))

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

(leaf typescript-mode
  :ensure t
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  :custom (typescript-indent-level . 2)
  :mode (("\\.tsx\\'" . typescript-tsx-mode)))

(leaf yaml-mode :ensure t)

(leaf yasnippet :ensure t)
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

(leaf yascroll
  :ensure t
  :custom ((yascroll:delay-to-hide . 3.0)
           (yascroll:disabled-modes . '(dashboard-mode image-mode)))
  :config
  (unless (display-graphic-p)
    (global-yascroll-bar-mode 1)))

;;
;; Skip heavy packages
;;

(defvar my/skip nil)
(unless my/skip

  (leaf all-the-icons
    :ensure t
    :if (display-graphic-p)
    :config
    (unless (file-exists-p
             (file-name-concat
              (getenv "HOME") ".local/share/fonts/all-the-icons.ttf"))
      (all-the-icons-install-fonts))
    (with-eval-after-load 'neotree
      (defvar neo-theme)
      (setq neo-theme 'icons)))

  (leaf all-the-icons-dired
    :ensure t
    :after all-the-icons
    :hook (dired-mode-hook
           . (lambda ()
               (if (display-graphic-p)
                   (all-the-icons-dired-mode +1)
                 (all-the-icons-dired-mode -1)))))

  (leaf avy-migemo
    :el-get momomo5717/avy-migemo
    :after migeo
    :global-minor-mode avy-migemo-mode)

  (leaf dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook))

  (leaf el-get :ensure t)

  ;; TODO
  (leaf flymake-aspell
    :ensure t
    :after flymake
    :hook (text-mode-hook . flymake-aspell-setup))

  ;; TODO
  (leaf flymake-textlint
    :el-get iquiw/flymake-textlint
    :hook ((markdown-mode-hook
            org-mode-hook
            text-mode-hook)
           . flymake-textlint-setup))

  (leaf kind-icon
    :ensure t
    :after corfu
    :defvar corfu-margin-formatters
    :custom
    (kind-icon-default-face . 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; cmigemo is required
  (leaf migemo
    :ensure t
    :require t
    :if (executable-find "cmigemo")
    :custom
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
    :defun migemo-init
    :config
    (migemo-init))

  (leaf orderless
    :ensure t
    :require t
    :after migemo
    :defun migemo-get-pattern
    :preface
    (defun orderless-migemo (component)
      "Use migemo with orderless."
      (let ((pattern (migemo-get-pattern component)))
	      (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
    :defun orderless-define-completion-style orderless-matching-styles
    :defvar (orderless-matching-styles
             orderless-default-style
             orderless-migemo-style)
    :config
    (orderless-define-completion-style orderless-default-style
      (orderless-matching-styles '(orderless-initialism
				                           orderless-literal
				                           orderless-regexp)))
    (orderless-define-completion-style orderless-migemo-style
      (orderless-matching-styles '(orderless-initialism
				                           orderless-literal
				                           orderless-regexp
				                           orderless-migemo)))
    (defvar completion-category-overrides)
    (setq completion-category-overrides
          '((command (styles orderless-default-style))
            (file (styles orderless-migemo-style))
            (buffer (styles orderless-migemo-style))
            (symbol (styles orderless-default-style))
            (consult-location (styles orderless-migemo-style))
            (consult-multi (styles orderless-migemo-style))
            (org-roam-node (styles orderless-migemo-style))
            (unicode-name (styles orderless-migemo-style))
            (variable (styles orderless-default-style))))
    (setq orderless-matching-styles
          '(orderless-literal orderless-regexp orderless-migemo))
    :custom
    (completion-styles . '(orderless basic)))

  (leaf paradox
    :doc "wrapper of package.el"
    :ensure t
    :config
    (let ((inhibit-message t)
          (message-log-max nil))
      (paradox-enable)))

  (leaf pixel-scroll
    :global-minor-mode pixel-scroll-mode)

  (leaf tree-sitter
    :ensure t
    :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
    :global-minor-mode global-tree-sitter-mode
    :config
    (leaf tree-sitter-langs :ensure t))

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
  )

;; Faster startup (end)
(setq file-name-handler-alist my-saved-file-name-handler-alist)
(setq gc-cons-threshold 16777216) ; 16mb

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; indent-tabs-mode: nil
;; End:
