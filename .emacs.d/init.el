;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Local emacs-lisp repository
;; https://qiita.com/tadsan/items/431899f76f3765892abd
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; Japanese language environment
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setenv "LANG" "C.UTF-8")
;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
(setq default-process-coding-system '(undecided-dos . utf-8-unix))
;; ※ 設定値の car を "undecided-dos" にしておくと、Windows コマンドの出力にも柔軟に対応できます。関連して 29) の説明も参照してください。

;; https://github.com/conao3/leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

(leaf transient-dwim
  :ensure t
  :bind (("M-=" . transient-dwim-dispatch)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :custom ((dabbrev-case-fold-search . nil)
           (indent-tabs-mode . nil)
           (kill-whole-line . t)
           (menu-bar-mode . nil)
           (next-line-add-newlines . nil)
           (make-backup-files . nil)
           (tool-bar-mode . nil)
           ;; Disable to color the selected region
           (transient-mark-mode . nil)
           (visible-bell . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
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

(leaf eglot
  :ensure t
  :hook ((python-mode-hook . eglot-ensure))
  :require t
  :custom ((eldoc-echo-area-use-multiline-p . nil))
  :config
  (add-to-list 'eglot-server-programs '(python-mode "pyls")))

(leaf flycheck
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

(leaf flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode-hook)
  :require t)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0))
  :global-minor-mode show-paren-mode)

(leaf use-package :ensure t :require t)

;; ein (Emacs in Notebook)
;; https://tam5917.hatenablog.com/entry/2021/03/28/204747
(eval-when-compile
  (require 'ein)
  (require 'ein-notebook)
  (require 'ein-notebooklist))
  ;(require 'ein-markdown-mode)
  ;(require 'smartrep))
;; (add-hook 'ein:notebook-mode-hook 'electric-pair-mode) ;; お好みで
;; (add-hook 'ein:notebook-mode-hook 'undo-tree-mode) ;; お好みで
;; undoを有効化 (customizeから設定しておいたほうが良さげ)
(setq ein:worksheet-enable-undo t)
;; 画像をインライン表示 (customizeから設定しておいたほうが良さげ)
(setq ein:output-area-inlined-images t)
;; Start jupyter notebook
;; M-x ein:login

;; 日本語入力 emacs-mozc https://w.atwiki.jp/ntemacs/pages/48.html
(require 'mozc-im)
(require 'mozc-popup)
(require 'mozc-cursor-color)
(require 'wdired)
(setq default-input-method "japanese-mozc-im")
;; popupスタイル を使用する
(setq mozc-candidate-style 'popup)
;; カーソルカラーを設定する
(setq mozc-cursor-color-alist '((direct        . "white")
                                (read-only     . "yellow")
                                (hiragana      . "light green")
                                (full-katakana . "goldenrod")
                                (half-ascii    . "dark orchid")
                                (full-ascii    . "orchid")
                                (half-katakana . "dark goldenrod")))
;; C-\ で IME をトグルする
(global-set-key (kbd "C-\\") 'toggle-input-method)
(define-key isearch-mode-map (kbd "C-\\") 'isearch-toggle-input-method)
(define-key wdired-mode-map (kbd "C-\\") 'toggle-input-method)
;; C-SPC で IME をトグルする
(global-set-key (kbd "C-SPC") 'toggle-input-method)
(define-key isearch-mode-map (kbd "C-SPC") 'isearch-toggle-input-method)
(define-key wdired-mode-map (kbd "C-SPC") 'toggle-input-method)
;; F2 で IME をトグルする
(global-set-key (kbd "<f2>") 'toggle-input-method)
(define-key isearch-mode-map (kbd "<f2>") 'isearch-toggle-input-method)
(define-key wdired-mode-map (kbd "<f2>") 'toggle-input-method)
;; mozc-cursor-color を利用するための対策
;; (defvar mozc-im-mode nil)
;; (make-variable-buffer-local 'mozc-im-mode)
(defvar-local mozc-im-mode nil)
(add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
(add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
(advice-add 'mozc-cursor-color-update
            :around (lambda (orig-fun &rest args)
                      (let ((mozc-mode mozc-im-mode))
                        (apply orig-fun args))))
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
;; Windows の mozc では、セッション接続直後 directモード になるので hiraganaモード にする
(advice-add 'mozc-session-execute-command
            :after (lambda (&rest args)
                     (when (eq (nth 0 args) 'CreateSession)
                       ;; (mozc-session-sendkey '(hiragana)))))
                       (mozc-session-sendkey '(Hankaku/Zenkaku)))))
;; デフォルト フォント
(set-face-attribute 'default nil :family "HackGen35Nerd Console")

;; Appearances
;; カーソルの点滅を OFF にする
;; (blink-cursor-mode 0)
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))
(display-time)
;; 対応する括弧を自動的に挿入する
;; (electric-pair-mode 1)
;;行番号の表示
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d ")
    ))
;; 現在行をハイライト: http://keisanbutsuriya.blog.fc2.com/blog-entry-91.html
(global-hl-line-mode t)
;; フレームの高さを補正する設定
(defun reset-frame-parameter (frame)
  (sleep-for 0.1)
  (set-frame-parameter frame 'height 32))
(add-hook 'after-make-frame-functions #'reset-frame-parameter)
;; smooth mouse scroll
(setq mouse-wheel-scroll-amount '(1))
;; M-x whitespace-mode: 空白や改行を表示する

;; Saving Emacs Sessions
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)
;; Split window
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1)
  ;; IMEをオフにする
  (when (fboundp 'mac-toggle-input-method)
    (mac-toggle-input-method nil)))
(global-set-key (kbd "C-;") 'other-window-or-split)
(global-set-key (kbd "<f8>") 'other-window-or-split)
;; tramp hanging
;; https://gongo.hatenablog.com/entry/2011/11/14/195912
(setq vc-handled-backends ())

;; Save files
;; 行末の空白を自動的に削除して保存
;; https://tototoshi.hatenablog.com/entry/20101202/1291289625
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Auto chmod+x for scripts
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Dired
(setq dired-dwim-target t)
(setq dired-use-ls-dired t)
(defvar suffix-for-open-list
  '(app csv dmg doc docx jpg htm html pdf pkg ppg ppt pptx rtf svg tif tiff xdw xls xlsx))
(let ((alist ()))
  (setq dired-guess-shell-alist-user
        (dolist (suffix suffix-for-open-list alist)
          (push (list (concat "\\." (symbol-name suffix)) "open") alist))))
;;; wdired.el
(require 'dired-x)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(flycheck-emacs-lisp-initialize-packages t t)
 '(imenu-list-position 'left t)
 '(imenu-list-size 30 t)
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(package-archives
   '(("org" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")
     ("gnu" . "https://elpa.gnu.org/packages/")))
 '(package-selected-packages
   '(flycheck-elsa flycheck-package evil-collection embark-consult consult embark marginalia orderless vertico yaml-mode neotree jsonnet-mode undo-tree mozc mozc-im mozc-popup evil-surround csv-mode clojure-mode typescript typescript-mode evil-magit web-mode powerline popwin matlab-mode markdown-mode magit lua-mode helm exec-path-from-shell evil-leader ess dockerfile-mode cmake-mode auto-complete))
 '(safe-local-variable-values '((checkdoc-minor-mode . t) (mangle-whitespace . t)))
 '(tool-bar-mode nil))

;; Key settings
;; Shortcuts
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cf" 'font-lock-fontify-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cl" 'what-line)
(global-set-key "\M-n" 'linum-mode)
;; save-buffer
(global-set-key (kbd "<f5>") 'save-buffer)
;; Type backslash instead of yen mark
(define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す
;; Magnify and demagnify texts.
(setq text-scale-mode-step 1.05); 1.2
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

;; https://www.ncaq.net/2020/05/13/21/16/35/
;; (require 'cl) を見逃す
(setq byte-compile-warnings '(not cl-functions obsolete))
;; (require 'cl)
(require 'cl-lib)

;; http://qiita.com/hottestseason/items/1e8a46ad1ebcf7d0e11c
(defvar installed-package-list
  '(
    cmake-mode
    csv-mode
    dockerfile-mode
    ess
    evil
    evil-leader
    evil-surround
    exec-path-from-shell
    helm
    julia-mode
    jsonnet-mode
    lua-mode
    magit
    markdown-mode
    matlab-mode
    mozc
    mozc-im
    mozc-popup
    neotree
    powerline
    popwin
    typescript-mode
    undo-tree
    web-mode
    yaml-mode
    ))
(let ((not-installed (cl-loop for x in installed-package-list
                           when (not (package-installed-p x))
                           collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))

;; evil-collection
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))
;; Use "a" instead of "v" in dired
(define-key dired-mode-map [remap dired-find-alternate-file] 'dired-view-file)
;; dired: "go": dired-view-file, not "v"
;; magit: "gr": magit-refresh, not "g"

;; evil-leader
;; http://stackoverflow.com/questions/8483182/evil-mode-best-practice
(global-evil-leader-mode)
;; Note: You should enable `global-evil-leader-mode' before you enable
;;       `evil-mode', otherwise `evil-leader' won't be enabled in initial
;;       buffers (*scratch*, *Messages*, ...).
(evil-leader/set-leader ",")
(evil-leader/set-key
  "b" 'switch-to-buffer
  "d" 'dired
  "f" 'find-file
  "j" 'dired-jump
  "k" 'kill-buffer
  "q" 'quit-windows
  "s" 'save-buffer
  "x" 'execute-extended-command
  "w" 'evil-window-prev
  "SPC" 'set-mark-command
  "0" 'delete-window
  "1" 'delete-other-windows
  "2" 'split-window-vertically
  "3" 'split-window-horizontally
  "4" 'switch-to-buffer-other-window
  )

;; evil-mode
;; https://lists.ourproject.org/pipermail/implementations-list/2011-September/001140.html
(evil-mode 1)
(define-key evil-motion-state-map " " 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "S-SPC") 'evil-scroll-page-up)
(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'evil-end-of-line)
;; https://teratail.com/questions/126355
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
;; 物理行移動
;; g j: evil-next-visual-line
;; g k: evil-previous-visual-line

;; ミニバッファでEvilを有効化
(setq evil-want-minibuffer t)
(setq evil-want-fine-undo t)     ;操作を元に戻す単位を細かくする

;; ノーマルステートになったら IME をオフにする
;; http://ichiroc.hatenablog.com/entry/2013/09/06/075832
(when (fboundp 'mac-toggle-input-method)
  (add-hook 'evil-normal-state-entry-hook
            '(lambda ()
               (mac-toggle-input-method nil))))

;; evil-surround
(global-evil-surround-mode 1)
;; https://blog.3qe.us/entry/2020/05/12/012958
;; b B r a: ) } ] >
;; yswb: 単語を丸括弧で囲む
;; csbB: 丸括弧を大括弧に書き換える
;; dsb: 丸括弧を削除する
;; v Sb: 選択文字列を丸括弧で囲む
;; v Sf: 選択文字列を関数形式で囲む

;; auto-complete; http://cx4a.org/software/auto-complete/manual.ja.html
;; (require 'auto-complete-config)
;; (ac-config-default)
;; (setq ac-use-menu-map t) ; C-n, C-p

;; ediff
;; http://qiita.com/l3msh0@github/items/97909d6e2c92af3acc00
;; コントロール用のバッファを同一フレーム内に表示
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのバッファを上下ではなく左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)

;; JavaScript
(setq js-indent-level 2)
(setq-default tab-width 2)

;; NeoTree with evil mode
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
            (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
            (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))

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
;; http://ytsk.hatenablog.com/entry/2015/09/23/021856
(setq ns-use-srgb-colorspace nil)
(powerline-center-evil-theme)
;; fix for 24.4
;; https://github.com/milkypostman/powerline/issues/58
;; (add-hook 'desktop-after-read-hook 'powerline-reset)

;; typescript-mode
(add-hook 'typescript-mode-hook (lambda () (setq typescript-indent-level 2)))

;; uniquify
;; http://www.clear-code.com/blog/2012/3/20.html
;;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.?html$" . web-mode))
(setq web-mode-engines-alist '(("\\.xhtml$" . "smarty")))
;; http://qiita.com/kwappa/items/6bde1fe2bbeedc85023e
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq web-mode-script-padding 0)
             (setq web-mode-style-padding 0)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             ))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#3F3F3F"))))
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


;; GNU Emacs 27.1
;; https://ubuntuhandbook.org/index.php/2020/09/install-emacs-27-1-ppa-ubuntu-20-04/
;; sudo add-apt-repository ppa:kelleyk/emacs
;; sudo apt update
;; sudo apt upgrade
;; sudo apt remove --autoremove emacs-common # remove Emacs 26.3 first
;; sudo apt install emacs27

;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/
;; vertico
;; consult
;; marginalia
;; orderless
;; embark
;; embark-consult
;; 補完スタイルにorderlessを利用する
(with-eval-after-load 'orderless
  (setq completion-styles '(orderless)))

;; 補完候補を最大20行まで表示する
(setq vertico-count 20)

;; vertico-modeとmarginalia-modeを有効化する
(defun minibuffer-after-init-hook ()
  (vertico-mode)
  ;; evil-want-minibuffer対応
  ;; 選択中にC-n/pを有効化
  (define-key vertico-map [remap next-window-line] #'vertico-next)
  (define-key vertico-map [remap previous-window-line] #'vertico-previous)
  (define-key vertico-map [remap evil-complete-next] #'vertico-next)
  (define-key vertico-map [remap evil-complete-previous] #'vertico-previous)
  (define-key vertico-map [remap evil-paste-pop] #'vertico-previous)
  ;; 選択中にjkを有効化
  (define-key vertico-map [remap evil-next-line] #'vertico-next)
  (define-key vertico-map [remap evil-previous-line] #'vertico-previous)
  ;; C-i: TAB補完
  ;; C-f/bでページ送りを有効化
  (define-key vertico-map [remap forward-char] #'vertico-scroll-up)
  (define-key vertico-map [remap backward-char] #'vertico-scroll-down)
  (define-key vertico-map [remap evil-jump-forward] #'vertico-scroll-up)
  (define-key vertico-map [remap evil-jump-backward] #'vertico-scroll-down)
  (define-key vertico-map [remap evil-scroll-page-down] #'vertico-scroll-up)
  (define-key vertico-map [remap evil-scroll-page-up] #'vertico-scroll-down)
  ;; RETを有効化
  (define-key vertico-map [remap evil-ret] #'vertico-exit)

  (marginalia-mode)
  ;; savehist-modeを使ってVerticoの順番を永続化する
  (savehist-mode))
(add-hook 'after-init-hook #'minibuffer-after-init-hook)

;; 標準コマンドをconsultコマンドに差し替える
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap goto-line] 'consult-goto-line)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; C-uを付けるとカーソル位置の文字列を使うmy-consult-lineコマンドを定義する
(defun my-consult-line (&optional at-point)
  "Consult-line uses things-at-point if set C-u prefix."
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

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; indent-tabs-mode: nil
;; End:
