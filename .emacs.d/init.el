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
(menu-bar-mode -1)
(display-time)
(setq ring-bell-function
      (lambda ()
        (invert-face 'mode-line)
        (run-with-timer 0.1 nil 'invert-face 'mode-line)))
(setq visible-bell nil)
;; 対応する括弧を光らせる
(setq show-paren-delay 0)
(setq show-paren-style 'single)
(show-paren-mode t)
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
;; ツールバーを非表示
(tool-bar-mode 0)
;; フレームの高さを補正する設定
(defun reset-frame-parameter (frame)
  (sleep-for 0.1)
  (set-frame-parameter frame 'height 32))
(add-hook 'after-make-frame-functions #'reset-frame-parameter)
;; smooth mouse scroll
(setq mouse-wheel-scroll-amount '(1))
;; M-x whitespace-mode: 空白や改行を表示する

;; Personal preferences
(setq dabbrev-case-fold-search nil)
(setq kill-whole-line t)
(setq next-line-add-newlines nil)
(setq truncate-partial-width-windows nil)
;; Set tab width
(setq default-tab-width 4)
(add-hook 'c-mode-common-hook '(lambda () (setq tab-width 4)))
;; Disable to color the selected region
(setq transient-mark-mode nil)
;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; バッファ自動再読み込み
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(global-auto-revert-mode 1)
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
(setq make-backup-files nil)
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
 '(indent-tabs-mode nil)
 '(package-selected-packages
   '(evil-collection embark-consult consult embark marginalia orderless vertico yaml-mode neotree jsonnet-mode undo-tree mozc mozc-im mozc-popup evil-surround csv-mode clojure-mode typescript typescript-mode evil-magit web-mode powerline popwin matlab-mode markdown-mode magit lua-mode helm exec-path-from-shell evil-leader ess dockerfile-mode cmake-mode auto-complete))
 '(safe-local-variable-values '((checkdoc-minor-mode . t) (mangle-whitespace . t))))

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

;; Package managenment; http://sakito.jp/emacs/emacs24.html
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(package-initialize)
;; http://qiita.com/hottestseason/items/1e8a46ad1ebcf7d0e11c
(defvar installed-package-list
  '(
    auto-complete
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
  "SPC" 'set-mark-command
  "x" 'execute-extended-command
  "b" 'switch-to-buffer
  "f" 'find-file
  "d" 'dired
  "j" 'dired-jump
  "k" 'kill-buffer
  "q" 'View-quit
  "w" 'evil-window-prev
  "p" 'comint-previous-input
  "n" 'comint-next-input)

;; evil-mode
;; https://lists.ourproject.org/pipermail/implementations-list/2011-September/001140.html
(evil-mode 1)
(define-key evil-motion-state-map " " 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "S-SPC") 'evil-scroll-page-up)
(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'evil-end-of-line)
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
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t) ; C-n, C-p

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(hl-line ((t (:background "color-236"))))
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

;; init.el閲覧時にPackage cl is deprecatedを表示されないようにする
;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; End:
