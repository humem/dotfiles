;;; lang-python.el --- My lang-python.el  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                        (require 'lsp-pyright)
                        (lsp-deferred))))

(use-package python-mode
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(use-package py-isort
  :after python
  :hook ((python-mode . pyvenv-mode)
         (before-save . py-isort-before-save)))

(use-package blacken
  :delight
  :hook (python-mode . blacken-mode)
  :custom (blacken-line-length 79))
;; (leaf blacken
;;    :ensure t
;;    :custom ((blacken-line-length . 119)               ; 1行の流さを119文字まで許可
;;             (blacken-skip-string-normalization . t))) ; 文字リテラルの「'」を「"」に変更しないように抑制


;;; lang python
(provide 'lang-python)

;;; lang-python.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; indent-tabs-mode: nil
;; End:
