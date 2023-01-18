;;; early-init.el --- early-init file for Emacs -*- lexical-binding: t -*-

;;; Code:

;;; Commentary:

;; Debugging
;; (setq debug-on-error t)
;; (setq warning-minimum-level :error)

;; Suppress cl warning
(setq byte-compile-warnings '(cl-functions))

;; Disable GUI bars
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;;; early-init.el ends here
