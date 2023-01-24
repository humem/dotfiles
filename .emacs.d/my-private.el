;;; my-private.el -- My private settings  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(custom-set-variables '(user-full-name "Hiroshi Umemoto"))
(defvar my/enable-setup-tracker t)

;; (defvar my/enable-profiler t)
;; (defvar my/fonts-family "HackGenNerd Console")
;; (custom-set-variables '(gcmh-verbose t))

;; Enable when running on dynabook
(when nil
  (defvar my/fonts-height 105)
  (defvar my/battery t)
  (defvar my/modus-themes 'modus-vivendi)
  (defvar my/modus-themes-region '(accented no-extend))
  (custom-set-faces
   '(fill-column-indicator ((t (:foreground "dim gray"))))
   '(doom-modeline-bar-inactive ((t (:box (:line-width (2 . 2)))))))
  (custom-set-variables
   '(highlight-indent-guides-auto-character-face-perc 30)
   '(highlight-indent-guides-auto-top-character-face-perc 50))
  (defvar my/skip t)
  )

(provide 'my-private)

;;; my-private.el ends here
