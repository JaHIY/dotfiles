; straight
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package monokai-theme
             :config
             (load-theme 'monokai t))

;; set default font
(set-frame-font
  (cond
    ((string-equal system-type "windows-nt") ; Microsoft Windows
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina")
       ((member "Consolas" (font-family-list)) "Consolas")))
    ((string-equal system-type "darwin") ; macOS
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina")
       ((member "Menlo" (font-family-list)) "Menlo")
       ((member "Monaco" (font-family-list)) "Monaco")))
    ((string-equal system-type "gnu/linux") ; linux
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina")
       ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono"))))
  t
  t)

;; set font for chinese characters
(set-fontset-font
  t
  '(#x4e00 . #x9fff)
  (cond
    ((string-equal system-type "windows-nt")
     (cond
       ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC")
       ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
       ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
       ((member "SimHei" (font-family-list)) "SimHei")))
    ((string-equal system-type "darwin")
     (cond
       ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC")
       ((member "Heiti SC" (font-family-list)) "Heiti SC")
       ((member "Heiti TC" (font-family-list)) "Heiti TC")))
    ((string-equal system-type "gnu/linux")
     (cond
       ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC")
       ((member "Source Han Sans" (font-family-list)) "Source Han Sans")
       ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))

;; set font for emoji
(set-fontset-font
  t
  '(#x1f300 . #x1fad0)
  (cond
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola")
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
  ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
  ;; GNU Emacs Removes Color Emoji Support on the Mac
  ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
  ;;
  )
