;; straight
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
    ((eq system-type 'windows-nt)
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina-12")
       ((member "Consolas" (font-family-list)) "Consolas-12")))
    ((eq system-type 'darwin)
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina-12")
       ((member "Menlo" (font-family-list)) "Menlo-12")
       ((member "Monaco" (font-family-list)) "Monaco-12")))
    ((eq system-type 'gnu/linux)
     (cond
       ((member "Fira Code Retina" (font-family-list)) "Fira Code Retina-12")
       ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono-12"))))
  t
  t)

;; set font for chinese characters
(when (fboundp 'set-fontset-font)
  (set-fontset-font
    t
    '(#x4e00 . #x9fff)
    (cond
      ((eq system-type 'windows-nt)
       (cond
         ((member "等距更纱黑体 SC" (font-family-list)) "等距更纱黑体 SC-12")
         ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei-12")
         ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei-12")
         ((member "SimHei" (font-family-list)) "SimHei-12")))
      ((eq system-type 'darwin)
       (cond
         ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC-12")
         ((member "Heiti SC" (font-family-list)) "Heiti SC-12")
         ((member "Heiti TC" (font-family-list)) "Heiti TC-12")))
      ((eq system-type 'gnu/linux)
       (cond
         ((member "Sarasa Mono SC" (font-family-list)) "Sarasa Mono SC-12")
         ((member "Source Han Sans" (font-family-list)) "Source Han Sans-12")
         ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei-12"))))))

;; set font for emoji
(when (fboundp 'set-fontset-font)
  (set-fontset-font
    t
    '(#x1f300 . #x1fad0)
    (cond
      ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji-12")
      ((member "Noto Emoji" (font-family-list)) "Noto Emoji-12")
      ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji-12")
      ((member "Symbola" (font-family-list)) "Symbola-12")
      ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji-12"))
    ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
    ;; GNU Emacs Removes Color Emoji Support on the Mac
    ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
    ;;
    ))

(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)
                 ("[gG][sS]" gbk-dos . gbk-dos))))
