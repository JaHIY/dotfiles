;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(prefer-coding-system 'utf-8)

(when (eq system-type 'windows-nt)
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos)
                 ("[gG][sS]" gbk-dos . gbk-dos))))

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

(eval-and-compile
  (setq use-package-always-ensure t) ;不用每个包都手动添加:ensure t关键字
  (setq use-package-always-defer t) ;默认都是延迟加载，不用每个包都手动添加:defer t
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally t)
  (setq use-package-verbose t))

(use-package monokai-theme
             :config
             (load-theme 'monokai t))

(use-package emacs
             :config
             (setq display-line-numbers-type t)
             (global-display-line-numbers-mode t))

(use-package emacs
             :if (display-graphic-p)
             :config
             ;; set default font
             (set-frame-font
               (cond
                 ((eq system-type 'windows-nt)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina-12")
                    ((find-font (font-spec :name "Consolas")) "Consolas-12")))
                 ((eq system-type 'darwin)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina-12")
                    ((find-font (font-spec :name "Menlo")) "Menlo-12")
                    ((find-font (font-spec :name "Monaco")) "Monaco-12")))
                 ((eq system-type 'gnu/linux)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina-12")
                    ((find-font (font-spec :name "DejaVu Sans Mono")) "DejaVu Sans Mono-12"))))
               t
               t)

             (when (fboundp 'set-fontset-font)
               ;; set font for chinese characters
               (set-fontset-font
                 t
                 '(#x4e00 . #x9fff)
                 (cond
                   ((eq system-type 'windows-nt)
                    (cond
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC-12")
                      ((find-font (font-spec :name "Microsoft YaHei")) "Microsoft YaHei-12")
                      ((find-font (font-spec :name "Microsoft JhengHei")) "Microsoft JhengHei-12")
                      ((find-font (font-spec :name "SimHei")) "SimHei-12")))
                   ((eq system-type 'darwin)
                    (cond
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC-12")
                      ((find-font (font-spec :name "Heiti SC")) "Heiti SC-12")
                      ((find-font (font-spec :name "Heiti TC")) "Heiti TC-12")))
                   ((eq system-type 'gnu/linux)
                    (cond
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC-12")
                      ((find-font (font-spec :name "Source Han Sans")) "Source Han Sans-12")
                      ((find-font (font-spec :name "WenQuanYi Micro Hei")) "WenQuanYi Micro Hei-12")))))

               ;; set font for emoji
               (set-fontset-font
                 t
                 '(#x1f300 . #x1fad0)
                 (cond
                   ((find-font (font-spec :name "Noto Color Emoji")) "Noto Color Emoji-12")
                   ((find-font (font-spec :name "Noto Emoji")) "Noto Emoji-12")
                   ((find-font (font-spec :name "Segoe UI Emoji")) "Segoe UI Emoji-12")
                   ((find-font (font-spec :name "Symbola")) "Symbola-12")
                   ((find-font (font-spec :name "Apple Color Emoji")) "Apple Color Emoji-12"))
                 ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
                 ;; GNU Emacs Removes Color Emoji Support on the Mac
                 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
                 ;;
                 )))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
