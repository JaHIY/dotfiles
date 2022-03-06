;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; disable startup message
(setq inhibit-startup-message t)

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
             :defer nil
             :config
             (load-theme 'monokai t))

(use-package emacs
             :config
             (setq display-line-numbers-type t)
             (global-display-line-numbers-mode t))

(use-package emacs
             :config
             ;; set default tab char's display width to 4 spaces
             (setq-default tab-width 4)
             (defvaralias 'c-basic-offset 'tab-width)
             (defvaralias 'cperl-indent-level 'tab-width)
             ;; make indent commands use space only (never tab character)
             (setq-default indent-tabs-mode nil))

(use-package emacs
             :if (display-graphic-p)
             :config
             ;; set default font
             (set-frame-font
               (cond
                 ((eq system-type 'windows-nt)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina")
                    ((find-font (font-spec :name "Consolas")) "Consolas")))
                 ((eq system-type 'darwin)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina")
                    ((find-font (font-spec :name "Menlo")) "Menlo")
                    ((find-font (font-spec :name "Monaco")) "Monaco")))
                 ((eq system-type 'gnu/linux)
                  (cond
                    ((find-font (font-spec :name "Fira Code Retina")) "Fira Code Retina")
                    ((find-font (font-spec :name "DejaVu Sans Mono")) "DejaVu Sans Mono"))))
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
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
                      ((find-font (font-spec :name "Microsoft YaHei")) "Microsoft YaHei")
                      ((find-font (font-spec :name "Microsoft JhengHei")) "Microsoft JhengHei")
                      ((find-font (font-spec :name "SimHei")) "SimHei")))
                   ((eq system-type 'darwin)
                    (cond
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
                      ((find-font (font-spec :name "Heiti SC")) "Heiti SC")
                      ((find-font (font-spec :name "Heiti TC")) "Heiti TC")))
                   ((eq system-type 'gnu/linux)
                    (cond
                      ((find-font (font-spec :name "Sarasa Mono SC")) "Sarasa Mono SC")
                      ((find-font (font-spec :name "Source Han Sans")) "Source Han Sans")
                      ((find-font (font-spec :name "WenQuanYi Micro Hei")) "WenQuanYi Micro Hei")))))

               ;; set font for emoji
               (set-fontset-font
                 t
                 '(#x1f300 . #x1fad0)
                 (cond
                   ((find-font (font-spec :name "Noto Color Emoji")) "Noto Color Emoji")
                   ((find-font (font-spec :name "Noto Emoji")) "Noto Emoji")
                   ((find-font (font-spec :name "Segoe UI Emoji")) "Segoe UI Emoji")
                   ((find-font (font-spec :name "Symbola")) "Symbola")
                   ((find-font (font-spec :name "Apple Color Emoji")) "Apple Color Emoji"))
                 ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
                 ;; GNU Emacs Removes Color Emoji Support on the Mac
                 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
                 ;;
                 )))

(set-face-attribute 'default nil :height 140)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
