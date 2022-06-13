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
  (setq use-package-always-defer nil) ;默认加载开关
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
             (global-display-line-numbers-mode t)
             (global-hl-line-mode 1)
             (global-visual-line-mode 1))

(use-package emacs
             :config
             ;; set default tab char's display width to 4 spaces
             (setq-default tab-width 4)
             (defvaralias 'c-basic-offset 'tab-width)
             (defvaralias 'cperl-indent-level 'tab-width)
             ;; make indent commands use space only (never tab character)
             (setq-default indent-tabs-mode nil))

(use-package emacs
             :unless (display-graphic-p)
             :config
             (menu-bar-mode -1)
             (tool-bar-mode -1))

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

(use-package evil
             :init
             (setq evil-toggle-key "C-<f1>")
             (setq evil-shift-width 2)
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             :config
             (evil-mode 1)

             ;; Use visual line motions even outside of visual-line-mode buffers
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

             (evil-set-initial-state 'dired-mode 'emacs)
             (evil-set-initial-state 'pdf-view-mode 'emacs)
             (evil-set-initial-state 'messages-buffer-mode 'emacs))

(use-package evil-nerd-commenter
             :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package counsel
             :diminish ivy-mode
             :diminish counsel-mode
             :bind (("C-s" . swiper)
                    :map ivy-minibuffer-map
                    ("TAB" . ivy-alt-done))
             :init
             (ivy-mode 1)
             (counsel-mode 1)
             :config
             (setq ivy-use-virtual-buffers t)
             (setq enable-recursive-minibuffers t))

(use-package prescient
             :diminish
             :config
             ;; (prescient-persist-mode)
             )


(use-package ivy-prescient
             :after counsel
             :init
             (ivy-prescient-mode)
             (prescient-persist-mode))

(use-package company
             :straight (company :files (:defaults "icons"))
             :diminish company-mode
             :bind (:map company-active-map
                         ("<tab>" . nil)
                         ("TAB" . nil))
             :custom
             (company-minimum-prefix-length 2)
             (company-idle-delay 0.01)
             :config)

(use-package company-prescient
             :after company
             :config
             (company-prescient-mode 1)
             (prescient-persist-mode))

(use-package which-key
             :diminish which-key-mode
             :config
             (which-key-mode))

; magit
(use-package magit
             :defer t
             :bind ("C-x g" . magit-status))

(use-package pdf-tools
             :magic ("%PDF" . pdf-view-mode)
             :config
             (pdf-tools-install)
             (setq-default pdf-view-display-size 'fit-page))

(defun try/TeX-command-save-buffer-and-run-all ()
  "Save the buffer and run TeX-command-run-all"
  (interactive)
  (let (TeX-save-query) (TeX-save-document (TeX-master-file)))
  (TeX-command-run-all nil))

;; copied ivy-bibtex and modified it to cite action
(defun try/ivy-bibtex-cite (&optional arg local-bib)
  "Search BibTeX entries using ivy.

  With a prefix ARG the cache is invalidated and the bibliography
  reread.

  If LOCAL-BIB is non-nil, display that the BibTeX entries are read
  from the local bibliography.  This is set internally by
  `ivy-bibtex-with-local-bibliography'."
  (interactive "P")
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (ivy-read (format "Insert citation %s: " (if local-bib " (local)" ""))
              candidates
              :preselect preselect
              :caller 'ivy-bibtex
              :history 'ivy-bibtex-history
              :action 'ivy-bibtex-insert-citation)))

(defun try/latex-mode-setup ()
  (require 'company-reftex)
  (turn-on-reftex)
  (require 'company-auctex)
  (require 'company-math)
  (setq-local company-backends

              (append '( (company-reftex-labels
                          company-reftex-citations)
                        (company-math-symbols-unicode company-math-symbols-latex company-latex-commands)
                        (company-auctex-macros company-auctex-symbols company-auctex-environments)
                        company-ispell)
                      company-backends)))


(defun try/counsel-insert-file-path ()
  "Insert relative file path using counsel minibuffer"
  (interactive)
  (unless (featurep 'counsel) (require 'counsel))
  (ivy-read "Insert filename: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (insert (file-relative-name x)))))

(use-package olivetti
             :diminish
             :hook (text-mode . olivetti-mode)
             :config
             (setq olivetti-body-width 100))

(use-package outshine
             :config
             (setq LaTeX-section-list '( ("part" 0)
                                        ("chapter" 1)
                                        ("section" 2)
                                        ("subsection" 3)
                                        ("subsubsection" 4)
                                        ("paragraph" 5)
                                        ("subparagraph" 6)
                                        ("begin" 7)))
             (add-hook 'LaTeX-mode-hook #'(lambda ()
                                            (outshine-mode 1)
                                            (setq outline-level #'LaTeX-outline-level)
                                            (setq outline-regexp (LaTeX-outline-regexp t))
                                            (setq outline-heading-alist
                                                  (mapcar (lambda (x)
                                                            (cons (concat "\\" (nth 0 x)) (nth 1 x)))
                                                          LaTeX-section-list))))

             )

;; company
(use-package company-math)
(use-package company-auctex)
(use-package company-reftex)

;;  use cdlatex
(use-package cdlatex)

;; https://gist.github.com/saevarb/367d3266b3f302ecc896
;; https://piotr.is/2010/emacs-as-the-ultimate-latex-editor/

(use-package latex
             :straight auctex
             :defer t
             :custom
             (olivetti-body-width 100)
             (cdlatex-simplify-sub-super-scripts nil)
             (reftex-default-bibliography
               '("~/ref.bib"))
             (bibtex-dialect 'biblatex)
             :mode
             ("\\.tex\\'"       . latex-mode)
             :bind (:map LaTeX-mode-map
                         ("C-c C-e" . cdlatex-environment))
             :hook
             (LaTeX-mode        . olivetti-mode)
             (LaTeX-mode        . TeX-PDF-mode)
             (LaTeX-mode        . company-mode)
             (LaTeX-mode        . flyspell-mode)
             (LaTeX-mode        . flycheck-mode)
             (LaTeX-mode        . LaTeX-math-mode)
             (LaTeX-mode        . turn-on-reftex)
             (LaTeX-mode        . TeX-source-correlate-mode)
             (LaTeX-mode        . try/latex-mode-setup)
             (LaTeX-mode        . turn-on-cdlatex)

             :config
             (setq TeX-auto-save t)
             (setq TeX-parse-self t)
             (setq-default TeX-master nil)
             (setq TeX-save-query nil)
             (setq TeX-engine 'xetex)

             (setq reftex-plug-into-AUCTeX t)

             ;; pdftools
             ;; https://emacs.stackexchange . com/questions/21755/use-pdfview-as-default-auctex-pdf-viewer#21764
             (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                   TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                   TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
             ;; to have the buffer refresh after compilation,
             ;; very important so that PDFView refesh itself after comilation
             (add-hook 'TeX-after-compilation-finished-functions
                       #'TeX-revert-document-buffer))

(use-package general
             :config
             (general-evil-setup t))

(general-define-key
  :states '(normal visual)
  :keymaps 'LaTeX-mode-map
  :prefix "SPC"
  "TAB"  '(outshine-cycle :which-key "outshine-cycle")
  "f"  '(LaTeX-fill-region :which-key "latex-fill-region")
  "SPC"  '(try/TeX-command-save-buffer-and-run-all :which-key "latex-save-run")
  "c"  '(try/ivy-bibtex-cite :which-key "ivy-cite")
  "i"   '(try/counsel-insert-file-path :which-key "insert-relative-filepath")
  "t" '(tab-bar-switch-to-tab :which-key "tab-switch-name")
  "o"  '(outshine-imenu :which-key "menu")
  "t"  '(outshine-cycle-buffer :which-key "fold-buffer"))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
