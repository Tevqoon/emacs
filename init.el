(setq inhibit-startup-message t)

(setq frame-resize-pixelwise t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu

(setq-default cursor-type 'bar)

(column-number-mode) ; Add column numbers to modeline
(global-display-line-numbers-mode t) ; Add line numbers

;;Disable line numbers for some modes
(dolist (mode '(org-mode hook
                         term-mode-hook
                         shell-mode-hook
                         eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :height 140)

;; Set right option key to act nicely to enter symbol layers
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; Make ESC quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-solarized-light t))

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa"   . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("elpa"   . "http://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Fix emacs $PATH to correspond with shell  

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

(use-package swiper)
(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Make counsel-switch-buffer the default buffer switcher
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 12)))

(require 'cl-lib)
(defun my-keyboard-escape-quit (fun &rest args)
  (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
    (apply fun args)))
(advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit)

(use-package page-break-lines)
(global-page-break-lines-mode)

;; Uses rainbow colors for matching parens etc
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows possible keyboard shortcuts
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/repos")
    (setq projectile-project-search-path '("~/Documents/repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)
(setq auth-sources '("~/.authinfo"))

(use-package tablist)
(use-package pdf-tools)
(pdf-tools-install)

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-files
        '("~/Documents/repos/org/agenda")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

(setq org-refile-targets
      '(("~/Documents/repos/org/Archive.org" :maxlevel . 1)
        ("~/Documents/repos/org/agenda/Tasks.org" :maxlevel . 1)))

;; Save org buffers after refiling
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))))

(use-package org-fragtog
  :init (add-hook 'org-mode-hook 'org-fragtog-mode))

(setq org-preview-latex-process-alist
      '(
       (dvipng
        :programs ("latex" "dvipng")
        :description "dvi > png"
        :message "you need to install the programs: latex and dvipng."
        :image-input-type "dvi"
        :image-output-type "png"
        :image-size-adjust (1.0 . 1.0)
        :latex-compiler ("latex -interaction nonstopmode -output-directory %o %f")
        :image-converter ("dvipng -D %D -T tight -o %O %f"))))

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(use-package anki-editor)

(use-package org-noter)

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Documents/repos/org/agenda/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree "~/Documents/repos/org/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "jj")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package python-mode
  :ensure t
  ;;:hook (python-mode . eglot)
  ;; :hook (python-mode . lsp)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

  ;; Leaving the debugging stuff out for now.
  ;; (dap-python-executable "python3")
  ;; (dap-python-debugger 'debugpy)
  ;; :config
  ;; (require 'dap-python))

(use-package company-math)
;; global activation of the unicode symbol completion 
(add-to-list 'company-backends 'company-math-symbols-unicode)

(use-package flycheck)
(global-flycheck-mode)

;; Yasnippet settings
(use-package yasnippet)
(yas-global-mode 1)

(use-package eglot)

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)

(use-package company)

(add-hook 'after-init-hook 'global-company-mode)

;; No delay when showing suggestions
(setq company-idle-delay 0.3)
;; Show suggestions after the first character is typed
(setq company-minimum-prefix-length 1)
;; Make the selection wrap around
(setq company-selection-wrap-around t)
;; Make tab cycle
(company-tng-mode)
