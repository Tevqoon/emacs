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

;; Initialize package sources
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

;; Fix emacs $PATH to correspond with shell  

  (use-package exec-path-from-shell)
    (exec-path-from-shell-initialize)

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-solarized-light t))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'doom-solarized-light t))
    ('dark (load-theme 'doom-solarized-dark-high-contrast t))))

(add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

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
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 12)))

(tab-bar-mode) ; We run it a priori in case the saved desktop already has tabs.

;;  (setq tab-bar-select-tab-modifiers )

(require 'cl-lib)
(defun my-keyboard-escape-quit (fun &rest args)
  (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
    (apply fun args)))
(advice-add 'keyboard-escape-quit :around #'my-keyboard-escape-quit)

(use-package page-break-lines)
(global-page-break-lines-mode)

(delete-selection-mode 1)

;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(desktop-save-mode 1)

(electric-pair-mode 1)

;; Inhibit < so it can be used for snippets in org
(add-function :before-until electric-pair-inhibit-predicate
(lambda (c) (eq c ?<)))

(show-paren-mode 1)
(setq show-paren-delay 0)

(winner-mode 1)

;; Uses rainbow colors for matching parens etc
(use-package rainbow-delimiters
  :defer t
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

(setq winum-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-`") 'winum-select-window-by-number)
    (define-key map (kbd "s-0") 'winum-select-window-0-or-10)
    (define-key map (kbd "s-1") 'winum-select-window-1)
    (define-key map (kbd "s-2") 'winum-select-window-2)
    (define-key map (kbd "s-3") 'winum-select-window-3)
    (define-key map (kbd "s-4") 'winum-select-window-4)
    (define-key map (kbd "s-5") 'winum-select-window-5)
    (define-key map (kbd "s-6") 'winum-select-window-6)
    (define-key map (kbd "s-7") 'winum-select-window-7)
    (define-key map (kbd "s-8") 'winum-select-window-8)
    (define-key map (kbd "s-9") 'winum-select-window-8)
    map))

(use-package winum)

(winum-mode)

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

(use-package org-noter)

(use-package anki-editor)

(defun anki/after-save-actions ()
  "Checks whether the current file contains the ANKI_NOTE_TYPE property in the current org buffer, and pushes the flashcards in the buffer if it does."
  (when (member "ANKI_NOTE_TYPE" (org-buffer-property-keys))
    (anki-editor-push-notes)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
                                          #'anki/after-save-actions)))

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

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/repos/org/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
   ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil))

(setq org-roam-capture-templates
  '(("d" "default" plain
     "%?"
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)
    ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
            :unnarrowed t)
    ("b" "book notes" plain
          "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Book")
          :unnarrowed t)
    ("f" "Flashcard" plain (file "~/Documents/repos/org/roam/templates/AnkiNoteTemplate.org")
     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
       :unnarrowed t)))

(use-package project)
(use-package eglot)

(add-hook 'python-mode-hook 'eglot-ensure)
;;(add-hook 'LaTeX-mode-hook 'eglot-ensure)

(use-package python-mode
  :hook (python-mode . eglot)
  ;; :hook (python-mode . lsp)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

  ;; Leaving the debugging stuff out for now.
  ;; (dap-python-executable "python3")
  ;; (dap-python-debugger 'debugpy)
  ;; :config
  ;; (require 'dap-python))

(use-package auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)))

(setq latex-run-command "xelatex")

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(use-package company-auctex
  :init (company-auctex-init))

(use-package xenops)

(add-hook 'latex-mode-hook #'xenops-mode)
(add-hook 'LaTeX-mode-hook #'xenops-mode)
(add-hook 'org-mode-hook #'xenops-mode)

(add-hook 'xenops-mode-hook #'xenops-dwim)


(setq xenops-reveal-on-entry t)
(setq xenops-math-image-scale-factor 2.0)

(use-package flycheck)
(global-flycheck-mode)

;; Yasnippet settings
(use-package yasnippet)
(yas-global-mode 1)

(use-package yasnippet-snippets)

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
