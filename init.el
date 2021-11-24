;; (setq debug-on-error t)

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
                         eshell-mode-hook
                         xwidget-webkit-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :height 140)

;; Set right option key to act nicely to enter symbol layers
(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)
(setq ns-right-command-modifier 'hyper)

;; Make ESC quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Switching between frames of emacs in line with my common shortcuts
(global-set-key (kbd "C-s-<tab>") #'ns-next-frame)

(setq disabled-command-function nil)

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

(defun bjm-swiper-recenter (&rest args)
  "recenter display after swiper"
  (recenter)q
  )
(advice-add 'swiper :after #'bjm-swiper-recenter)


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

(tab-bar-mode)
(setq tab-bar-new-tab-choice "*scratch*")

(global-set-key (kbd "s-t") #'tab-bar-new-tab)
(global-set-key (kbd "s-T") #'tab-undo)

(global-set-key (kbd "s-w") #'tab-close)
(setq tab-bar-close-last-tab-choice 'delete-frame)

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

(electric-pair-mode 1)

;; Inhibit the symbol less so it can be used for snippets in org
(add-function
 :before-until electric-pair-inhibit-predicate
 (lambda (c) (eq c ?<)))

(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package paredit)

(winner-mode 1)

(global-auto-revert-mode)

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

(global-set-key (kbd "C-x g") 'magit-status)

(use-package forge)
(setq auth-sources '("~/.authinfo"))

(use-package tablist)
(use-package pdf-tools)
(pdf-tools-install)

(setq winum-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-`") 'winum-select-window-by-number)
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
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)")))

;; Save org buffers after refiling
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)

(global-set-key (kbd "C-c a") 'org-agenda)

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

(setq org-capture-templates
      `(("t" "Task" entry (file "~/Documents/org/20211117164414-inbox.org")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

(define-key global-map (kbd "M-i")
  (lambda
    () (interactive) (org-capture)))

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

(setq org-export-with-broken-links t)

(use-package org-ref)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-map)
         ("C-c n r" . org-roam-refile)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(advice-add 'org-roam-refile :after 'org-save-all-org-buffers)

(setq org-roam-mode-section-functions
    (list #'org-roam-backlinks-section
          #'org-roam-reflinks-section
          #'org-roam-unlinked-references-section))

(add-to-list 'display-buffer-alist
           '("\\*org-roam\\*"
             (display-buffer-in-direction)
             (direction . right)
             (window-width . 0.33)
             (window-height . fit-window-to-buffer)))

(setq org-roam-dailies-directory "daily/")

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
   ;; :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t)
    (setq  org_roam-ui-follow nil)
    (setq org-roam-ui-update-on-save t)
    (setq org-roam-ui-open-on-start nil))

(defun org-roam-ui-open-in-emacs ()
  (interactive)
  (xwidget-webkit-browse-url "http://localhost:35901"))

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
         :unnarrowed t)
    ("r" "bibliography reference" plain
     "%?
%^{author} published %^{entry-type} in %^{date}: fullcite:%\\1."
     :target
     (file+head "references/${citekey}.org" "#+title: ${title}\n")
     :unnarrowed t)))

(use-package vulpea)

(add-to-list 'org-tags-exclude-from-inheritance "project")

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
  (org-element-map                          ; (2)
       (org-element-parse-buffer 'headline) ; (1)
       'headline
     (lambda (h)
       (eq (org-element-property :todo-type h)
           'todo))
     nil 'first-match))                     ; (3)

(setq prune/ignored-files
      '("20211119122103-someday.org"
        "20211117183951-tasks.org"
        "20211117164414-inbox.org"))

(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-update-tag ()
  "Update PROJECT tag in the current buffer."
  (when (and (not (member (buffer-name) prune/ignored-files))
             (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (vulpea-project-p)
            (setq tags (cons "project" tags))
          (setq tags (remove "project" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(add-hook 'find-file-hook #'vulpea-project-update-tag)
(add-hook 'before-save-hook #'vulpea-project-update-tag)

(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

(defun vulpea-agenda-files-update (&rest _)
  "Update the value of `org-agenda-files'."
  (setq org-agenda-files (vulpea-project-files)))

(advice-add 'org-agenda :before #'vulpea-agenda-files-update)

(use-package anki-editor)

(add-to-list 'org-tags-exclude-from-inheritance "flashcards")

(defun anki/flashcard-p ()
  "Returns non-nil if the current buffer has a flash card"
  (member "ANKI_NOTE_TYPE" (org-buffer-property-keys)))

(defun anki/flashcards-update-tag ()
  "Update flashcard tag in the current buffer"
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (anki/flashcard-p)
            (setq tags (cons "flashcards" tags))
          (setq tags (remove "flashcards" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
  (when (or (seq-difference tags original-tags)
            (seq-difference original-tags tags))
    (apply #'vulpea-buffer-tags-set tags))))))

(add-hook 'find-file-hook #'anki/flashcards-update-tag)
(add-hook 'before-save-hook #'anki/flashcards-update-tag)

(defun anki/flashcards-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"flashcards\"%"))]))))

(defun anki/push-filename (filename)
  "Opens the file with filename as a temporary buffer and pushes its notes."
  (save-excursion
    (with-current-buffer (find-file-noselect filename)
      (progn (anki-editor-push-notes)))))

(defun anki/push-all ()
  "Maps over the files with the flashcards tag and pushes them."
  (interactive)
  (mapc #'anki/push-filename (anki/flashcards-files)))

(use-package python-mode
  :hook (python-mode . eglot)
  ;; :hook (python-mode . lsp)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

(setq org-latex-packages-alist '(("" "/Users/jure/.emacs.d/defaults/js" t)))
;;(setq org-latex-packages-alist nil)

(use-package tex-mode
  :ensure auctex)
(setq font-latex-fontify-script nil)

(setq latex-run-command "lualatex")

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

(setq-default TeX-master nil)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(use-package cdlatex
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))

(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

(use-package xenops)

;; (add-hook 'latex-mode-hook #'xenops-mode)
;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
(add-hook 'org-mode-hook #'xenops-mode)

;; The org files used are relatively small, hence we can afford to auto-expand.
(add-hook 'org-mode-hook (lambda () (add-hook 'xenops-mode-hook #'xenops-dwim)))

(setq xenops-reveal-on-entry t)
(setq xenops-math-image-scale-factor 2.0)

(use-package project)
(use-package eglot)

(add-hook 'python-mode-hook 'eglot-ensure)
;;(add-hook 'LaTeX-mode-hook 'eglot-ensure)

(use-package flycheck)
(global-flycheck-mode)

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode))

(use-package laas
:hook (LaTeX-mode . laas-mode))

;; Yasnippet settings
(use-package yasnippet)
(yas-global-mode 1)

(use-package yasnippet-snippets)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((websocket))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
