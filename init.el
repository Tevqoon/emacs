;; (setq debug-on-error t)

(setq inhibit-startup-message t)

(setq frame-resize-pixelwise t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room

(setq ring-bell-function 'ignore) ; Shut the annoying fucking sound

(setq-default cursor-type 'bar)

(column-number-mode) ; Add column numbers to modeline

;; (global-display-line-numbers-mode t) ; Add line numbers
;; No line numbers for now fuck that

;;Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                xwidget-webkit-mode-hook
                pdf-view-mode-hook
                racket-repl-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Switching between frames of emacs in line with my common shortcuts
(global-set-key (kbd "C-s-<tab>") #'ns-next-frame)

(setq custom-file "~/.emacs.d/custom.el") ; Separate custom file since we retangle =init.el= often.
(load custom-file)

(setq echo-keystrokes .01) ; Make sure to show keystrokes immediately

(server-start) ;Start emacs as a server to be used externally.

;; (defun ask-before-closing ()
;;   "Close only if y was pressed."
;;   (interactive)
;;   (if (y-or-n-p (format "Are you sure you want to close emacs? "))
;;       (save-buffers-kill-emacs)                                                                               
;;     (message "Canceled frame close")))

;;(global-set-key (kbd "C-x C-c") 'ask-before-closing)

(setq confirm-kill-emacs #'y-or-n-p) ; Fucking ask me first i stg

;; utf-8 ;; 
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq make-backup-files nil) ; don't litter

(set-register ?r '(file . "~/.emacs.d/readme.org"))
(set-register ?t '(file . "~/Documents/org/tasks.org"))

  (defun efs/org-mode-visual-fill ()
    "Sets the width just so that there's a little bit
     of space on the left and right."
    (setq visual-fill-column-width 110
          visual-fill-column-center-text t))

  (efs/org-mode-visual-fill)
  (global-visual-fill-column-mode 1)
  ; Use it everywhere

    (use-package visual-fill-column)

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
            ;('light (load-theme 'modus-operandi t))
            ('dark (load-theme 'doom-dracula t))
            ;('dark (load-theme 'modus-vivendi t))
            ))

(if (eq window-system 'ns)
    (progn
      (setq ns-alternate-modifier 'meta)
      (setq ns-right-alternate-modifier 'none)
      (setq ns-right-command-modifier 'hyper)
      (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)
      (setq org-roam-directory "~/Documents/org")
        ))

  (use-package battery)
  (when (and battery-status-function
         (not (string-match-p "N/A" 
                  (battery-format "%B"
                          (funcall battery-status-function)))))
    (display-battery-mode 1))

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
(global-set-key (kbd "s-b") 'counsel-switch-buffer)

(setq ivy-dynamic-exhibit-delay-ms 250)

    ;; Enable richer annotations using the Marginalia package
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    ;; The :init configuration is always executed (Not lazy!)
    :init

    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode))

  (use-package all-the-icons)
  (use-package doom-modeline
    :init (doom-modeline-mode 1)
    :custom ((doom-modeline-height 12)))

  (setq display-time-default-load-average nil)
  (display-time-mode 1)

  (tab-bar-mode 1)

  (setq tab-bar-new-tab-choice "*scratch*")

  (global-set-key (kbd "s-t") #'tab-bar-new-tab)
  (global-set-key (kbd "s-T") #'tab-undo)

  (global-set-key (kbd "s-w") #'tab-close)
  (setq tab-bar-close-last-tab-choice 'delete-frame)

  (global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

  (global-set-key (kbd "s-o") #'other-window)

  (winner-mode 1)

  (global-set-key (kbd "H-<right>") #'winner-redo)
  (global-set-key (kbd "H-<left>") #'winner-undo)

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

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(use-package god-mode)

(global-set-key (kbd "<escape>") #'god-local-mode)

(defun my-god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(define-key god-local-mode-map (kbd ".") #'repeat)

(define-key god-local-mode-map (kbd "i") #'god-local-mode)
(define-key god-local-mode-map (kbd "z") #'repeat)

(define-key god-local-mode-map (kbd "S-SPC") #'open-org-agenda)

(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)

(define-key god-local-mode-map (kbd "[") #'backward-paragraph)
(define-key god-local-mode-map (kbd "]") #'forward-paragraph)


(add-hook 'post-command-hook #'my-god-mode-update-cursor-type)

(use-package dash)
(use-package s)
(use-package cl-lib)

(defun dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (dump varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (mapc (lambda (var) (print (list 'setq var (list 'quote (symbol-value var)))
			     buffer))
	varlist))

(defvar closing-variables nil
  "Variables to dump to a file upon closing emacs.")

(defvar closing-variables-filename "~/.emacs.d/variables.el"
  "The filename in which closing variables are stored.")

(load closing-variables-filename)

(defun dump-closing-variables ()
  "Writes all of the variables in the list closing-variables to the file closing-variables-filename"
  (interactive)
  (dump-vars-to-file closing-variables closing-variables-filename))

(add-hook 'kill-emacs-hook #'dump-closing-variables) ; Write on exit

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

(which-key-enable-god-mode-support)

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

  (use-package git-timemachine)

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
    (setq org-ellipsis " ▾")
    (setq org-hide-emphasis-markers t))

  (use-package org-bullets
    :after org
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)

    ;; Line spacing
  (setq line-spacing 0.1)

  (set-face-attribute 'default nil :height 140)

  (let* (;(variable-tuple '(:font "Source Sans Pro"))
         (variable-tuple '(:font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1"))
         ;(variable-tuple '(:font "Arial"))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-document-title
                              ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))


  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Brygada 1918" :height 155)))) ;; For regular writing
   ;'(variable-pitch ((t (:family "Arial" :height 155))))
   '(fixed-pitch ((t (:family "Menlo" :height 140))))      ;; For code and stuff

   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)

  (use-package org-pretty-table
    :straight
      (:host github :repo "Fuco1/org-pretty-table" :branch "master" :files ("*.el" "out")))

  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "ACTIVE(a)" "REFILE(r)" "FINISH(f)" "EXPLORE(e)" "HOLD(h)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(c!)")))

  ;; So it doesn't ruin window configs
  (setq org-agenda-window-setup 'current-window) 



  ;; Save org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)

  (global-set-key (kbd "C-c a") 'org-agenda)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
(setq org-agenda-start-on-weekday nil)
(setq org-reverse-note-order nil)
 '(org-fast-tag-selection-single-key (quote expert))


(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
   (quote (
	   ("w" todo "WAITING" nil)
	   ("u" alltodo ""
	    ((org-agenda-skip-function
	      (lambda nil
		(org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					  (quote regexp) "\n]+>")))
	     (org-agenda-overriding-header "Unscheduled TODO entries: ")))
	   ("d" "Daily agenda and all TODOs"
            ((tags "PRIORITY=\"A\""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                    (org-agenda-overriding-header "High-priority unfinished tasks:")))
	     (todo "NEXT" ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)))
			   (org-agenda-overriding-header "Up next: ")))
	     (agenda "" ((org-agenda-span 'week)))
	     (todo "REFILE" ((org-agenda-overriding-header "Things to refile: ")))
	     (todo "FINISH" ((org-agenda-overriding-header "Things to finish up: ")))
             (alltodo ""
                      ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                      (air-org-skip-subtree-if-priority ?A)
                                                      (org-agenda-skip-if nil '(scheduled deadline))
						      (org-agenda-skip-entry-if 'todo '("NEXT" "REFILE" "FINISH"))))
                       (org-agenda-overriding-header "All normal priority tasks:")))
	     )))))

(defun open-org-agenda ()
  (interactive)
  (org-agenda nil "d"))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (C . t)
       (latex . t)
       ))

(require 'ox-latex)

(setq org-babel-python-command "python3")
(setq org-latex-create-formula-image-program 'dvipng)

    (setq org-confirm-babel-evaluate nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t
          org-src-preserve-indentation t)
  
    (require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output"))
(add-to-list 'org-structure-template-alist '("cs" . "src C"))
(add-to-list 'org-structure-template-alist '("def" . "definicija"))
(add-to-list 'org-structure-template-alist '("izr" . "izrek"))
(add-to-list 'org-structure-template-alist '("prf" . "proof"))
(add-to-list 'org-structure-template-alist '("trd" . "trditev"))

  ;; Automatically tangle our Emacs.org config file when we save it
  (defun efs/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.emacs.d/readme.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

  (setq org-export-with-broken-links t)

  (use-package org-ref)

(use-package org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(straight-use-package
 '(org-remark
   :type git :host github :repo "nobiot/org-remark"))

(org-remark-global-tracking-mode +1)

(define-key global-map (kbd "C-c n m") #'org-remark-mark)

(with-eval-after-load 'org-remark
  (define-key org-remark-mode-map (kbd "C-c n o") #'org-remark-open)
  (define-key org-remark-mode-map (kbd "C-c n ]") #'org-remark-view-next)
  (define-key org-remark-mode-map (kbd "C-c n [") #'org-remark-view-prev)
  (define-key org-remark-mode-map (kbd "C-c n r") #'org-remark-remove))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n b " . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n d" . org-roam-dailies-map)
         ("C-c n n r" . org-roam-refile)
         ("C-c n n g" . org-id-get-create)
         ("C-c n p" . anki/my/push-notes)
         ("C-c n n p" . anki/push-all)
         ("C-c n n t" . org-roam-extract-subtree)
         ("C-c n n a" . org-roam-alias-add)
	 ("C-c n c" . org-capture-task)
	 ("C-c n n n" . org-noter)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
         ("C-c C-x C-l" . nil); Built in LaTeX previews are an annoyance with xenops.
	 ("C-c l" . org-store-link)
	 ("C-c n l" . insert-standalone-latex)
	 ("C-c n n l" . open-standalone-latex))
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

  (setq org-capture-templates
        `(("t" "Task" entry (file "~/Documents/org/tasks.org")
           "* TODO %?\n" :empty-lines 1)))

(defun org-capture-task ()
  (interactive)
  (org-capture nil "t"))

(straight-use-package
 '(org-transclusion
  :type git
  :host github
  :repo "nobiot/org-transclusion"))

(define-key global-map (kbd "C-c n t a") #'org-transclusion-add)
(define-key global-map (kbd "C-c n t n") #'org-transclusion-mode)

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
      (setq org_roam-ui-follow nil)
      (setq org-roam-ui-update-on-save t)
      (setq org-roam-ui-open-on-start nil))

(use-package org-noter)

(setq org-noter-default-notes-file-names '("notes.org"))
(setq org-noter-notes-search-path (list (concat org-roam-directory "/notes")))

    (setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ))

  (use-package vulpea)

(straight-use-package
 '(anki-editor
   :type git
   :host github
   :repo "Tevqoon/anki-editor"))

(setq anki-editor-latex-style 'mathjax)

(add-hook 'org-mode-hook (lambda () (anki-editor-mode 1)))

(defvar anki-push-times-hash-table (make-hash-table :test 'equal)
  "A hash table which keeps track of push times for all notes which anki push was called on. Is used in the function anki/my/push/notes in order to determine whether a push is necessary.")

(defun anki/my/push-notes ()
  "Checks whether the last time the file's notes were pushed is before its last edit. In this case, it pushes the file. The file's push time is updated in the hash table, and the note is saved. Useful mainly for automating pushing a large amount of files, since pushing 100+ files with up to 10 notes each can get slow. If called with C-u, pushes regardless."
  (interactive)
  (let* ((current-filename (file-name-nondirectory buffer-file-name))
	 (push-time (gethash current-filename anki-push-times-hash-table))
	 (edit-time (file-attribute-modification-time (file-attributes current-filename))))
    (if (and push-time ; There is a push time
	     (time-less-p edit-time push-time) ; And the file was last pushed after it was edited
	     (not current-prefix-arg) ; And the function was not called with C-u
	     )
	   (message "No need to push.") ; THen there is no need to push.
	   (anki-editor-push-notes) ; Otherwise push.
	   )
	 (puthash current-filename (current-time) anki-push-times-hash-table)
	 (save-buffer)
	))

(add-to-list 'closing-variables 'anki-push-times-hash-table) ; Saves the update table on save and loads it on startup.

  (use-package deft
    :config
    (setq deft-extensions '("org")
          deft-directory org-roam-directory
          deft-recursive t
          deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t)
    :bind
    ("C-c n e" . deft)
    )

(defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
      (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
        (if begin
            (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
          (deft-base-filename file))))

    (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

    (setq deft-strip-summary-regexp
          (concat "\\("
                  "[\n\t]" ;; blank
                  "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                  "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
                  "\\)"))

  (defun org/project-p ()
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

  (defun anki/flashcard-p ()
    "Returns non-nil if the current buffer has a flash card"
    (member "ANKI_NOTE_TYPE" (org-buffer-property-keys)))

    (setq prune/ignored-files
          '("20211117183951-tasks.org"
            "20211117164414-inbox.org")) ; These should always have project tags.

    (setq tag-checkers (list (cons "project"    'org/project-p)
                             (cons "flashcards" 'anki/flashcard-p)))

  (mapc (lambda (p) (add-to-list 'org-tags-exclude-from-inheritance (car p)))
        tag-checkers)

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun org-update-tag (tcpair)
       "Update '(tag . checker) tag in the current buffer."
       (when (and (not (member (buffer-name) prune/ignored-files))
                  (not (active-minibuffer-window))
                  (vulpea-buffer-p))
         (save-excursion
           (goto-char (point-min))
           (let* ((tags (vulpea-buffer-tags-get))
                  (original-tags tags))
             (if (funcall (cdr tcpair))
                 (setq tags (cons (car tcpair) tags))
               (setq tags (remove (car tcpair) tags)))

             ;; cleanup duplicates
             (setq tags (seq-uniq tags))

             ;; update tags if changed
             (when (or (seq-difference tags original-tags)
                       (seq-difference original-tags tags))
               (apply #'vulpea-buffer-tags-set tags))))))

  (defun org-update-all-tags ()
    (mapc #'org-update-tag tag-checkers))

  (add-hook 'find-file-hook #'org-update-all-tags)
  (add-hook 'before-save-hook #'org-update-all-tags)

  (defun org-project-files ()
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

   (defun anki/flashcards-files ()
    "Return a list of note files containing flashcards tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
        :from tags
        :left-join nodes
        :on (= tags:node-id nodes:id)
        :where (like tag (quote "%\"flashcards\"%"))]))))

  (defun roam-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (org-project-files)))

  (advice-add 'org-agenda :before #'roam-agenda-files-update)

    (defun anki/push-filename (filename push-anyway-p)
      "Opens the file with filename as a temporary buffer and pushes its notes. It checks whether a file needs to be pushed first, to avoid opening all of the files as buffers. As such it calls the original pushing function, since it'd be redundant to check whether it needs to be pushed twice."
      (let* ((current-filename (file-name-nondirectory filename))
	     (push-time (gethash current-filename anki-push-times-hash-table))
	     (edit-time (file-attribute-modification-time (file-attributes filename))))
	(unless (and push-time ; There is a push time
		 (time-less-p edit-time push-time) ; And the file was last pushed after it was edited
		 (not push-anyway-p) ; And we're forcing a push
		 )
	    (save-excursion
              (with-current-buffer (find-file-noselect filename)
		(progn (anki-editor-push-notes)
		       (save-buffer)
		 ))))
	  (puthash current-filename (current-time) anki-push-times-hash-table)))

    (defun anki/push-all ()
      "Maps over the files with the flashcards tag and pushes them. If called with C-u, it will pass it through and force the pushing of all of the files. Useful for updates in the pushing engine and the like. "
      (interactive)
      (if current-prefix-arg
	        (mapc (lambda (f) (anki/push-filename f t))   (anki/flashcards-files))
	        (mapc (lambda (f) (anki/push-filename f nil)) (anki/flashcards-files))
	  )
      
      ; (dump-closing-variables) ; should have this on a timer or something instead
      (message "Done pushing.")
      )

  (use-package python-mode
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

(setq TeX-command-extra-options " --shell-escape ")

(use-package cdlatex)

(setq cdlatex-command-alist
      '(("al" "Insert aligned environment" "" cdlatex-environment ("aligned") nil t)
	("bm" "Insert bmatrix environment" "" cdlatex-environment ("bmatrix") nil t)
	("se" "Insert a nice subseteq" "\\subseteq" nil nil nil t)
	("sse" "Insert a nice supseteql" "\\supseteq" nil nil nil t)
	("imp" "implies" "\\implies" nil nil nil t)
	("imb" "Implied" "\\impliedby" nil nil nil t)
	))

(add-hook 'LaTeX-mode-hook #'turn-on-org-cdlatex)
(add-hook 'org-mode-hook   #'turn-on-org-cdlatex)

  (use-package xenops)

  ;; (add-hook 'latex-mode-hook #'xenops-mode)
  ;; (add-hook 'LaTeX-mode-hook #'xenops-mode)
  (add-hook 'org-mode-hook #'xenops-mode)

  ;; The org files used are relatively small, hence we can afford to auto-expand.
  (add-hook 'org-mode-hook (lambda () (add-hook 'xenops-mode-hook #'xenops-dwim)))

  (setq xenops-reveal-on-entry t)
  (setq xenops-math-image-scale-factor 1.8) ; Macs be high res.

(defun xenops-math-latex-calculate-dpi ()
  "Calculate DPI to be used during fragment image generation."
  (* 10 (/ (round (* (org--get-display-dpi)
		     (car (xenops-math-latex-process-get :image-size-adjust))
		xenops-math-image-scale-factor)) 10)))


(setq xenops-math-latex-process 'dvisvgm)
;(setq xenops-math-latex-process 'imagemagick)
;(setq xenops-math-latex-process 'dvipng)

(defun get-current-standalones-latex (match-p)
  (completing-read
		 "Enter standalone name: "
		 (mapcar (lambda (x) (s-chop-suffix ".png" x))
			 (-filter (lambda (x) (s-suffix? ".png" x))
				  (directory-files (concat org-roam-directory "/tex"))))
		 nil
		 match-p))

(defun compile-standalone-latex (fname)
  "Compiles the fname standalone latex fragment first into pdf, then converts it to png."
  (let ((default-directory (concat org-roam-directory "/tex/")))
    (call-process-shell-command (concat "pdflatex -interaction nonstopmode -shell-escape " fname))
    (call-process-shell-command (concat "convert -density 200 -quality 100 " fname ".pdf " fname ".png"))))

(defun compile-current-standalone-latex ()
  (compile-standalone-latex
   (s-chop-suffix ".tex"
		  (s-chop-prefix (concat org-roam-directory "/tex/") buffer-file-truename))))

(defun make-new-standalone-latex (fname)
  "Makes a new file with the proper template."
  (find-file-other-window (concat org-roam-directory "/tex/" fname ".tex"))
  (yas-expand-snippet (yas-lookup-snippet "standalone_latex_template" 'latex-mode)))

(defun open-standalone-latex (fname)
  "Merely opens one of the possible snippets."
  (interactive (list (get-current-standalones-latex t)))
  (find-file-other-window (concat org-roam-directory "/tex/" fname ".tex")))

(defun insert-standalone-latex (fname)
  "Ask for user's diagram name with completions etc and insert the link to it.
If the file doesn't exist, visit a new .tex buffer and insert the right snippet."
  (interactive (list (get-current-standalones-latex nil)))
  (org-insert-link nil (concat "file:tex/" fname ".png"))
  (unless (file-exists-p (concat org-roam-directory "/tex/" fname ".tex"))
      (make-new-standalone-latex fname)))

(use-package racket-mode)

  (use-package aas
    :hook (LaTeX-mode . aas-activate-for-major-mode)
    :hook (org-mode . aas-activate-for-major-mode))

  (use-package laas
  :hook (LaTeX-mode . laas-mode))

  ;; Yasnippet settings
  (use-package yasnippet)
  (yas-global-mode 1)

  (use-package yasnippet-snippets)

  (use-package expand-region)
  (global-set-key (kbd "s-f") 'er/expand-region)

  (use-package multiple-cursors)
  (global-set-key (kbd "s-<down>") 'mc/mark-next-like-this)
  (global-set-key (kbd "s-<up>") 'mc/mark-previous-like-this)

  (global-set-key (kbd "s-M-<up>") 'mc/unmark-next-like-this)
  (global-set-key (kbd "s-M-<down>") 'mc/unmark-previous-like-this)
  
  (global-set-key (kbd "s-d") 'mc/mark-all-dwim)

  ;; Makes it so only =C-g= quits.
  (define-key mc/keymap (kbd "<return>") nil)
  (global-set-key (kbd "s-<mouse-1>") 'mc/add-cursor-on-click)
