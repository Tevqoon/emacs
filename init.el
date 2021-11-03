(setq inhibit-startup-message t)

(setq frame-resize-pixelwise t)
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
;;(set-fringe-mode 10)        ; Give some breathing room

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

;; (use-package spacemacs-theme
;;   :defer t
;;   :init (load-theme 'spacemacs-light t))

(use-package doom-themes
  :defer t
  :init (load-theme 'doom-solarized-light t))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; ;; Initialize use-package on non-Linux platforms;; Add a hydra for changing text size, cool example
;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))
;; ;; Add such a hydra to the general modifier stuff
;; (comfy-space
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))
;; (unless (package-installed-p 'use-package)
;;    (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package swiper)
(use-package counsel)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :demand
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

;; Uses rainbow colors for matching parens etc
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows possible keyboard shortcuts
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Example of how to use general
;;(general-define-key
;; "M-C-j" 'counsel-switch-buffer)

(use-package general
  :config
  (general-create-definer comfy-space
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; We can thusly set custom shortcuts on the space layer
  (comfy-space
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;;Evil mode stuff, let's leave it on for now
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; C-h is backspace in insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)


;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

;; ;; Add a hydra for changing text size, cool example
;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))
;; ;; Add such a hydra to the general modifier stuff
;; (comfy-space
;;   "ts" '(hydra-text-scale/body :which-key "scale text"))

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

;; ---------------------------------------------------------

;; Set command jk to jump around paragraphs
(global-set-key (kbd "s-j") 'forward-paragraph)
(global-set-key (kbd "s-k") 'backward-paragraph)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(counsel-mode t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(exwm-floating-border-color "#D0D0E3")
 '(fci-rule-color "#4E4E4E")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#efe5da4aafb2" "#cfc5e1add08c" "#fe53c9e7b34f" "#dbb6d3c3dcf4" "#e183dee1b053" "#f944cc6dae48" "#d360dac5e06a"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   ((("#dce6e0" "color-22" "green")
     . 0)
    (("#dce8ed" "color-30" "cyan")
     . 20)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(jdee-db-active-breakpoint-face-colors (cons "#D0D0E3" "#009B7C"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#D0D0E3" "#005F00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#D0D0E3" "#4E4E4E"))
 '(lsp-ui-doc-border "#586e75")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(objed-cursor-color "#D70000")
 '(package-selected-packages
   '(forge solarized solarized-theme evil-magit magit counsel-projectile projectile hydra evil-collection multiple-cursors evil general spacemacs-theme doom-themes ivy-rich which-key rainbow-delimiters smooth-scroll swiper doom-modeline ivy command-log-mode use-package))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rustic-ansi-faces
   ["#F5F5F9" "#D70000" "#005F00" "#AF8700" "#1F55A0" "#AF005F" "#007687" "#0F1019"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#F5F5F9")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#005F00")
    (cons 40 "#3a6c00")
    (cons 60 "#747900")
    (cons 80 "#AF8700")
    (cons 100 "#bc7900")
    (cons 120 "#c96c00")
    (cons 140 "#D75F00")
    (cons 160 "#c93f1f")
    (cons 180 "#bc1f3f")
    (cons 200 "#AF005F")
    (cons 220 "#bc003f")
    (cons 240 "#c9001f")
    (cons 260 "#D70000")
    (cons 280 "#b41313")
    (cons 300 "#922727")
    (cons 320 "#703a3a")
    (cons 340 "#4E4E4E")
    (cons 360 "#4E4E4E")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
