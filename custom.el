(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" default))
 '(org-agenda-files
   '("/Users/jure/Documents/org/20220324190857-fizika_2.org" "/Users/jure/Documents/org/20211201003254-financial.org" "/Users/jure/Documents/org/20220419144537-willpower_hax_487_execute_by_default_lesswrong.org" "/Users/jure/Documents/org/20220417165421-william_shockley_on_what_makes_a_person_who_publishes_a_lot_of_papers_and_the_superstar_researcher_system_dynamic_ecology.org" "/Users/jure/Documents/org/20220401083315-osnovni_izreki_topologije_evklidskih_prostorov.org" "/Users/jure/Documents/org/20220218101523-algebra_3.org" "/Users/jure/Documents/org/20220215120506-ugt.org" "/Users/jure/Documents/org/20220214101949-analiza_2b.org" "/Users/jure/Documents/org/20220213234052-anki.org" "/Users/jure/Documents/org/daily/2022-05-15.org" "/Users/jure/Documents/org/20220324185755-analiza_2a.org" "/Users/jure/Documents/org/inbox.org" "/Users/jure/Documents/org/20220225084107-deljivost_topoloskih_lastnosti.org" "/Users/jure/Documents/org/20220408084615-jordan_brouwerjev_delilni_izrek.org" "/Users/jure/Documents/org/tasks.org" "/Users/jure/Documents/org/20220518162237-mobiusove_transformacije.org" "/Users/jure/Documents/org/20220311094051-hausdorff_space.org" "/Users/jure/Documents/org/20220417165421-how_i_am_productive_lesswrong.org" "/Users/jure/Documents/org/20211116204149-meta_notetaking.org" "/Users/jure/Documents/org/20220331173447-ugt_vaje.org" "/Users/jure/Documents/org/20220417165421-every_productivity_thought_i_ve_ever_had_as_concisely_as_possible_alexey_guzey.org" "/Users/jure/Documents/org/20220422114703-the_replacing_guilt_series_notes.org" "/Users/jure/Documents/org/20220331093242-analiza_2b_vaje.org" "/Users/jure/Documents/org/20220518105210-holomorfne_funkcije_kot_preslikave_iz_c_v_c.org" "/Users/jure/Documents/org/20211125224815-goals.org" "/Users/jure/Documents/org/20211125032426-emacs.org" "/Users/jure/Documents/org/20220328190452-exercise.org" "/Users/jure/Documents/org/20220309101827-orientabilnost.org" "/Users/jure/Documents/org/20220513083713-poliedrske_ploskve.org"))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
	   (lambda nil
	     (unless
		 (org-map-entries #'org-heading-components)
	       (my/delete-current-file-no-ask-danger)))
	   nil t)
     (eval add-hook 'after-save-hook
	   (lambda nil
	     (let
		 ((entries
		   (org-map-entries #'org-element-at-point nil)))
	       (unless entries
		 (my/delete-current-file-no-ask-danger)))))
     (org-archive-location . "archive.org::* %s")
     (setq org-archive-location "archive.org::* %s")
     (eval add-hook 'after-save-hook #'compile-current-standalone-latex nil t)
     (TeX-command-extra-options . " --shell-escape ")))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((websocket) (websocket) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Menlo" :height 140))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1" :height 1.5 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1" :height 1.25))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1" :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#556b72" :font "-*-Brygada 1918-semibold-normal-normal-*-*-*-*-*-p-0-iso10646-1"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "Brygada 1918" :height 155)))))
