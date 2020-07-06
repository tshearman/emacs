;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Toby Shearman"
      user-mail-address "toby@estimatingnature.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code Light" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(after! org
  (setq org-directory "~/.doom.d/org/")
  (defvar org-journal (concat org-directory "journal.org"))
  (defvar org-work (concat org-directory "work.org"))
  (defvar org-inbox (concat org-directory "inbox.org"))
  (defvar org-projects (concat org-directory "projects.org"))
  (setq org-hide-emphasis-markers t
        org-latex-hyperref-template t
        org-agenda-files (list org-inbox
                               org-work
                               org-projects)
        org-refile-targets '(org-agenda-files)
        org-todo-keyword-faces '(("·" . "green")
                                 ("→" . "yellow")
                                 ("⟲" . "yellow")
                                 ("ⓧ" . (:foreground "blue" :weight bold)))
        org-todo-keywords '((sequence "·(t!)" "→(s!)" "|" "✓(d!)" "/(c@!)" "⟲(w@!)")
                            (sequence "idea(i)" "|" "✓(d!)" "ⓧ(c@!)" "⟲(w@!)"))
        org-capture-templates '(("i" "Inbox" entry
                                 (file+headline org-inbox "Inbox")
                                 "* · %i%?")
                                ("j" "Journal" entry
                                 (file+olp+datetree org-journal)
                                 "* [%<%H:%M>][%^g]\n%?\n")
                                ("w" "Work" entry
                                 (file+olp+datetree org-work)
                                 "* [%<%H:%M>][%^g]\n%?\n")
                                ("l" "Literature" entry
                                 (file+headline org-inbox "Literature ")
                                 "* ·[%^g] %i%?"))
        org-startup-indented 'indent
        org-startup-folded 'content
        org-src-tab-acts-natively t
        org-enforce-todo-dependencies t
        org-log-done (quote time)
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-tag-alist '(("work" . ?w)
                        ("life" . ?l)
                        ("projects" . ?p)
                        ("ttrpg" , ?g)
                        ("thoughts", ?t))
        ispell-program-name "/usr/local/bin/aspell")

  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'turn-off-auto-fill)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c x") 'org-capture))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-block-separator nil
        org-agenda-compact-blocks t
        org-agenda-start-day "-1d"
        org-agenda-start-on-weekday nil
        org-agenda-custom-commands
        '(("r" "Review"
           agenda ""
                    ((org-agenda-start-day "-13d")
                     (org-agenda-span 14)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-start-with-log-mode '(closed))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "^\\*\\* ✓ "))))
          ("v" "View"
           ((agenda ""
                    ((org-agenda-overriding-header "\nAgenda ====================")
                     (org-agenda-start-day "today")
                     (org-agenda-span 1)
                     (org-super-agenda-groups
                      '((:name "Tasks"
                         :time-grid t
                         :todo "→")
                        (:name "Today"
                         :time-grid t
                         :date today
                         :scheduled today
                         :order 1)
                        (:name "Waiting"
                         :time-grid t
                         :todo "⟲")
                        (:name "Important"
                         :priority "A")))))
            (alltodo ""
                     ((org-agenda-overriding-header "\n\nTasks ====================")
                      (org-super-agenda-groups
                       '((:name "Important"
                          :priority "A"
                          :order 6)
                         (:name "Due Today"
                          :deadline today
                          :order 2)
                         (:name "Due Soon"
                          :deadline future
                          :order 8)
                         (:name "Overdue"
                          :deadline past
                          :order 7)
                         (:name "Projects"
                          :tag "project"
                          :order 14)
                         (:name "Research"
                          :tag "research"
                          :order 15)
                         (:name "To Read"
                          :tag "literature"
                          :order 30)
                         (:name "Waiting"
                          :todo "⟲"
                          :order 20)
                         (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
  :config
  (org-super-agenda-mode))

(use-package org-roam
  :after org-super-agenda
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  (org-roam-index-file (concat org-roam-directory "index.org"))
  :bind
  (:map org-roam-mode-map
   (("C-c n l" . org-roam)
    ("C-c n f" . org-roam-find-file)
    ("C-c n j" . org-roam-jump-to-index)
    ("C-c n b" . org-roam-switch-to-buffer)
    ("C-c n g" . org-roam-graph))
   :map org-mode-map
   (("C-c n i" . org-roam-insert)))
  :config
  (org-roam-mode))

(use-package org-journal
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-dir (concat org-directory "journal"))
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("amsart" "\\documentclass[10pt]{amsart}"
                 ("\\section*{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("amsjournal" "\\documentclass[10pt]{amsbook}"
                 ("\\chapter*{%s}" . "\\chapter*{%s}")
                 ("\\section*{%s}" . "\\section*{%s}")
                 ("\\subsection*{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection*{%s}" . "\\subsubsection*{%s}")))
  
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc"  t ("pdflatex"))
          ("T1"   "fontenc"   t ("pdflatex"))
          (""     "graphicx"  t)
          (""     "grffile"   t)
          (""     "longtable" nil)
          (""     "wrapfig"   nil)
          (""     "rotating"  nil)
          ("normalem" "ulem"  t)
          (""     "amsmath"   t)
          (""     "textcomp"  t)
          (""     "amssymb"   t)
          (""     "capt-of"   nil))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;;syntax highlight code blocks
(setq org-src-fontify-natively t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Set default font faces for Org mode
;; (add-hook 'org-mode-hook (lambda ()
;;                             (setq buffer-face-mode-face '(:family "CMU Serif"))
;;                             (buffer-face-mode)))
