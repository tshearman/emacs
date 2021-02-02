;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Toby Shearman"
      user-mail-address "toby@estimatingnature.com"
      epa-file-encrypt-to '("toby@estimatingnature.com"))

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
(setq doom-font (font-spec :family "Fira Code Light" :size 18)
      doom-variable-pitch-font (font-spec :family "CMU Serif" :size 18))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-gruvbox)
(setq doom-theme 'spacemacs-light)
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(setq org-directory "~/.doom.d/org/")
(after! org
  (defvar org-inbox-file (concat org-directory "inbox.org.gpg"))
  (defvar org-projects-file (concat org-directory "projects.org.gpg"))
  (setq org-refile-allow-creating-parent-nodes t
        org-hide-emphasis-markers t
        org-latex-hyperref-template t
        org-agenda-files (file-expand-wildcards "~/.doom.d/org/*.org.gpg")
        org-refile-targets '(org-agenda-files)
        org-todo-keyword-faces '(("◦" . "yellow")
                                 ("→" . "green")
                                 ("⟲" . "blue")
                                 ("✗" . (:foreground "red" :weight bold)))
        org-todo-keywords '((sequence "◦(t!)" "→(s!)" "|" "✔(d!)" "✗(c@!)" "⟲(w@!)")
                            (sequence "❀(i)" "|" "✔(d!)" "✗(c@!)" "⟲(w@!)"))
        org-capture-templates '(("i" "Inbox" entry (file+headline org-inbox-file "Inbox")
                                 "* ◦ %i%?")
                                ("!" "Idea" entry (file+headline org-inbox-file "Ideas!")
                                 "* ❀ %i%?")
                                ("l" "Literature" entry (file+headline org-inbox-file "Literature")
                                 "* ◦[%^g] %i%?")
                                ("p" "Project" entry (file_headline org-projects-file "Inbox")
                                 "* %i%?"))
        org-startup-indented 'indent
        org-startup-folded 'content
        org-src-tab-acts-natively t
        org-enforce-todo-dependencies t
        org-log-done (quote time)
        org-log-redeadline (quote time)
        org-log-reschedule (quote time)
        org-superstar-headline-bullets-list '("◉" "⁖" "⁖" "⁖" "⁖" "⁖")
        org-tag-alist '(("weekly" . ?W)
                        ("life" . ?l)
                        ("projects" . ?p)
                        ("literature" . ?l)
                        ("ttrpg" . ?g)
                        ("thoughts" . ?t))
        ispell-program-name "/usr/bin/aspell"
        org-archive-location "%s_archive.gpg::")

  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'turn-off-auto-fill)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
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
                    ((org-agenda-start-day "-7d")
                     (org-agenda-span 14)
                     (org-agenda-start-on-weekday 0)
                     (org-agenda-start-with-log-mode '(closed))
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "^\\*\\* ✔ "))))
          ("v" "View"
           ((agenda ""
                    ((org-agenda-overriding-header "\n\nAgenda ====================")
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
  :init
  (setq org-roam-encrypt-files t
        org-roam-directory (concat org-directory "roam/")
        org-roam-dailies-directory (concat org-directory "dailies/")
        org-roam-index-file (concat org-roam-directory "index.org.gpg")
        org-roam-capture-templates '(("d" "default" plain (function org-roam-capture--get-point)
                                      "%?"
                                      :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                      :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n"
                                      :unnarrowed t)
                                     ("l" "latex" plain (function org-roam-capture--get-point)
                                      "\n- tags :: %?\n\n\n\n\nbibliographystyle:humannat\nbibliography:../../references/bazaar"
                                      :file-name "%<%Y%m%d%H%M%S>-${slug}"
                                      :head "#+TITLE: ${title}\n#+ROAM_ALIAS: \n#+ROAM_TAGS: \n"))
        org-roam-dailies-capture-templates '(("d" "default" entry #'org-roam-capture--get-point
                                              "* %?"
                                              :file-name "dailies/%<%Y-%m-%d>"
                                              :head "#+TITLE: %<%Y-%m-%d>\n\n")))
  :hook
  (after-init . org-roam-mode)
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

;; Interactive Org Roam Server Graph

;; Interactive Org Roam Server Graph
(use-package org-roam-server
  :ensure t
  :load-path "~/.org-roam-server"
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port)))

;; automatically enable server-mode
(after! org-roam
    (org-roam-db-build-cache)
    (require 'simple-httpd)
    (setq httpd-root "/var/www")
    (httpd-start)
    (org-roam-server-mode))

(use-package org-journal
  :after org-roam
  :bind
  ("C-c n j" . org-journal-new-entry)
  :init
  (setq org-journal-enable-agenda-integration t
        org-journal-dir org-roam-directory
        org-journal-date-prefix "#+TITLE: "
        org-journal-file-format "%Y-%m-%d.org.gpg"
        org-journal-date-format "%A, %d %B %Y"))

(use-package deft
      :after org
      :bind
      ("C-c n d" . deft)
      :custom
      (deft-auto-save-interval 0)
      (deft-recursive t)
      (deft-use-filter-string-for-filename t)
      (deft-default-extension "org.gpg")
      (deft-extensions '("org" "org.gpg" "gpg"))
      (deft-directory (concat org-directory "roam")))

(defun my/org-open-new-window (path)
  "Open info in a new buffer"
  (setq available-windows
        (delete (selected-window) (window-list)))
  (setq new-window
         (or (car available-windows)
             (split-window-right)))
  (select-window new-window)
  (org-open-file path))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (my/org-open-new-window pdf-file)
      (message "No PDF found for %s" key))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-default-bibliography '("~/.doom.d/references/bazaar.bib")
        reftex-plug-intoAUCTex t))

(use-package org-ref
  :after org
  :init
  (setq org-ref-bibliography-notes '("~/.doom.d/references/notes.org")
        org-ref-default-bibliography '("~/.doom.d/references/bazaar.bib")
        bibtex-completion-bibliography '("~/.doom.d/references/bazaar.bib")
        bibtex-completion-pdf-field "file"
        org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point))

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
          (""     "hyperref"  t)
          (""     "graphicx"  t)
          (""     "grffile"   t)
          (""     "longtable" nil)
          (""     "wrapfig"   nil)
          (""     "rotating"  nil)
          ("normalem" "ulem"  t)
          (""     "amsmath"   t)
          (""     "natbib"    t)
          (""     "textcomp"  t)
          (""     "amssymb"   t)
          (""     "capt-of"   nil))))

(setq org-latex-pdf-process
    '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

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

 ;; Variables
(setq
 bg-white           "#fbf8ef"
 bg-light           "#222425"
 bg-dark            "#1c1e1f"
 bg-darker          "#1c1c1c"
 fg-white           "#ffffff"
 shade-white        "#efeae9"
 fg-light           "#655370"
 dark-cyan          "#008b8b"
 region-dark        "#2d2e2e"
 region             "#39393d"
 slate              "#8FA1B3"
 keyword            "#f92672"
 comment            "#525254"
 builtin            "#fd971f"
 purple             "#9c91e4"
 doc                "#727280"
 type               "#66d9ef"
 string             "#b6e63e"
 gray-dark          "#999"
 gray               "#bbb"
 sans-font          "Fira Code Light"
 serif-font         "Fira Code Light"
 et-font            "CMU Serif"
 sans-mono-font     "Fira Code Light"
 serif-mono-font    "Fira Code Light")

;; ;; Settings
;; ;;(custom-theme-set-faces
(use-package doom-themes
  :custom-face
  (org-level-1 ((t (:family ,et-font
                    :height 1.6
                    :weight normal
                    :slant normal
                    :foreground ,bg-dark))))
  (org-level-2 ((t (:family ,et-font
                    :height 1.3
                    :weight normal
                    :slant normal
                    :foreground ,bg-dark))))
  (org-level-3 ((t (:family ,et-font
                    :weight normal
                    :slant italic
                    :height 1.2
                    :foreground ,bg-dark))))
  (org-level-4 ((t (:family ,et-font
                    :weight normal
                    :slant italic
                    :height 1.1
                    :foreground ,bg-dark))))
  (org-level-5 ((t (:family ,et-font
                    :weight bold
                    :height 1.1
                    :foreground ,slate))))
  (org-level-6 ((t (:family ,et-font
                    :weight bold
                    :height 1.1
                    :foreground ,slate))))
  (org-level-7 ((t (:family ,et-font
                    :weight bold
                    :height 1.1
                    :foreground ,slate))))
  (org-level-8 ((t (:family ,et-font
                    :weight bold
                    :height 1.1
                    :foreground ,slate))))
  (header-line ((t (:family ,et-font
                    :background nil))))
  (org-document-title ((t (:family ,et-font
                           :height 1.8
                           :foreground ,bg-dark
                           :underline nil))))
  (org-document-info ((t (:family ,et-font
                          :foreground ,gray
                          :slant italic
                          :height 1.2))))
  (org-headline-done ((t (:family ,et-font
                          :foreground ,gray
                          :strike-through t))))
  (org-quote ((t (:family ,et-font))))
  (org-block ((t (:family ,et-font
                  :background nil
                  :foreground: ,bg-dark))))
  (org-block-begin-line ((t (:family ,sans-mono-font
                             :height 0.8
                             :foreground ,slate))))
  (org-block-end-line ((t (:family ,sans-mono-font
                           :height 0.8
                           :foreground ,slate
                           :background nil))))

  (org-document-info-keyword ((t (:family ,et-font
                                  :foreground ,comment
                                  :height 0.8))))
  (org-link ((t (:family ,et-font
                 :underline nil
                 :weight normal
                 :foreground ,bg-dark))))
  (org-special-keyword ((t (:height 0.8
                            :foreground ,comment
                            :family ,sans-mono-font))))
  (org-todo ((t (:family ,et-font
                 :foreground ,builtin))))
  (org-done ((t (:foreground ,dark-cyan,
                 :family ,et-font))))
  (org-agenda-current-time ((t (:family ,et-font
                                :foreground ,slate))))
  (org-hide ((t (:family ,et-font
                 :foreground ,bg-white))))
  (org-time-grid ((t (:family ,et-font
                      :foreground ,comment))))
  (org-warning ((t (:family ,et-font
                    :foreground ,builtin))))
  (org-date ((t (:family ,sans-mono-font
                 :height 0.8))))
  (org-agenda-structure ((t (:family ,et-font
                             :height 1.3
                             :foreground ,doc
                             :weight normal))))
  (org-agenda-date ((t (:family ,et-font
                        :foreground ,doc
                        :height 1.1))))
  (org-agenda-date-today ((t (:family ,et-font
                              :foreground ,keyword
                              :height 1.5))))
  (org-scheduled ((t (:family ,et-font
                      :foreground ,gray))))
  (org-deadline ((t (:family ,et-font))))
  (org-upcoming-deadline ((t (:family ,et-font
                              :foreground ,keyword))))
  (org-schedule-today ((t (:family ,et-font
                           :foreground ,slate))))
  (org-scheduled-previously ((t (:family ,et-font
                                 :foreground ,slate))))
  (org-agenda-done ((t (:family ,et-font
                        :foreground ,doc
                        :strike-through t))))
  (org-ellipsis ((t (:family ,et-font
                     :foreground ,comment))))
  (org-tag ((t (:family ,sans-mono-font
                :foreground ,doc))))
  (org-table ((t (:family ,serif-mono-font
                  :background ,bg-white
                  :height 0.9))))
  (org-code ((t (:family ,serif-mono-font
                 :foreground ,comment
                 :height 0.9))))
  )

;;    (dired-subtree-depth-1-face
;;    (:background nil)
;;    nil)
;;    (dired-subtree-depth-2-face
;;    (:background nil)
;;    nil)
;;    (dired-subtree-depth-3-face
;;    (:background nil)
;;    nil)
;;    (dired-subtree-depth-4-face
;;    (:background nil)
;;    nil)
;;    (dired-subtree-depth-5-face
;;    (:background nil)
;;    nil)
;;    (dired-subtree-depth-6-face
;;    (:background nil)
;;    nil)
;;    (nlinum-current-line
;;    (:foreground ,builtin)
;;    (:foreground ,bg-dark))
;;    (vertical-border
;;    (:background ,region
;;                 :foreground ,region)
;;    nil)
;;    (which-key-command-description-face
;;    (:foreground ,type)
;;    nil)
;;    (flycheck-error
;;    (:background nil)
;;    nil)
;;    (flycheck-warning
;;    (:background nil)
;;    nil)
;;    (font-lock-string-face
;;    (:foreground ,string)
;;    nil)
;;    (font-lock-comment-face
;;    (:foreground ,doc
;;                 :slant italic)
;;    (:background nil
;;                 :foreground ,doc
;;                 :slant italic))
;;    (region
;;    (:background ,region)
;;    nil)
;;    (header-line
;;    (:background nil
;;                 :inherit nil)
;;    (:background nil
;;                 :inherit nil)))
