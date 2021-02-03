;;; zaiste-theme.el
(require 'doom-themes)
(require 'org-indent)

(defgroup zaiste-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(def-doom-theme zaiste
  "A light theme inspired by Bluloco"

  ;; name        default   256       16
  ((bg         '("#f0f0f0" nil       nil            ))
   (bg-alt     '("#f0f0f0" nil       nil            ))
   (base0      '("#efefef" "#efefef" "white"        ))
   (base1      '("#e7e7e7" "#e7e7e7" "brightblack"  ))
   (base2      '("#dfdfdf" "#dfdfdf" "brightblack"  ))
   (base3      '("#c6c7c7" "#c6c7c7" "brightblack"  ))
   (base4      '("#8c8c84" "#9ca0a4" "brightblack"  ))
   (base5      '("#484a42" "#424242" "brightblack"  ))
   (base6      '("#434343" "#2e2e2e" "brightblack"  ))
   (base7      '("#2c2f34" "#1e1e1e" "brightblack"  ))
   (base8      '("#1b2229" "black"   "black"        ))
   (fg         '("#484a42" "#424242" "black"        ))
   (fg-alt     '("#c6c7c7" "#c7c7c7" "brightblack"  ))

   (grey       '("#a0a1a7" "#a0a1a7" "brightblack"  ))
   (red        '("#e45649" "#e45649" "red"          ))
   (orange     '("#da8548" "#dd8844" "brightred"    ))
   (green      '("#50a14f" "#50a14f" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#bda800" "#c5a332" "yellow"       ))
   (baby-blue  '("#d2ecff" "#d2ecff" "brightblue"   ))
   (blue       '("#0098dd" "#0098dd" "brightblue"   ))
   (dark-blue  '("#275fe4" "#275fe4" "blue"         ))
   (magenta    '("#a626a4" "#a626a4" "magenta"      ))
   (violet     '("#823ff1" "#823ff1" "brightmagenta"))
   (cyan       '("#0184bc" "#0184bc" "brightcyan"   ))
   (dark-cyan  '("#005478" "#005478" "cyan"         ))

   (highlight      blue)
   (vertical-bar   base2)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base4)
   (doc-comments   (doom-lighten comments 0.15))
   (constants      violet)
   (functions      green)
   (keywords       violet)
   (methods        cyan)
   (operators      blue)
   (type           red)
   (strings        yellow)
   (variables      magenta)
   (numbers        magenta)
   (region         baby-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 0.2))

   (modeline-bg base1)
   (modeline-bg-l base2)
   (modeline-bg-inactive (doom-darken bg 0.05))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))
   (varpitch "ETBookOT")
   (monpitch "Fira Code Light"))

  ((font-lock-comment-face :family ,monpitch
                           :foreground comments
                           :weight 'bold)
   (font-lock-doc-face :inherit 'font-lock-comment-face
                       :foreground doc-comments
                       :weight 'regular)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base8)

   (doom-modeline-bar :background highlight)
   (doom-modeline-project-dir :foreground violet :weight 'bold)
   (doom-modeline-buffer-file :weight 'regular)

   (mode-line :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt)
   (mode-line-emphasis :foreground highlight)

   (magit-blame-heading :foreground orange :background bg-alt)
   (magit-diff-removed :foreground (doom-darken red 0.2) :background (doom-blend red bg 0.1))
   (magit-diff-removed-highlight :foreground red :background (doom-blend red bg 0.2) :bold bold)

   (evil-ex-lazy-highlight :background baby-blue)

   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   (markdown-markup-face     :foreground base5)
   (markdown-header-face     :inherit 'bold :foreground red)
   (markdown-code-face       :background base1)
   (mmm-default-submode-face :background base1)

   (org-block            :family varpitch
                         :background base0)
   (org-level-1          :family varpitch
                         :foreground base8
                         :weight 'bold
                         :height 1.6)
   (org-level-2          :family varpitch
                         :foreground base6
                         :weight 'bold
                         :height 1.3)
   (org-level-3          :family varpitch
                         :foreground base5
                         :weight 'bold
                         :height 1.2)
   (org-level-4          :family varpitch
                         :foreground base4
                         :weight 'bold
                         :height 1.1)
   (org-level-5          :family varpitch
                         :foreground base4
                         :weight 'bold
                         :height 1.1)
   (org-ellipsis         :family varpitch
                         :foreground base1
                         :strike-through nil)
   (header-line          :family varpitch
                         :background nil)
   (org-quote            :family varpitch)
   (org-checkbox-statistics-done :family monpitch
                                 :foreground base2
                                 :weight 'normal)
   (org-done :foreground dark-cyan
             :weight 'normal)
   (org-headline-done :foreground base2
                      :strike-through nil)
   (org-date :family varpitch
             :foreground orange)
   (org-code :family monpitch
             :foreground dark-blue)
   (org-special-keyword :family varpitch
                        :foreground base8
                        :underline t)
   (org-document-title :family varpitch
                       :foreground base8
                       :weight 'bold
                       :height 1.8)
   (org-document-info-keyword :family varpitch
                              :foreground base4
                              :height 0.9)
   (org-block-begin-line :family varpitch
                         :foreground base4
                         :height 0.65)
   (org-meta-line :family varpitch
                  :foreground base4
                  :height 0.75)
   (org-list-dt :family varpitch
                :foreground base3)
   (org-todo :family varpitch
             :foreground fg)
   (org-hide :family monpitch
             :foreground base0
             :distant-foreground base0)
   (org-indent :family monpitch
               :foreground bg-alt
               :distant-foreground bg-alt)
   (org-link :family varpitch
             :weight 'normal
             :underline t
             :foreground base8)
   (org-special-keyword :height 0.8
                        :foreground comments
                        :family monpitch)
   (org-time-grid :family varpitch
                  :foreground comments)
   (org-agenda-current-time :family varpitch
                            :foreground dark-cyan)
   (org-agenda-structure :family varpitch
                         :height 1.4
                         :foreground fg)
   (org-agenda-date :family varpitch
                    :foreground comments
                    :height 1.1)
   (org-agenda-date-today :family varpitch
                          :foreground red
                          :height 1.8)
   (org-agenda-date-weekend :family varpitch
                            :foreground comments
                            :height 1.1)
   (org-super-agenda-header :family varpitch
                            :height 1.3)
   (org-scheduled :foreground comments)
   (org-scheduled-today :foreground yellow)
   (org-warning :foreground red)
   (org-upcoming-deadline :foreground fg)
   (org-checkbox :foreground orange)
   (org-tag :family monpitch
            :foreground comments
            :weight 'light
            :height 0.7)
   (org-table :family monpitch
              :background bg-alt
              :height 0.9)

   (org-todo-keyword-faces '(("◦" . fg)
                             ("→" . green)
                             ("⟲" . dark-cyan)
                             ("✗" . red)))

   (org-priority-faces '((65 :foreground red)
                         (66 :foreground orange)
                         (67 :foreground cyan)))

   (helm-candidate-number :background blue :foreground bg)

   (web-mode-current-element-highlight-face :background dark-blue :foreground bg)

   (wgrep-face :background base1)

   (ediff-current-diff-A        :foreground red   :background (doom-lighten red 0.8))
   (ediff-current-diff-B        :foreground green :background (doom-lighten green 0.8))
   (ediff-current-diff-C        :foreground blue  :background (doom-lighten blue 0.8))
   (ediff-current-diff-Ancestor :foreground teal  :background (doom-lighten teal 0.8))

   (tooltip :background base1 :foreground fg)

   (ivy-posframe :background base0)

   (lsp-ui-doc-background      :background base0)
   (lsp-face-highlight-read    :background (doom-blend red bg 0.3))
   (lsp-face-highlight-textual :inherit 'lsp-face-highlight-read)
   (lsp-face-highlight-write   :inherit 'lsp-face-highlight-read)

   (flycheck-error :background nil)
   (flycheck-warning :background nil)

   (font-lock-string-face :foreground constants)
   (font-lock-comment-face :foreground comments
                           :slate 'italic)

   (header-line :background nil)

   )

  ;; --- extra variables ---------------------
  ;;()
  )

;;; zaiste-theme.el ends here
