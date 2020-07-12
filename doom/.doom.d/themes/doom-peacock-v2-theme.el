;;; doom-peacock-v2-theme.el --- Doom's Peacock theme with minor tweaks -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-peacock-v2-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-peacock-v2-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-peacock-v2-theme
  :type 'boolean)

(defcustom doom-peacock-v2-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-peacock-v2-theme
  :type 'boolean)

(defcustom doom-peacock-v2-comment-bg doom-peacock-v2-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-peacock-v2-theme
  :type 'boolean)

(defcustom doom-peacock-v2-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-peacock-v2-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-peacock-v2
  "Doom's Peacock theme with minor tweaks"

  ;; darker background (taken from doom-monokai-classic)
  ((bg         '("#272822" nil       nil          ))
   (bg-alt     '("#1D1E19" nil       nil          ))
   (base0      '("#1B2229" "black"   "black"      ))
   (base1      '("#161613" "#101010" "brightblack"))
   (base2      '("#1D1F20" "#191919" "brightblack"))
   (base3      '("#2D2E2E" "#252525" "brightblack"))
   (base4      '("#4E4E4E" "#454545" "brightblack"))
   (base5      '("#555556" "#6B6B6B" "brightblack"))
   (base6      '("#767679" "#7B7B7B" "brightblack"))
   (base7      '("#CFC0C5" "#C1C1C1" "brightblack"))
   (base8      '("#FFFFFF" "#FFFFFF" "brightwhite"))
   (fg         '("#F8F8F2" "#DFDFDF" "brightwhite"))
   (fg-alt     '("#556172" "#4D4D4D" "white"))

   (grey       base4)
   (white      '("#f8f8f0" "base4"   "base4"        ))
   ;; (red        '("#ff5d38" "#ff6655" "red"          )) ;; peacock todo 16
   (red        '("#fc6823" "#ff6655" "red"          )) ;; peacock todo 16
   ;; (orange     '("#cb4b16" "#dd8844" "brightred"    ))
   (orange     '("#FD971F" "#FD971F" "brightred")) ;; lighter
   (green      '("#98be65" "#99bb66" "green"        ))
   (teal       '("#26a6a6" "#44b9b1" "brightgreen"  )) ;; peacock
   ;; (teal       '("#01bfbf" "#44b9b1" "brightgreen"  )) ;; peacock
   (yellow     '("#bcd42a" "#ECBE7B" "yellow"       )) ;; peacock, todo 16
   (blue       '("#51afef" "#51afef" "brightblue"   ))
   (dark-blue  '("#2257A0" "#2257A0" "blue"         ))
   (magenta    '("#c678dd" "#c678dd" "magenta"      ))
   (violet     '("#a9a1e1" "#a9a1e1" "brightmagenta"))
   (cyan       '("#46D9FF" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
   (coral-popup  '("#a60033" "#f6bfbc" "coral-popup"         ))

   ;; face categories -- required for all themes
   (highlight      red)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      coral-popup)
   (builtin        red)
   (comments       (doom-darken fg 0.3)) ;; more visible comments which don't clash with existing colours
   (doc-comments   (doom-darken fg 0.3))
   ;; (comments       (if doom-peacock-v2-brighter-comments dark-cyan base5)) ;; TODO
   ;; (doc-comments   (doom-lighten (if doom-peacock-v2-brighter-comments dark-cyan base5) 0.25)) ;; TODO
   (constants      red)        ;; done
   (functions      red)     ;; done
   (keywords       teal)       ;; done
   (methods        functions)     ;; not sure how to test this.
   (operators      white)        ;; not showing up on `=` etc.
   (type           white)      ;;
   (strings        yellow)
   (variables      (doom-lighten blue 0.3))      ;; done
   (numbers        orange)        ;; done

   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base0) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (-modeline-bright doom-peacock-v2-brighter-modeline)
   (-modeline-pad
    (when doom-peacock-v2-padded-modeline
      (if (integerp doom-peacock-v2-padded-modeline) doom-peacock-v2-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken bg 0.475)
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (font-lock-comment-face
    :foreground comments
    :background (if doom-peacock-v2-comment-bg (doom-lighten bg 0.05))
    :slant 'italic)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :slant 'italic
    :weight 'bold)
   (font-lock-type-face
    :foreground type
    :slant 'italic
    :weight 'bold)

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground base7)

   ;; tooltip
   (tooltip              :background bg-alt :foreground fg)

   ;; company
   (company-tooltip            :inherit 'tooltip)
   (company-tooltip-common                           :foreground highlight)
   (company-tooltip-search     :background highlight :foreground bg :distant-foreground fg)
   (company-tooltip-selection  :background selection)
   (company-tooltip-mouse      :background magenta   :foreground bg :distant-foreground fg)
   (company-tooltip-annotation                       :foreground violet)
   (company-scrollbar-bg       :inherit 'tooltip)
   (company-scrollbar-fg       :background highlight)
   (company-preview                                  :foreground highlight)
   (company-preview-common     :background base3 :foreground magenta)
   (company-preview-search     :inherit 'company-tooltip-search)
   (company-template-field     :inherit 'match)

   ;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)

   ;; pos-tip
   (popup          :inherit 'tooltip)
   (popup-tip-face :inherit 'tooltip)


   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; rjsx-mode
   (rjsx-tag :foreground teal)
   (rjsx-attr :foreground red)

   ;; org-mode
   (org-level-1 :foreground teal :weight 'bold)
   (org-level-2 :foreground orange :weight 'bold)
   (org-level-3 :foreground violet :weight 'bold)
   (org-level-4 :foreground red :weight 'bold)
   (org-level-5 :foreground green :weight 'bold)
   (org-level-6 :foreground cyan :weight 'bold)
   (org-level-7 :foreground blue :weight 'bold)
   (org-level-8 :foreground yellow :weight 'bold)

   ;; (org-level-1 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-2 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-3 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-4 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-5 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-6 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-7 :foreground teal :weight 'bold :height 1.1)
   ;; (org-level-8 :foreground teal :weight 'bold :height 1.1)

   (org-link :foreground red :underline t)
   )


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-peacock-v2-theme.el ends here
