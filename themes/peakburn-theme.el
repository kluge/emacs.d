;;; peakburn-theme.el --- A color theme for Emacs.

;; Copyright (C) 2013-2015 Kari Oikarinen
;; Copyright (C) 2011-2013 Bozhidar Batsov

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A derivative of the Zenburn color theme for Emacs by Bozhidar
;; Batsov.

;;; Credits:

;; Jani Nurminen created the original Zenburn theme and Pan Shizhu
;; created the Peaksea color theme on which this theme is based.

;;; Code:

(deftheme peakburn "The Peakburn color theme")

;;; Color Palette

(defvar peakburn-colors-alist
  '(("peakburn-fg"       . "#D0D0D0")
    ("peakburn-fg-1"     . "#B0B0B0")
    ("peakburn-fg-2"     . "#707070")
    ("peakburn-bg-1"     . "#101010")
    ("peakburn-bg-05"    . "#181818")
    ("peakburn-bg"       . "#202020")
    ("peakburn-bg+1"     . "#303030")
    ("peakburn-bg+2"     . "#404040")
    ("peakburn-bg+3"     . "#505050")
    ("peakburn-red"      . "#CC9393")
    ("peakburn-red-1"    . "#BC8383")
    ("peakburn-red-2"    . "#AC7373")
    ("peakburn-red-3"    . "#9C6363")
    ("peakburn-red-4"    . "#8C5353")
    ("peakburn-orange"   . "#E0C060")
    ("peakburn-yellow"   . "#D0D090")
    ("peakburn-yellow-1" . "#D0D070")
    ("peakburn-yellow-2" . "#C0B060")
    ("peakburn-green-1"  . "#208030")
    ("peakburn-green"    . "#40A060")
    ("peakburn-green+1"  . "#60B080")
    ("peakburn-green+2"  . "#60C080")
    ("peakburn-green+3"  . "#60D080")
    ("peakburn-green+4"  . "#60F080")
    ("peakburn-puregreen" . "#00F000")
    ("peakburn-cyan-1"   . "#50B0D0")
    ("peakburn-cyan"     . "#80C0E0")
    ("peakburn-blue+2"   . "#A0D0FF")
    ("peakburn-blue+1"   . "#90C0FF")
    ("peakburn-blue"     . "#70A0F0")
    ("peakburn-blue-1"   . "#6080E0")
    ("peakburn-blue-2"   . "#5070D0")
    ("peakburn-blue-3"   . "#3050B0")
    ("peakburn-blue-4"   . "#2040A0")
    ("peakburn-magenta"  . "#800080")
    ("peakburn-pink"     . "#F0C0F0"))
  "List of Peakburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro peakburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `peakburn-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   peakburn-colors-alist))
     ,@body))

;;; Theme Faces
(peakburn-with-color-variables
  (custom-theme-set-faces
   'peakburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,peakburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,peakburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,peakburn-fg :background ,peakburn-bg))))
   `(cursor ((t (:foreground ,peakburn-fg :background ,peakburn-puregreen))))
   `(escape-glyph ((t (:foreground ,peakburn-yellow :bold t))))
   `(fringe ((t (:foreground ,peakburn-fg :background ,peakburn-bg+1))))
   `(header-line ((t (:foreground ,peakburn-yellow
                                  :background ,peakburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,peakburn-bg+1))))
   `(success ((t (:foreground ,peakburn-green :weight bold))))
   `(warning ((t (:foreground ,peakburn-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,peakburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,peakburn-green))))
   `(compilation-error-face ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,peakburn-fg))))
   `(compilation-info-face ((t (:foreground ,peakburn-blue))))
   `(compilation-info ((t (:foreground ,peakburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,peakburn-green))))
   `(compilation-line-face ((t (:foreground ,peakburn-yellow))))
   `(compilation-line-number ((t (:foreground ,peakburn-fg-1))))
   `(compilation-message-face ((t (:foreground ,peakburn-blue))))
   `(compilation-warning-face ((t (:foreground ,peakburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,peakburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,peakburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,peakburn-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,peakburn-fg))))
   `(grep-error-face ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,peakburn-blue))))
   `(grep-match-face ((t (:foreground ,peakburn-orange :weight bold))))
   `(match ((t (:backgrond ,peakburn-bg-1 :foreground ,peakburn-orange :weight bold))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,peakburn-green
                      :background ,peakburn-bg
                      :underline nil))))
   `(helm-source-header
     ((t (:foreground ,peakburn-yellow
                      :background ,peakburn-bg-1
                      :underline nil
                      :weight bold))))
   `(helm-selection ((t (:background ,peakburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,peakburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,peakburn-bg :background ,peakburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,peakburn-green+4 :background ,peakburn-bg-1))))
   `(helm-separator ((t (:foreground ,peakburn-red :background ,peakburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,peakburn-red :background ,peakburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,peakburn-orange :background ,peakburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,peakburn-magenta :background ,peakburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,peakburn-yellow :background ,peakburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,peakburn-magenta :background ,peakburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,peakburn-red :background ,peakburn-bg))))
   `(helm-buffer-process ((t (:foreground ,peakburn-cyan :background ,peakburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,peakburn-fg :background ,peakburn-bg))))
   `(helm-buffer-size ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg))))
   `(helm-ff-directory ((t (:foreground ,peakburn-cyan :background ,peakburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,peakburn-fg :background ,peakburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,peakburn-red :background ,peakburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,peakburn-yellow :background ,peakburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,peakburn-bg :background ,peakburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,peakburn-cyan :background ,peakburn-bg))))
   `(helm-grep-file ((t (:foreground ,peakburn-fg :background ,peakburn-bg))))
   `(helm-grep-finish ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,peakburn-red :background ,peakburn-bg))))
   `(helm-moccur-buffer ((t (:foreground ,peakburn-cyan :background ,peakburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,peakburn-fg :background ,peakburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,peakburn-fg :background ,peakburn-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,peakburn-yellow :background ,peakburn-bg+2 :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,peakburn-yellow-2 :weight bold :background ,peakburn-bg-1))))
   `(isearch-fail ((t (:foreground ,peakburn-fg :background ,peakburn-red-4))))
   `(lazy-highlight ((t (:foreground ,peakburn-yellow-2 :weight bold :background ,peakburn-bg-05))))

   `(menu ((t (:foreground ,peakburn-fg :background ,peakburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,peakburn-yellow))))
   `(mode-line
     ((,class (:foreground ,peakburn-green+1
                           :background ,peakburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,peakburn-green+1 :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,peakburn-green-1
                      :background ,peakburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,peakburn-bg+3))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,peakburn-bg+2))))
   `(trailing-whitespace ((t (:background ,peakburn-red))))
   `(vertical-border ((t (:foreground ,peakburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,peakburn-cyan-1))))
   `(font-lock-comment-face ((t (:foreground ,peakburn-yellow))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,peakburn-yellow-1))))
   `(font-lock-constant-face ((t (:foreground ,peakburn-cyan))))
   `(font-lock-doc-face ((t (:foreground ,peakburn-green+1))))
   `(font-lock-doc-string-face ((t (:foreground ,peakburn-blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,peakburn-blue))))
   `(font-lock-keyword-face ((t (:foreground ,peakburn-blue+2 :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,peakburn-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,peakburn-blue+1))))
   `(font-lock-string-face ((t (:foreground ,peakburn-green))))
   `(font-lock-type-face ((t (:foreground ,peakburn-blue+1))))
   `(font-lock-variable-name-face ((t (:foreground ,peakburn-pink))))
   `(font-lock-warning-face ((t (:foreground ,peakburn-orange :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,peakburn-puregreen :background ,peakburn-bg :inverse-video nil))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,peakburn-fg))))
   `(ack-file ((t (:foreground ,peakburn-blue))))
   `(ack-line ((t (:foreground ,peakburn-yellow))))
   `(ack-match ((t (:foreground ,peakburn-orange :background ,peakburn-bg-1 :weight bold))))
;;;;; auctex
   `(font-latex-bold ((t (:inherit bold))))
   `(font-latex-warning ((t (:inherit font-lock-warning))))
   `(font-latex-sedate ((t (:foreground ,peakburn-yellow :weight bold ))))
   `(font-latex-title-4 ((t (:inherit variable-pitch :weight bold))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,peakburn-bg+3 :foreground "black"))))
   `(ac-selection-face ((t (:background ,peakburn-blue-3 :foreground ,peakburn-fg))))
   `(popup-tip-face ((t (:background ,peakburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,peakburn-blue-4))))
   `(popup-scroll-bar-background-face ((t (:background ,peakburn-bg-1))))
   `(popup-isearch-match ((t (:background ,peakburn-bg :foreground ,peakburn-fg))))
;;;;; company
   `(company-tooltip ((t (:background ,peakburn-bg+3 :foreground "black"))))
   `(company-tooltip-selection ((t (:background ,peakburn-blue-3 :foreground ,peakburn-fg))))
   `(company-tooltip-common ((t (:background ,peakburn-bg+3 :foreground "black"))))
   `(company-tooltip-common-selection ((t (:foreground ,peakburn-fg))))
   `(company-scrollbar-fg ((t (:background ,peakburn-blue-4))))
   `(company-scrollbar-bg ((t (:background ,peakburn-bg-1))))
   `(company-preview ((t (:background ,peakburn-bg+1 :foreground ,peakburn-fg-2))))
   `(company-preview-common ((t (:background ,peakburn-bg+1 :foreground ,peakburn-fg-2))))
;;;;; diff
   `(diff-added ((,class (:foreground ,peakburn-green+4 :background nil))
                 (t (:foreground ,peakburn-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,peakburn-yellow))))
   `(diff-removed ((,class (:foreground ,peakburn-red :background nil))
                   (t (:foreground ,peakburn-red-3 :background nil))))
   `(diff-refine-added ((t :inherit diff-added :weight bold)))
   `(diff-refine-change ((t :inherit diff-changed :weight bold)))
   `(diff-refine-removed ((t :inherit diff-removed :weight bold)))
   `(diff-header ((,class (:background ,peakburn-bg+2))
                  (t (:background ,peakburn-fg :foreground ,peakburn-bg))))
   `(diff-file-header
     ((,class (:background ,peakburn-bg+2 :foreground ,peakburn-fg :bold t))
      (t (:background ,peakburn-fg :foreground ,peakburn-bg :bold t))))
;;;;; flycheck
   `(flycheck-error-face ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
   `(flycheck-warning-face ((t (:foreground ,peakburn-orange :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate ((t (:foreground ,peakburn-orange :weight bold :underline t))))
   `(flyspell-incorrect ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,peakburn-bg+1))
                   (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,peakburn-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,peakburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,peakburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,peakburn-yellow))))
;;;;; linum-mode
   `(linum ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg+1))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,peakburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,peakburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,peakburn-bg+1))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment))))
   `(message-header-name ((t (:foreground ,peakburn-green+1))))
   `(message-header-other ((t (:foreground ,peakburn-green))))
   `(message-header-to ((t (:foreground ,peakburn-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,peakburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,peakburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,peakburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,peakburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,peakburn-green))))
   `(message-mml ((t (:foreground ,peakburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,peakburn-cyan :background ,peakburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,peakburn-bg :background ,peakburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,peakburn-bg :background ,peakburn-red :weight bold))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,peakburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,peakburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,peakburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,peakburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,peakburn-blue-3  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,peakburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,peakburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,peakburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,peakburn-bg+3 :strike-through t))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,peakburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,peakburn-bg+2 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,peakburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,peakburn-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,peakburn-green+3))))
   `(org-formula ((t (:foreground ,peakburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,peakburn-green+3))))
   `(org-hide ((t (:foreground ,peakburn-bg-1))))
   `(org-level-1 ((t (:foreground ,peakburn-orange))))
   `(org-level-2 ((t (:foreground ,peakburn-green+4))))
   `(org-level-3 ((t (:foreground ,peakburn-blue-1))))
   `(org-level-4 ((t (:foreground ,peakburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,peakburn-cyan))))
   `(org-level-6 ((t (:foreground ,peakburn-green+2))))
   `(org-level-7 ((t (:foreground ,peakburn-red-4))))
   `(org-level-8 ((t (:foreground ,peakburn-blue-3))))
   `(org-link ((t (:foreground ,peakburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,peakburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,peakburn-red-4))))
   `(org-scheduled-today ((t (:foreground ,peakburn-blue+1))))
   `(org-special-keyword ((t (:foreground ,peakburn-fg-1 :weight normal))))
   `(org-table ((t (:foreground ,peakburn-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,peakburn-orange))))
   `(org-todo ((t (:bold t :foreground ,peakburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,peakburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,peakburn-bg-1))))
   `(org-column-title ((t (:background ,peakburn-bg-1 :underline t :weight bold))))
;;;;; outline
   `(outline-1 ((t (:foreground ,peakburn-orange))))
   `(outline-2 ((t (:foreground ,peakburn-green+4))))
   `(outline-3 ((t (:foreground ,peakburn-blue-1))))
   `(outline-4 ((t (:foreground ,peakburn-yellow-2))))
   `(outline-5 ((t (:foreground ,peakburn-cyan))))
   `(outline-6 ((t (:foreground ,peakburn-green+2))))
   `(outline-7 ((t (:foreground ,peakburn-red-4))))
   `(outline-8 ((t (:foreground ,peakburn-blue-3))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,peakburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,peakburn-green+2))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,peakburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,peakburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,peakburn-green-1))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,peakburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,peakburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,peakburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,peakburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,peakburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,peakburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,peakburn-blue-4))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,peakburn-red-3 :background ,peakburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,peakburn-blue-1 :background ,peakburn-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; term
   `(term-color-black ((t (:foreground ,peakburn-bg
                                       :background ,peakburn-bg-1))))
   `(term-color-red ((t (:foreground ,peakburn-red-2
                                     :background ,peakburn-red-4))))
   `(term-color-green ((t (:foreground ,peakburn-green
                                       :background ,peakburn-green+2))))
   `(term-color-yellow ((t (:foreground ,peakburn-orange
                                        :background ,peakburn-yellow))))
   `(term-color-blue ((t (:foreground ,peakburn-blue-1
                                      :background ,peakburn-blue-3))))
   `(term-color-magenta ((t (:foreground ,peakburn-magenta
                                         :background ,peakburn-red))))
   `(term-color-cyan ((t (:foreground ,peakburn-cyan
                                      :background ,peakburn-blue))))
   `(term-color-white ((t (:foreground ,peakburn-fg
                                       :background ,peakburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,peakburn-bg-05))))
   ))

;;; Theme Variables
(peakburn-with-color-variables
  (custom-theme-set-variables
   'peakburn
;;;;; ansi-color
   `(ansi-color-names-vector [,peakburn-bg ,peakburn-red ,peakburn-green ,peakburn-yellow
                                          ,peakburn-blue ,peakburn-magenta ,peakburn-cyan ,peakburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,peakburn-bg-05)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,peakburn-red-1)
       ( 40. . ,peakburn-red)
       ( 60. . ,peakburn-orange)
       ( 80. . ,peakburn-yellow-2)
       (100. . ,peakburn-yellow-1)
       (120. . ,peakburn-yellow)
       (140. . ,peakburn-green-1)
       (160. . ,peakburn-green)
       (180. . ,peakburn-green+1)
       (200. . ,peakburn-green+2)
       (220. . ,peakburn-green+3)
       (240. . ,peakburn-green+4)
       (260. . ,peakburn-cyan)
       (280. . ,peakburn-blue-2)
       (300. . ,peakburn-blue-1)
       (320. . ,peakburn-blue)
       (340. . ,peakburn-blue+1)
       (360. . ,peakburn-magenta)))
   `(vc-annotate-very-old-color ,peakburn-magenta)
   `(vc-annotate-background ,peakburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar peakburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for peakburn color names.
In buffers visiting library `peakburn-theme.el' the peakburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar peakburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after peakburn activate)
;;   "Maybe also add font-lock keywords for peakburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or peakburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "peakburn-theme.el")))
;;     (unless peakburn-colors-font-lock-keywords
;;       (setq peakburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car peakburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc peakburn-colors-alist))))))
;;     (font-lock-add-keywords nil peakburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after peakburn activate)
;;   "Also remove font-lock keywords for peakburn colors."
;;   (font-lock-remove-keywords nil peakburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'peakburn)

;;;###autoload
(add-to-list 'safe-local-eval-forms
             '(when (require 'rainbow-mode nil t) (rainbow-mode 1)))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; peakburn-theme.el ends here
