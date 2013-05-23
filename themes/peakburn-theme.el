;;; peakburn-theme.el --- A color theme for Emacs.

;; Copyright (C) 2013 Kari Oikarinen
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
    ("peakburn-bg-1"     . "#101010")
    ("peakburn-bg-05"    . "#181818")
    ("peakburn-bg"       . "#202020")
    ("peakburn-bg+1"     . "#303030")
    ("peakburn-bg+2"     . "#404040")
    ("peakburn-bg+3"     . "#505050")
    ("peakburn-red+1"    . "#DCA3A3")
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
    ("peakburn-blue+1"   . "#7090FF")
    ("peakburn-blue"     . "#6080F0")
    ("peakburn-blue-1"   . "#5070E0")
    ("peakburn-blue-2"   . "#4060D0")
    ("peakburn-blue-3"   . "#3050C0")
    ("peakburn-blue-4"   . "#2040B0")
    ("peakburn-blue-5"   . "#1030A0")
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
   `(match ((t (:background ,peakburn-bg-1 :foreground ,peakburn-orange :weight bold))))
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
   `(font-lock-type-face ((t (:foreground ,peakburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,peakburn-pink))))
   `(font-lock-warning-face ((t (:foreground ,peakburn-orange :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-default-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,peakburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,peakburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,peakburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,peakburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,peakburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,peakburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,peakburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,peakburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,peakburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,peakburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,peakburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:foreground ,peakburn-yellow))))
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
   `(ac-selection-face ((t (:background ,peakburn-blue-4 :foreground ,peakburn-fg))))
   `(popup-tip-face ((t (:background ,peakburn-yellow-2 :foreground "black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,peakburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,peakburn-bg-1))))
   `(popup-isearch-match ((t (:background ,peakburn-bg :foreground ,peakburn-fg))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,peakburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,peakburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,peakburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,peakburn-green))))
   `(android-mode-warning-face ((t (:foreground ,peakburn-yellow))))
;;;;; bm
   `(bm-face ((t (:background ,peakburn-yellow-1 :foreground ,peakburn-bg))))
   `(bm-fringe-face ((t (:background ,peakburn-yellow-1 :foreground ,peakburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,peakburn-green-1 :foreground ,peakburn-bg))))
   `(bm-persistent-face ((t (:background ,peakburn-green-1 :foreground ,peakburn-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,peakburn-orange :weight bold :underline t))))
   `(clojure-test-error-face ((t (:foreground ,peakburn-red :weight bold :underline t))))
   `(clojure-test-success-face ((t (:foreground ,peakburn-green+1 :weight bold :underline t))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,peakburn-blue :foreground ,peakburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,peakburn-bg-05 :foreground ,peakburn-bg))))
   `(ctbl:face-row-select ((t (:background ,peakburn-cyan :foreground ,peakburn-bg))))
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
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,peakburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,peakburn-orange))))
   `(diredp-date-time ((t (:foreground ,peakburn-magenta))))
   `(diredp-deletion ((t (:foreground ,peakburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,peakburn-red))))
   `(diredp-dir-heading ((t (:foreground ,peakburn-blue :background ,peakburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,peakburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,peakburn-red))))
   `(diredp-executable-tag ((t (:foreground ,peakburn-green+1))))
   `(diredp-file-name ((t (:foreground ,peakburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,peakburn-green))))
   `(diredp-flag-mark ((t (:foreground ,peakburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,peakburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,peakburn-red))))
   `(diredp-link-priv ((t (:foreground ,peakburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,peakburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,peakburn-orange))))
   `(diredp-no-priv ((t (:foreground ,peakburn-fg))))
   `(diredp-number ((t (:foreground ,peakburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,peakburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,peakburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,peakburn-green-1))))
   `(diredp-symlink ((t (:foreground ,peakburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,peakburn-magenta))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,peakburn-green+4 :background ,peakburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,peakburn-red :background ,peakburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,peakburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,peakburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,peakburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,peakburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,peakburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,peakburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,peakburn-cyan :weight bold))))
;;;;; flycheck
   `(flycheck-error-face ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
   `(flycheck-warning-face ((t (:foreground ,peakburn-orange :weight bold :underline t))))
;;;;; flymake
   `(flymake-errline ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
   `(flymake-warnline ((t (:foreground ,peakburn-orange :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate ((t (:foreground ,peakburn-orange :weight bold :underline t))))
   `(flyspell-incorrect ((t (:foreground ,peakburn-red-1 :weight bold :underline t))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,peakburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning))))
   `(erc-default-face ((t (:foreground ,peakburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,peakburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,peakburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,peakburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,peakburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,peakburn-green))))
   `(erc-pal-face ((t (:foreground ,peakburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,peakburn-orange :background ,peakburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,peakburn-green+1))))
   `(erc-underline-face ((t (:underline t))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,peakburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,peakburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,peakburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,peakburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,peakburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,peakburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,peakburn-magenta :weight bold))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,peakburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,peakburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,peakburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,peakburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,peakburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,peakburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,peakburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,peakburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,peakburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,peakburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,peakburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,peakburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,peakburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,peakburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,peakburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,peakburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,peakburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,peakburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,peakburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,peakburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,peakburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,peakburn-green))))
   `(gnus-cite-7 ((t (:foreground ,peakburn-red))))
   `(gnus-cite-8 ((t (:foreground ,peakburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,peakburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,peakburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,peakburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,peakburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,peakburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,peakburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,peakburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,peakburn-bg+2))))
   `(gnus-signature ((t (:foreground ,peakburn-yellow))))
   `(gnus-x ((t (:background ,peakburn-fg :foreground ,peakburn-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,peakburn-blue))))
   `(guide-key/key-face ((t (:foreground ,peakburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,peakburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,peakburn-green
                      :background ,peakburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,peakburn-yellow
                      :background ,peakburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,peakburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,peakburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,peakburn-bg :background ,peakburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,peakburn-green+4 :background ,peakburn-bg-1))))
   `(helm-ff-directory ((t (:foreground ,peakburn-magenta))))
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
;;;;; js2-mode
   `(js2-warning-face ((t (:underline ,peakburn-orange))))
   `(js2-error-face ((t (:foreground ,peakburn-red :weight bold))))
   `(js2-jsdoc-tag-face ((t (:foreground ,peakburn-green-1))))
   `(js2-jsdoc-type-face ((t (:foreground ,peakburn-green+2))))
   `(js2-jsdoc-value-face ((t (:foreground ,peakburn-green+3))))
   `(js2-function-param-face ((t (:foreground, peakburn-green+3))))
   `(js2-external-variable-face ((t (:foreground ,peakburn-orange))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,peakburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,peakburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,peakburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,peakburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,peakburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,peakburn-red+1))))
   `(jabber-activity-face((t (:foreground ,peakburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,peakburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; linum-mode
   `(linum ((t (:foreground ,peakburn-fg-1 :background ,peakburn-bg+1))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,peakburn-red+1 :background ,peakburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,peakburn-blue+1 :background ,peakburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,peakburn-magenta :background ,peakburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,peakburn-yellow :background ,peakburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-section-title ((t (:foreground ,peakburn-yellow :weight bold))))
   `(magit-branch ((t (:foreground ,peakburn-orange :weight bold))))
   `(magit-item-highlight ((t (:background ,peakburn-bg+1))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,peakburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,peakburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,peakburn-green+3))))
   `(egg-branch ((t (:foreground ,peakburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,peakburn-yellow))))
   `(egg-term ((t (:foreground ,peakburn-yellow))))
   `(egg-diff-add ((t (:foreground ,peakburn-green+4))))
   `(egg-diff-del ((t (:foreground ,peakburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,peakburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,peakburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,peakburn-green+4))))
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
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,peakburn-orange))))
   `(mew-face-header-from ((t (:foreground ,peakburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,peakburn-green))))
   `(mew-face-header-to ((t (:foreground ,peakburn-red))))
   `(mew-face-header-key ((t (:foreground ,peakburn-green))))
   `(mew-face-header-private ((t (:foreground ,peakburn-green))))
   `(mew-face-header-important ((t (:foreground ,peakburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,peakburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,peakburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,peakburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,peakburn-red))))
   `(mew-face-body-url ((t (:foreground ,peakburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,peakburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,peakburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,peakburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,peakburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,peakburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,peakburn-red))))
   `(mew-face-mark-review ((t (:foreground ,peakburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,peakburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,peakburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,peakburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,peakburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,peakburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,peakburn-green))))
   `(mew-face-eof-part ((t (:foreground ,peakburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,peakburn-cyan :background ,peakburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,peakburn-bg :background ,peakburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,peakburn-bg :background ,peakburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,peakburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,peakburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,peakburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,peakburn-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,peakburn-yellow))))
   `(mingus-stopped-face ((t (:foreground ,peakburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,peakburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,peakburn-cyan))))
   `(nav-face-dir ((t (:foreground ,peakburn-green))))
   `(nav-face-hdir ((t (:foreground ,peakburn-red))))
   `(nav-face-file ((t (:foreground ,peakburn-fg))))
   `(nav-face-hfile ((t (:foreground ,peakburn-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,peakburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,peakburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,peakburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,peakburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,peakburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,peakburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,peakburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,peakburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,peakburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,peakburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,peakburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,peakburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,peakburn-bg+1))))
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
   `(org-level-8 ((t (:foreground ,peakburn-blue-4))))
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
   `(outline-8 ((t (:foreground ,peakburn-blue-4))))
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
   `( rainbow-delimiters-depth-12-face ((t (:foreground ,peakburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,peakburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,peakburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,peakburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,peakburn-blue-2))))
   `(rcirc-server ((t (:foreground ,peakburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,peakburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,peakburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,peakburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,peakburn-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,peakburn-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,peakburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,peakburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,peakburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,peakburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,peakburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,peakburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,peakburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,peakburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,peakburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,peakburn-orange))))
   `(rst-level-2-face ((t (:foreground ,peakburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,peakburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,peakburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,peakburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,peakburn-green-1))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,peakburn-red-3 :background ,peakburn-bg :weight bold))))
   `(show-paren-match ((t (:foreground ,peakburn-blue-1 :background ,peakburn-bg :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-inputed-output-face ((t (:foreground ,peakburn-red))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,peakburn-fg
                                    :background ,peakburn-bg))))
   `(tabbar-selected ((t (:foreground ,peakburn-fg
                                      :background ,peakburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,peakburn-fg
                                        :background ,peakburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
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
                                      :background ,peakburn-blue-4))))
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
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,peakburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,peakburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,peakburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,peakburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,peakburn-green+2 :background ,peakburn-bg))))
   `(w3m-lnum-match ((t (:background ,peakburn-bg-1
                                     :foreground ,peakburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,peakburn-yellow))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,peakburn-bg+1 :foreground ,peakburn-bg+1))))
   `(whitespace-hspace ((t (:background ,peakburn-bg+1 :foreground ,peakburn-bg+1))))
   `(whitespace-tab ((t (:background ,peakburn-red-1))))
   `(whitespace-newline ((t (:foreground ,peakburn-bg+1))))
   `(whitespace-trailing ((t (:background ,peakburn-red))))
   `(whitespace-line ((t (:background ,peakburn-bg :foreground ,peakburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,peakburn-orange :foreground ,peakburn-orange))))
   `(whitespace-indentation ((t (:background ,peakburn-yellow :foreground ,peakburn-red))))
   `(whitespace-empty ((t (:background ,peakburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,peakburn-yellow :foreground ,peakburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,peakburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,peakburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,peakburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,peakburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,peakburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,peakburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,peakburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,peakburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,peakburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,peakburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,peakburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,peakburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,peakburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,peakburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,peakburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,peakburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,peakburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,peakburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,peakburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,peakburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,peakburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,peakburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,peakburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,peakburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,peakburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,peakburn-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,peakburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,peakburn-bg-1 :foreground ,peakburn-bg-1))))
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
