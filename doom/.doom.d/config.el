;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Daniel Pagan"
      user-mail-address "deapagan@gmail.com")

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
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 13)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 24))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Help org agenda find gtasks
(setq org-agenda-files (list "~/org/gtasks/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! ccls
  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2)) ; optional as ccls is the default in Doom

(after! haskell-mode ; in this case the major mode and package named the same thing
  (set-ligatures! 'haskell-mode
    :true "True" :false "False"
    ; this will replace not only definitions
    ; but coresponding functions aswell
    :bool "bool"
    :true "True"
    :false "False"
    :int "Int"
    :float "Float"
    :not "not"
    :in "in"
    :and "&&"
    :and "and"
    :or "||"
    :or "or"
    :not-in "notElem"
    :lambda "\\"
))

(plist-put! +ligatures-extra-symbols
  ;; org
  :name          "»"
  :src_block     "»"
  :src_block_end "«"
  :quote         "“"
  :quote_end     "”"
  ;; Functional
  :lambda        "λ"
  :def           "ƒ"
  :composition   "∘"
  :map           "↦"
  ;; Types
  :null          "∅"
  :true          "ᴛ"
  :false         "ꜰ"
  :int           "ℤ"
  :float         "ℝ"
  :str           "S"
  :bool          "ℬ"
  :list          "ℒ"
  ;; Flow
  :not           "￢"
  :in            "∈"
  :not-in        "∉"
  :and           "∧"
  :or            "∨"
  :for           "∀"
  :some          "∃"
  :return        "⟼"
  :yield         "⟻"
  ;; Other
  :union         "⋃"
  :intersect     "∩"
  :diff          "∖"
  :tuple         "⨂"
  :dot           "•")  ;; you could also add your own if you want

;; Sync Google calendar
(after! org-gcal
(setq org-gcal-client-id (getenv "ORG_GCAL_CLIENT_ID")
        org-gcal-client-secret (getenv "ORG_GCAL_CLIENT_SECRET")
        org-gcal-file-alist '(("deapagan@gmail.com" . "~/org/deapagan-gcal.org")
                              ("dap2@andrew.cmu.edu" . "~/org/dap2-cmu-gcal.org")))
)

;; Define a custom calendar open function with all calendar sources
(defun open-all-calendars ()
  (interactive)
  (org-gcal-fetch)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Personal" "~/org/deapagan-gcal.org" "Cyan")  ; Personal gmail
    (cfw:org-create-file-source "CMU" "~/org/dap2-cmu-gcal.org" "Red") ; CMU gmail
   )))

(map! :leader :desc "Open all of my calendar sources with cfw" "o c" 'open-all-calendars)

;; Make the initial frame on boot be fullscreen
(add-to-list 'initial-frame-alist '(fullscreen . fullboth))

;; org-gtasks config to sync org todo lists
(after! org-gtasks
  (org-gtasks-register-account :name "Daniel Pagan"
                               :directory "~/org/gtasks/"
                               :client-id (getenv "ORG_GCAL_CLIENT_ID")
                               :client-secret (getenv "ORG_GCAL_CLIENT_SECRET")))

;; configure lastpass account
(after! lastpass
  (setq lastpass-user "deapagan@gmail.com")
  (setq lastpass-trust-login t)
  (lastpass-auth-source-enable))
