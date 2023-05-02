;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ariel Raudsepp"
      user-mail-address "ariel.raudsepp@gmail.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

 (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size 16.0 :weight 'semi-light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-snazzy)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")

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
;; You can also try 'Gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(add-hook 'find-file-hook 'virtual-comment-mode)

(map! "C-c i c" #'virtual-comment-make)

(setq lsp-ui-doc-position 'top)
(setq-hook! 'php-mode-hook +format-with-lsp nil)

;; (after! prettier-js
;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
;;   (add-hook 'web-mode-hook 'prettier-js-mode)
;;   (add-hook 'typescript-mode-hook 'prettier-js-mode)
;; )


(setq org-journal-file-type 'monthly
      org-journal-file-format "%Y-%m")

(after! org
  (setq org-capture-templates
        '(("d" "Diary" entry (file+datetree "dev-diary.org")
           "* %?\n From: %a"
           :empty-lines 1)
          ("n" "Notes" entry (file+headline "notes.org"
                                            "Notes")
           "* %?\n %i\n"
           :empty-lines 1))))
(after! chatgpt-shell
  (setq chatgpt-shell-openai-key ""))

(after! ob-chatgpt-shell
  (ob-chatgpt-shell-setup))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blinders Mode
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup blinders nil
  "Put on your blinders so you can focus on one line at a time"
  :group 'convenience)

;; (defface blinders-hidden-face
;;   (list (list t (list :background (face-attribute 'default :background)
;;                       :foreground (face-attribute 'default :background))))
;;   "blinders-mode hidden face"
;;   :group 'blinders)

(defcustom blinders-mode-timer-length 5
  "Amount of time to wait until the blinders are removed"
  :type 'integer
  :group 'blinders)
(defvar-local blinders-mode--timer nil)

(defun blinders-mode--remove-blinders ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'blinders))

(defun blinders-mode--refresh-blinders ()
  (interactive)
  ;; deal with timers
  (when blinders-mode--timer
    (cancel-timer blinders-mode--timer))
  (setq-local blinders-mode--timer (run-with-timer blinders-mode-timer-length nil 'blinders-mode--remove-blinders))
  ;; hide buffer
  (let ((before-ov (make-overlay (point-min)
                                 (line-beginning-position 0)))
        (after-ov (make-overlay (line-end-position 2)
                                (point-max))))
    (blinders-mode--remove-blinders)
    (overlay-put before-ov 'category 'blinders)
    (overlay-put before-ov 'face 'blinders-hidden-face)
    (overlay-put after-ov 'category 'blinders)
    (overlay-put after-ov 'face 'blinders-hidden-face)))

(define-minor-mode blinders-mode
  "Mode for showing only one line of the buffer at a time"
  :init-value nil
  (if blinders-mode
      (progn
        (defface blinders-hidden-face
          (list (list t (list :background (face-attribute 'default :background)
                              :foreground (face-attribute 'default :background))))
          "blinders-mode hidden face"
          :group 'blinders)
        (setq-local post-command-hook (cons 'blinders-mode--refresh-blinders post-command-hook)))
    (progn
      (setq-local post-command-hook (delete 'blinders-mode--refresh-blinders post-command-hook))
      (blinders-mode--remove-blinders))))

(add-hook 'prog-mode-hook 'blinders-mode)
