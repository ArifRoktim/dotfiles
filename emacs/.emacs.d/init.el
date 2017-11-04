;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  => Package stuff
;;  => General setting
;;  => Mappings
;;  => Custom set variables

;; ======== Package stuff ========

(package-initialize)
(require 'package)

;; Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(require 'evil)
(require 'evil-leader)
(require 'dracula-theme)
(require 'key-chord)
(require 'linum-relative)

;; ======== General setting ========

;; Set modes
(global-evil-leader-mode)
(evil-mode 1)
(key-chord-mode 1)

;; Remove bloat
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; Highlight current line
(global-hl-line-mode)

;; Relative line numbers
(global-linum-mode)
(linum-relative-global-mode)
(setq linum-relative-current-symbol "")

;; Use 4 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
;; neither, we use the current indent-tabs-mode                               
(defun infer-indentation-style ()
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

;; Keep 7 lines above and below cursor
(setq scroll-margin 7)
(setq scroll-step 1)
(setq scroll-conservatively 200)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

;; No annoying bells
(setq ring-bell-function 'ignore)

;; Persistent undo
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Save cursor position
(if (> emacs-major-version 24)
    (save-place-mode 1)
  ((require 'saveplace)
   (setq-default save-place t)))

;; ======== Mappings ========

;; Normal mode on jk
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; ======== Custom set variables ========
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-leader linum-relative ## key-chord dracula-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
