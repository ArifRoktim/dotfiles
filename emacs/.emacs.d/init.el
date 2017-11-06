;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  => Package stuff
;;  => General setting
;;  => Mappings
;;  => Eshell
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

; list the packages you want
(setq package-list '(evil magit evil-magit autopair linum-relative key-chord dracula-theme))

; fetch the list of packages available 
(or (file-exists-p package-user-dir) (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Make Y = y$
(setq-default evil-want-Y-yank-to-eol t)

(add-to-list 'load-path "~/.emacs.d/repos/evil-special-modes" )
(add-to-list 'load-path "~/.emacs.d/repos/rainbow-delimiters" )

(require 'evil)
(require 'dracula-theme)
(require 'key-chord)
(require 'linum-relative)
(require 'autopair)
(require 'rainbow-delimiters)
(require 'magit)
(require 'evil-magit)

;; ======== General setting ========

;; Set modes
(evil-mode 1)
(key-chord-mode 1)
(server-mode)
(autopair-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Add evil to many places
(if (> emacs-major-version 24)
    (progn
      (when (require 'evil-special-modes nil t) (evil-special-modes-init))
      ;; Add evil to minibuffer
      (require 'evil-minibuffer)
      (evil-minibuffer-init))
  nil)

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

;; Follow symbolic links
(setq vc-follow-symlinks t)

;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
;; neither, we use the current indent-tabs-mode                               
(defun infer-indentation-style ()
  (interactive)
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
  (progn (require 'saveplace)
         (setq-default save-place t)))
(setq save-place-forget-unreadable-files nil)

;; Center after search
(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))

(if (> emacs-major-version 24)
    (advice-add 'evil-search-next :after #'my-center-line)
  nil)
;; ======== Mappings ========

;; Normal mode on jk
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)

;; define a prefix keymap
(progn
  (define-prefix-command 'leader)

  ;; Move quickly
  (define-key leader (kbd "w") 'evil-window-up)
  (define-key leader (kbd "a") 'evil-window-left)
  (define-key leader (kbd "s") 'evil-window-down)
  (define-key leader (kbd "d") 'evil-window-right)

  ;; Resizing mappings
  (define-key leader (kbd "h") 'evil-window-decrease-width)
  (define-key leader (kbd "j") 'evil-window-increase-height)
  (define-key leader (kbd "k") 'evil-window-decrease-height)
  (define-key leader (kbd "l") 'evil-window-increase-width)
  )

(eval-after-load 'evil-maps
  '(progn
     (define-key evil-motion-state-map (kbd ",") 'leader)
     (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
     ))

;; ======== Eshell ========

(setq
 eshell-ls-use-colors t
 eshell-history-size 1000
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t)

(add-hook 'eshell-mode-hook 'eshell-cmpl-initialize)

;;; History
;;; Filter out space-beginning commands from history.
;;; TODO: history: do not store duplicates.  Push unique command to front of the list.
(setq eshell-input-filter
      (lambda (str)
        (not (or (string= "" str)
                 (string-prefix-p " " str)))))

;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")

(defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))
(add-hook 'eshell-mode-hook 'eshell-hist-use-global-history)

;;; Completion
(when (require 'bash-completion nil t)
  (defun eshell-bash-completion ()
    (while (pcomplete-here
            (nth 2 (bash-completion-dynamic-complete-nocomint (save-excursion (eshell-bol) (point)) (point))))))
  ;; Sometimes `eshell-default-completion-function' does better, e.g. "gcc
  ;; <TAB>" shows .c files.
  (setq eshell-default-completion-function 'eshell-bash-completion))

;; ls after cd
(defun eshell/cl (&rest args)
  (condition-case nil 
      (progn 
        (eshell/cd (pop args) )
        (eshell/ls)
        )))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#282a36"))
    (concat
     (with-face (concat user-login-name "@" (system-name) " ") :foreground "green")
     (with-face (concat (format-time-string "%m/%d|%I:%M" (current-time)) " ") :foreground "cyan")
     (with-face (concat (eshell/pwd) " ") :foreground "lightblue")
     (if (magit-get-current-branch)
         (with-face (concat "(" (magit-get-current-branch) ")") :foreground "magenta")
       nil
       )
     "\n"
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       "$")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "\$ ")
(setq eshell-prompt-string "\$ ")

;; ======== Custom set variables ========
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-magit magit autopair linum-relative ## key-chord dracula-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
