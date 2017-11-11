;;; init.el --- Config file
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  => Package stuff
;;  => General setting
;;  => Mappings
;;  => Eshell
;;  => Custom set variables

;; ======== Package stuff ========

;;; Code:

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

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Vim emulation packages
(use-package evil
  :init
  (setq-default evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1))
(use-package evil-special-modes
  :load-path "repos/evil-special-modes"
  :pin manual
  :ensure f)
(use-package linum-relative
  :config
  (global-linum-mode)
  (linum-relative-global-mode)
  (setq linum-relative-current-symbol ""))

;; Misc
(use-package dracula-theme)
(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state))
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  )

;; Git submodules
(use-package autopair
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (autopair-mode))))
(use-package rainbow-delimiters
  :load-path "repos/rainbow-delimiters"
  :pin manual
  :ensure f
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (rainbow-delimiters-mode))))

;; Magit
(use-package magit)
(use-package evil-magit)

;; Linting
(use-package flycheck
  :init
  (global-flycheck-mode)
  (setq-default flycheck-python-pycompile-executable "python2")
  (setq-default flycheck-python-flake8-executable "flake8-python2")
  (setq-default flycheck-python-pylint-executable "pylint2"))
  
(use-package flycheck-irony
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; Autocompletion stuff
(use-package company
  :init
  (bind-key "C-c c" 'company-complete)
  :config
  (company-mode t)
  (add-hook 'after-init-hook 'global-company-mode))

;; Python autocompletion
(use-package company-jedi
  :config
  (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (defun config/enable-company-jedi ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'config/enable-company-jedi))

;; C autocompletion
(use-package irony
  :config
  (add-hook 'c-mode-hook 'irony-mode))
(use-package company-irony
  :config
  (add-hook 'c-mode-hook (lambda ()
                           (add-to-list 'company-backends 'company-irony)
                           (add-to-list 'company-backends 'company-c-headers)
                           )))

(use-package company-c-headers)

;; ======== General setting ========

;; Set modes
(unless (server-running-p)
  (server-start))

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

;; Use 4 spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 120 4))

;; Follow symbolic links
(setq vc-follow-symlinks t)

(defun infer-indentation-style ()
  "If our source file uses tabs, we use tabs. If spaces spaces, and if neither, we use the current `indent-tabs-mode'"
  (interactive)
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq-local indent-tabs-mode nil))
    (if (> tab-count space-count) (setq-local indent-tabs-mode t))))
(add-hook 'prog-mode-hook 'infer-indentation-style)

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

(defun shortened-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun mode-line-buffer-file-parent-directory ()
  (when buffer-file-name
    (shortened-path default-directory 15)))
(setq-default mode-line-buffer-identification
              (cons
               '((:eval (mode-line-buffer-file-parent-directory)))
               mode-line-buffer-identification
               ))

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-client
                mode-line-modified
                mode-line-remote
                " "
                mode-line-buffer-identification
                mode-line-position
                evil-mode-line-tag
                " "
                mode-line-modes
                mode-line-misc-info
                mode-line-end-spaces)
              )


;; ======== Mappings ========

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

  ;; Eyebrowse key-bindings
  (define-key leader (kbd "0") 'eyebrowse-switch-to-window-config-0)
  (define-key leader (kbd "1") 'eyebrowse-switch-to-window-config-1)
  (define-key leader (kbd "2") 'eyebrowse-switch-to-window-config-2)
  (define-key leader (kbd "3") 'eyebrowse-switch-to-window-config-3)
  (define-key leader (kbd "4") 'eyebrowse-switch-to-window-config-4)
  (define-key leader (kbd "5") 'eyebrowse-switch-to-window-config-5)
  (define-key leader (kbd "6") 'eyebrowse-switch-to-window-config-6)
  (define-key leader (kbd "7") 'eyebrowse-switch-to-window-config-7)
  (define-key leader (kbd "8") 'eyebrowse-switch-to-window-config-8)
  (define-key leader (kbd "9") 'eyebrowse-switch-to-window-config-9)
  (define-key leader (kbd "q") 'eyebrowse-prev-window-config)
  (define-key leader (kbd "e") 'eyebrowse-next-window-config)

  ;; Magit shortcut
  (define-key leader (kbd "g") 'magit-status)

  ;; Flycheck shortcut
  (define-key leader (kbd "z") 'flycheck-previous-error)
  (define-key leader (kbd "c") 'flycheck-next-error)
  
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
    (company-irony use-package rainbow-delimiters linum-relative key-chord flycheck f eyebrowse evil-magit dracula-theme company autopair auto-complete-clang))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
