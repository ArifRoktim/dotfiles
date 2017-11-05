;;; Eshell
;;; TAKEN AND MODIFIED FROM https://github.com/Ambrevar/dotfiles/tree/master/.emacs.d/lisp

;;; REVIEW: ANSI coloring goes wrong sometimes.  Quite often with emerge/eix.
;;; Fixed in #27407.
(require 'patch-eshell)

;;; Use native 'sudo', system sudo asks for password every time.
;;(require 'em-tramp)

;;(with-eval-after-load "esh-module" ; Need a file name because `provide' is before the definition of `eshell-modules-list. TODO: Report.
;;  ;; Don't print the banner.
;;  (delq 'eshell-banner eshell-modules-list)
;;  (push 'eshell-tramp eshell-modules-list))

(setq
 eshell-ls-use-colors t
 ;; ffap-shell-prompt-regexp changes the behaviour of `helm-find-files' when
 ;; point is on prompt. I find this disturbing.
 eshell-history-size 999
 eshell-hist-ignoredups t
 eshell-destroy-buffer-when-process-dies t)

;;; Hooks
;;; `nobreak-char-display' makes some output look weird, e.g. with 'tree'.
;;(add-hook 'eshell-mode-hook 'turn-off-nobreak-char-display)
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

(provide 'init-eshell)

;; ls after cd
(defun eshell/cl (&rest args)
  (condition-case nil 
      (progn 
        (eshell/cd (pop args) )
        (eshell/ls)
        )))
