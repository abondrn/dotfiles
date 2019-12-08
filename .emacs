; IMPORTS & PACKAGE MANAGEMENT

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/packages")
(package-initialize)

(require 'powerline) ;https://github.com/jonathanchu/emacs-powerline

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'sr-speedbar)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-default-width 15)
(sr-speedbar-open)
(customize-set-variable 'speedbar-show-unknown-files t) ;show all types of files
(setq speedbar-directory-unshown-regexp "^$") ;show hidden files, i.e. dotfiles
(with-current-buffer sr-speedbar-buffer-nameß
  (setq window-size-fixed 'width))

; make editing large files possible by checking and fixing any associated performance issues
(setq jit-lock-defer-time 0.05)
; adjusts gc theshold to 20 mb; dramatically improves startup time and helps with overall speed
(setq gc-cons-threshold (* 20 1024 1024))

;; BUFFER MANAGEMENT
; copy path to clipboard
; open temporary window (close when deselected)

(require 'deft)
(setq deft-extensions '("org" "tex" "txt")
      deft-directory wd
      deft-recursive t
      deft-use-filename-as-title t)
(deft)

(prefer-coding-system 'utf-8)

;; relative colors
;;(defconst bg-color (face-attribute 'default :background))
;;(defconst fg-color (face-attribute 'default :foreground))
;;(defconst hl-bg-color (face-background 'highlight))
;;(defconst hl-fg-color (face-foreground 'highlight))

(defun code-mode ()
 ;(global-linum-mode t)
 (load-theme 'zenburn t)) 
(code-mode)

(defun word-mode ()
  "Changes looks and feel to that of conventional word processors."
  (setq-default cursor-type 'bar) ; minimal cursor
  (set-face-background 'hl-line "white smoke")
  (set-face-foreground 'highlight nil))

;; make name of file begin edited the name of the window
(setq frame-title-format '("" "%b - Emacs"))

(require 'tabbar)
(tabbar-mode t)

;; group by project
;; group by file type and/or path (depending on no), or major mode (for non-file buffers)
(setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
(defun tabbar-buffer-groups ()
  "Returns list of group names the current buffer belongs to.
   Excludes buffers when there are not visiting that buffer."
  (list
   (cond
    ((member (buffer-name) '("*Completions*"
			     "*scratch*"
			     "*Messages*"))
     "meta")
    ((or (string= ".org" (substring (buffer-name) -4 nil))
          (string= "*Org Agenda*" (buffer-name)))
     "org-mode")
    (t (expand-file-name default-directory)))))

(require 'tabbar-ruler)
(setq tabbar-ruler-global-tabbar t)

;; to be used in place of the antiquated *scratch* buffer
;; given a temporary name so the filename can be decided later
(defun new-buffer ()
  "Creates a new empty buffer not associated with a file"
  (interactive)
  (switch-to-buffer (generate-new-buffer "New Buffer")))

;; jump
