;;;; Contains all trivial settings & preferences

;;; PACKAGE MANAGEMENT
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;;; NAVIGATION
(enable ido-mode)
(setc ido-enable-flex-matching t)

;;; SELECTION & CLIPBOARD
(enable delete-selection-mode) ;delete selected text when typing
(defun add-copy-line ()
  "Copy the line at point and move to the beginning of the next line.
   Consecutive calls to this command append to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'add-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
        (kill-new (buffer-substring beg end))))
  (beginning-of-line 2)
  (message "Line appended to kill-ring"))

;;; COLOR & THEMES
(load-theme 'wombat)

;;; KEYBINDINGS
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "M-c") 'add-copy-line)
;(global-set-key (kbd "<return>") 'newline-and-indent) ;auto indent
(global-unset-key (kbd "M-e"))
(global-set-key (kbd "M-e C-p") 'eval-last-sexp)
(global-set-key (kbd "M-e C-x") 'eval-expression)
(global-set-key (kbd "C-c C-h") 'make-habit)

;;; SYSTEM
(when (eq system-type 'darwin) ;Mac specific
  (setc mac-pass-command-to-system nil)
  (setc mac-option-modifier 'meta) ;seldom used
  (setc mac-command-modifier 'control)) ;as in most applications

;;; ORG MODE
(require 'org)
(setc org-modules '(org-ctags org-docview org-habit org-mouse
		    org-choose org-favtable org-panel
		    ))
(setc org-startup-indented t)
(setc org-log-repeat nil)
; enable prompt-free code running
(setc org-confirm-babel-evaluate nil) ;code blocks
(setc org-confirm-elisp-link-function nil) ;elisp links
(setc org-confirm-shell-link-function nil) ;shell links

(defun make-habit ()
  (interactive)
  (org-todo "HABIT")
  (org-set-property "STYLE" "habit"))

(defun org-dblock-write:open-browser-summary (params)
  (insert
   (shell-command-to-string
    "/usr/local/lib/anaconda/bin/python ~/Documents/prog/app/web.py -twc")))

;; make it easier to write more complex and composable filters
;; by transforming symbolic expressions
(defun sp:flatten-and (and-args)
  (let ((or-args '((and)))) ;a list of and terms
    (dolist (and-arg (mapq sp:flatten and-args))
      ;distribute
      (aswap! or-args
	      (pcase and-arg
		(`(or . ,args)
		 (collect (list:append or-arg and-term)
			  for or-arg in args
			  for and-term in %))
		(_
		 (collect (list:append and-arg and-term)
			  for and-term in %)))))
    `(or ,@or-args)))
;; expands the search pattern to sidestep associativity issues
(defun sp:flatten (pat)
  (pcase pat
    (`(or . ,args) `(or . ,(mapq sp:flatten args)))
    (`(and . ,args) (sp:flatten-and args))
    ;De Morgan's laws
    (`(not (or . ,args))
     (sp:flatten `(and . ,(collect `(not ,arg) for arg in args))))
    (`(not (and . ,args))
     (sp:flatten `(or . ,(collect `(not ,arg) for arg in args))))
    ;double negation
    (`(not (not ,arg)) (sp:flatten arg))
    (_ pat)))
;; build up string
(defun sp:stringify (pat)
  (pcase pat
    (`(or . ,args) (str:join "|" (mapq sp:stringify args)))
    (`(and . ,args) (str:join "&" (mapq sp:stringify args)))
    (`(not ,arg) (concat "-" (sp:stringify arg)))
    (`(due ,cmp ,dt)
     (concat "DEADLINE" (sp:stringify cmp)
	     "\"<" (sp:stringify dt) ">\""))
    (`(scheduled ,cmp ,dt)
     (concat "SCHEDULED" (sp:stringify cmp)
	     "\"<" (sp:stringify dt) ">\""))
    (`(,prop ,pat)
     (concat (sp:stringify prop)
	     "={" (sp:stringify pat) "}"))
    ((pred symbolp) (symbol-name pat))
    ((pred stringp) pat)
    (_ (user-error "Unknown search pattern '%S'" pat))))
;; compile
(defun sp (pat)
  (-> pat sp:flatten sp:stringify))

(let* (;criteria
       (should-pat '(or relevant (TODO HABIT)))
       (immediate-pat`(and (not (TODO WAITING)) (or urgent outstanding (TODO ACTIVE)))) ;ongoing, in-progress
       (upcoming-pat `(or timesensitive (due < +2w))))
  (setq org-agenda-custom-commands ;not loaded initially
	`(("r" "Ranked reminders"
	   ((agenda "" ((org-agenda-ndays 2))) ;today and tomorrow
	    ,@(collect `(tags-todo ,(sp `(and ,pat (not (scheduled < +2d)))))
		       for pat in (list immediate-pat
					`(and       ,should-pat       ,upcoming-pat (not ,immediate-pat)) ;critical, timeline
					`(and (not ,should-pat)       ,upcoming-pat (not ,immediate-pat)) ;happening, occuring soon
					`(and       ,should-pat (not ,upcoming-pat) (not ,immediate-pat)) ;long-term life goals, eventually, sometime, future
					`(and (not ,should-pat) (not ,upcoming-pat) (not ,immediate-pat)) ;optional, maybe, mere suggestions
					)))
	   ((org-agenda-files '("~/Documents/tasks.org"))
	    (org-agenda-sorting-strategy '(timestamp-up
					   todo-state-down
					   tag-up
					   alpha-up
					   ))))
	   ("p" "Projects"
	    ((tags-todo "TODO={PROJECT}&relevant&achievable")
	     (tags-todo "TODO={PROJECT}&-relevant&achievable")
	     (tags-todo "TODO={PROJECT}&relevant&-achievable")
	     (tags-todo "TODO={PROJECT}&-relevant&-achievable|TODO={IDEA}"))
	    ((org-agenda-files '("~/Documents/prog/progproj.org")))))))
