;;; dasm-mode
;
; Works fine with DAsm-style 6502 source code
; Requires Emacs 20.x or higher
;
; Questions or suggestions to MagerValp@cling.gu.se
;
; Copyright 2002 Per Olofsson
; Released under the Gnu General Public License, GPL

; Changes by Anders Carlsson <anders.carlsson@mds.mdh.se> on July 31, 2002 :
;
; Modified with key bindings to actually execute the "dasm" binary. The 
; settings can easily be modified from keyboard input. You need to have 
; "dasm" in your $PATH (%PATH%) or make some changes below. If you need 
; more options, you might break out the lambda construct into its own 
; function and call it with useful parameters.

(setq auto-mode-alist (cons '("\\.asm$" . dasm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.a65$" . dasm-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.i$" . dasm-mode) auto-mode-alist))

(set (make-variable-buffer-local 'dasm-verbose) nil)
(set (make-variable-buffer-local 'dasm-listfile) nil)
(set (make-variable-buffer-local 'dasm-outfile) nil)

(defun dasm-assemble ()
  "Run the DAsm assembler on current buffer."
  (interactive)
  (compile (concat "dasm " (buffer-name)
		   (if (numberp dasm-verbose) 
		       (concat " -v" (number-to-string dasm-verbose)))
		   (if (stringp dasm-listfile) (concat " -l" dasm-listfile))
		   (if (stringp dasm-outfile) (concat " -o" dasm-outfile)))))

; Example of other options to add 				  
; (if (numberp dasm-passes) (concat " -p" dasm-passes))
; (if (stringp dasm-incdir) (concat " -I" dasm-incdir))


(defvar dasm-font-lock-keywords
  '(
    ("\\(;.*\\)$" . font-lock-comment-face)
    ("^\\([a-zA-Z0-9_:]+\\)\\b" . font-lock-constant-face)
    ("[ \t]+\\(d[csv].[bwl]?\\|hex\\|eqm\\|equ\\|set\\)\\b" . font-lock-type-face)
    ("[ \t]+\\(seg\\|seg.u\\|org\\|rorg\\|rend\\|align\\)\\b" . font-lock-warning-face)
    ("[ \t]+\\(include\\|incbin\\|incdir\\|ifconst\\|ifnconst\\|if\\|else\\|endif\\|eif\\|subroutine\\|repeat\\|repend\\)\\b" . font-lock-variable-name-face)
    ("[ \t]+\\(processor\\|err\\|echo\\|list\\)\\b" . font-lock-builtin-face)
    ("[ \t]+\\(adc\\|and\\|asl\\|bcc\\|bcs\\|beq\\|bit\\|bmi\\|bne\\|bpl\\|brk\\|bvc\\|bvs\\|clc\\|cld\\|cli\\|clv\\|cmp\\|cpx\\|cpy\\|dec\\|dex\\|dey\\|eor\\|inc\\|inx\\|iny\\|jmp\\|jsr\\|lda\\|ldx\\|ldy\\|lsr\\|nop\\|ora\\|pha\\|php\\|pla\\|plp\\|rol\\|ror\\|rti\\|rts\\|sbc\\|sec\\|sed\\|sei\\|sta\\|stx\\|sty\\|tax\\|tay\\|tsx\\|txa\\|txs\\|tya\\)\\b" . font-lock-keyword-face)
    )
  "Expressions to highlight in dasm-mode.")

(defun dasm-set-verbose ()
  "Set verboseness level (useful is 0-4)."
  (interactive)
  (let ((res (read-from-minibuffer "Verboseness: " "0" nil t)))
    (setq dasm-verbose (if (and (integerp res) (> res 0)) res))))

(defun dasm-set-outfile ()
  "Set output file name."
  (interactive)
  (let ((res (read-from-minibuffer "Output file: " dasm-outfile)))
    (setq dasm-outfile (if (> (length res) 0) res))))

(defun dasm-set-listfile ()
  "Set list file name (default is none)."
  (interactive)
  (let ((res (read-from-minibuffer "Listfile: ")))
    (setq dasm-listfile (if (> (length res) 0) res))))

(define-derived-mode dasm-mode fundamental-mode "DAsm"
  "Mode for editing DAsm cross assembler source."
  (interactive)
  (setq tab-width 10)
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-end) "$")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dasm-font-lock-keywords nil t))
  (make-local-variable 'compilations-read-command)
  (setq compilation-read-command t)
  (setq dasm-outfile
	(concat (substring (buffer-name) 0 (string-match "\\." (buffer-name))) 
		".prg"))
  (local-set-key "\C-c\C-a" 'dasm-assemble)
  (local-set-key "\C-c\C-v" 'dasm-set-verbose)
  (local-set-key "\C-c\C-o" 'dasm-set-outfile)
  (local-set-key "\C-c\C-l" 'dasm-set-listfile))
  


