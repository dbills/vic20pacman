;;;;
  ;;;; cygwin support
  ;;;;  ;; Sets your shell to use cygwin's bash, if Emacs finds it's running
  ;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
  ;; not already in your Windows Path (it generally should not be).
  ;;
  (let* ((cygwin-root "c:/cygwin")
         (cygwin-bin (concat cygwin-root "/bin")))
    (when (and (eq 'windows-nt system-type)
  	     (file-readable-p cygwin-root))
    
      (setq exec-path (cons cygwin-bin exec-path))
      (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
      ;; By default use the Windows HOME.
      ;; Otherwise, uncomment below to set a HOME
      ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
      ;; NT-emacs assumes a Windows shell. Change to baash.
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name) 
      (setq explicit-shell-file-name shell-file-name) 
    
      ;; This removes unsightly ^M characters that would otherwise
      ;; appear in the output of java applications.
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(set-background-color "darkslateblue")
(set-foreground-color "cyan")
(set-cursor-color "orange")
;;(show-hid-toolbar)

(load-library "desktop")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "English")
 '(global-font-lock-mode t nil (font-lock))
 '(jde-jdk-registry (quote (("1.6" . "c:/sun/sdk/jdk"))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq minibuffer-max-depth nil)
(desktop-read)
(tool-bar-mode)

(defvar g_workspace (current-window-configuration))

(defun save-workspace()
  (setq g_workspace (current-window-configuration))
  (princ "workspace saved"))
  
(defun save-or-restore-workspace()
  (interactive)
  (if (> (count-windows) 1)
      (save-workspace)
    (set-window-configuration g_workspace)))

(global-set-key "w" 'save-or-restore-workspace)
; buffer-menu-1-window
; count-windows
			  
;(mouse-wheel-mode)
(set-variable 'completion-ignore-case t)
(set-face-foreground font-lock-comment-face "plum3")
(global-set-key "" 'goto-line)
(global-set-key "" 'compile)
(global-set-key "cs" 'erase-buffer)

(put 'narrow-to-region 'disabled nil)

(put 'erase-buffer 'disabled nil)
(load-file "c:/cygwin/home/dane/cygwin-mount.el")
(cygwin-mount-activate)
(global-set-key "" 'compile)
(global-set-key "" 'other-window)
;
; grep the word at point
;
(defun mygrep()
  (grep (concat "grep -n " (thing-at-point 'word)  " *.cpp *.h *.sql"))
)
(setq semantic-load-turn-everything-on t)
;(add-to-list 'load-path (expand-file-name "c:/ntemacs/ntemacs23/site-lisp/jde"))
;(add-to-list 'load-path (expand-file-name "c:/ntemacs/ntemacs23/site-lisp/cedet-1.0pre7/common"))
;(load-file  "c:/ntemacs/ntemacs23/site-lisp/cedet-1.0pre7/common/cedet.el");
;(add-to-list 'load-path (expand-file-name "~/emacs/site/elib"))
;(require 'jde)

(defvar g_xpay nil)
(defvar g_xpay1 nil)

(defun xpay()
  (kill-xpay)
  (cd-absolute "/cygdrive/c/projects/xpay-core")
  (setq g_xpay (make-comint-in-buffer "router" nil "make" nil "run" ))
  (setq g_xpay1 (make-comint-in-buffer "xpay-tail" nil "tail" nil "-f" "/cygdrive/c/projects/xpay-core/xpay-output.log"))
;  (make-comint-in-buffer "soapclient*" nil "make" nil "soapclient2" )
  (show-buffer nil "*xpay-tail*")
  (view-buffer-other-window  "*router*")
  ;(show-buffer nil "*router*")
  )


(defun kill-xpay() 
  (switch-to-buffer "*router*")
  (toggle-read-only)
  (condition-case nil
      (comint-kill-subjob)
    (error nil))

  (switch-to-buffer "*xpay-tail*")
  (condition-case nil
      (comint-kill-subjob)
    (error nil))

  (switch-to-buffer "*soapclient*")
  (condition-case nil
      (comint-kill-subjob)
    (error nil))

  (condition-case nil
      (kill-buffer "*xpay-tail*")
    (error nil))
  (condition-case nil
      (kill-buffer "*soapclient*")
    (error nil))
  (condition-case nil
      (kill-buffer "*router*")
    (error nil)))

(set-variable 'Buffer-menu-buffer+size-width 40)
; use only spaces for indentation 
;(setq-default 'indent-tabs-mode nil)
(customize-set-variable 'indent-tabs-mode nil)

; turn on merchantlink's real ip file
(defun ml-on()
  "This function activates a real merchantlink ip address in the processor's config file"
  (copy-file "c:/projects/my_test_data/merchantlink-ip_tcpip-settings.xml" "c:/projects/xpay-core/config/merchantlink-ip/merchantlink-ip_tcpip-settings.xml" t))

(defun ml-off()
  "this function reverts the merchatlink's process config file to localhost"
  (copy-file "c:/projects/my_test_data/merchantlink-ip_tcpip-settings.bak" "c:/projects/xpay-core/config/merchantlink-ip/merchantlink-ip_tcpip-settings.xml"  t))

(server-start)
(setq compile-command "dasm pacman.asm -opac.p00;dasm pacman.asm -opac.p00 -lpac.lst")
(global-set-key "" 'undo
)
(set-default-font "-outline-Courier New-bold-normal-normal-mono-*-*-*-*-c-*-iso8859-7");
(tramp-unload-tramp)