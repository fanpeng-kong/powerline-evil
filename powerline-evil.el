;;; powerline-evil.el --- Utilities for better Evil support for Powerline

;; Copyright (C) 2014 Chris Johnson

;; Author: Chris Johnson <raugturi@gmail.com>
;; Version: 1.0
;; Package-Requires: ((evil "1.0.8") (powerline "2.3"))
;; Keywords: evil, mode-line, powerline
;; URL: http://github.com/raugturi/powerline-evil/

;;; Commentary:
;;
;; Utilities for better Evil support for Powerline and a few extra themes.
;;

;;; Code:

(require 'evil)
(require 'powerline)
(require 'powerline-evil-themes)

;; (defcustom powerline-evil-tag-style 'visual-expanded
;;   "The style to use for displaying the evil state tag.

;; Valid Values: standard, verbose, visual-expanded"
;;   :group 'powerline
;;   :type '(choice (const standard)
;;                  (const verbose)
;;                  (const visual-expanded)))

;; (defface powerline-evil-base-face
;;   '((t (:foreground "white" :inherit mode-line)))
;;   "Base face for powerline evil faces."
;;   :group 'powerline)

;; (defface powerline-evil-normal-face
;;   '((t (:foreground "#005f00" :background "afd700" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil NORMAL state."
;;   :group 'powerline)

;; (defface powerline-evil-insert-face
;;   '((t (:foreground "#005f5f" :background "ffffff" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil INSERT state."
;;   :group 'powerline)

;; (defface powerline-evil-visual-face
;;   '((t (:foreground "#080808" :background "#df5f00" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil VISUAL state."
;;   :group 'powerline)


;; (defface powerline-evil-operator-face
;;   '((t (:foreground "white" :background "sky blue" :inherit powerline-evil-operator-face)))
;;   "Powerline face for evil OPERATOR state."
;;   :group 'powerline)

;; (defface powerline-evil-replace-face
;;   '((t (:foreground "#ffffff" :background "d70000" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil REPLACE state."
;;   :group 'powerline)

;; (defface powerline-evil-motion-face
;;   '((t (:foreground "white" :background "blue" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil MOTION state."
;;   :group 'powerline)

;; (defface powerline-evil-emacs-face
;;   '((t (:foreground "#d7d7ff" :background "#5f00af" :inherit powerline-evil-base-face)))
;;   "Powerline face for evil EMACS state."
;;   :group 'powerline)


;; ;;;###autoload
;; (defun powerline-evil-face ()
;;   "Function to select appropriate face based on `evil-state'."
;;   (let* ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
;;     (if (facep face) face nil)))

;; (defun powerline-evil-tag ()
;;   "Get customized tag value for current evil state."
;;   (let* ((visual-block (and (evil-visual-state-p)
;;                             (eq evil-visual-selection 'block)))
;;          (visual-line (and (evil-visual-state-p)
;;                            (eq evil-visual-selection 'line))))
;;     (cond ((eq powerline-evil-tag-style 'visual-expanded)
;;            (cond (visual-block " +V+ ")
;;                  (visual-line " -V- ")
;;                  (t evil-mode-line-tag)))
;;           ((eq powerline-evil-tag-style 'verbose)
;;            (upcase (concat (symbol-name evil-state)
;;                            (cond (visual-block " BLOCK")
;;                                  (visual-line " LINE")))))
;;           (t evil-mode-line-tag))))

;; Evil mode
(eval-after-load 'evil
  '(progn
     (defface powerline-evil-normal-face
       '((t (:foreground "#005f00" :background "#afd700" :weight bold :inherit mode-line)))
       "face to fontify evil normal state"
       :group 'powerline)

     (defface powerline-evil-insert-face
       '((t (:foreground "#005f5f" :background "#ffffff" :weight bold :inherit mode-line)))
       "face to fontify evil insert state"
       :group 'powerline)

     (defface powerline-evil-visual-face
       '((t (:foreground "#080808" :background "#df5f00" :weight bold :inherit mode-line)))
       "face to fontify evil visual state"
       :group 'powerline)

     (defface powerline-evil-replace-face
       '((t (:foreground "#ffffff" :background "#d70000" :weight bold :inherit mode-line)))
       "face to fontify evil replace state"
       :group 'powerline)

     (defface powerline-evil-emacs-face
       '((t (:foreground "#d7d7ff" :background "#5f00af" :weight bold :inherit mode-line)))
       "face to fontify evil emacs state"
       :group 'powerline)

     (defface powerline-evil-motion-face
       '((t (:foreground "white" :background "blue" :weight bold :inherit mode-line)))
       "face to fontify evil motion state"
       :group 'powerline)

     (defface powerline-evil-operator-face
       '((t (:foreground "white" :background "sky blue" :weight bold :inherit mode-line)))
       "face to fontify evil operator state"
       :group 'powerline)

     (defface powerline-evil-iedit-face
       '((t (:foreground "white" :background "firebrick" :weight bold :inherit mode-line)))
       "face to fontify evil iedit state"
       :group 'powerline)

     (defface powerline-evil-iedit-insert-face
       '((t (:foreground "white" :background "#0F9" :weight bold :inherit mode-line)))
       "face to fontify evil iedit insert state"
       :group 'powerline)

     (defun powerline-evil-face (active)
       (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
         (cond ((and active (facep face))
                face)
               (active 'powerline-file-name-active)
               (t 'powerline-file-name-inactive))))

     (defun powerline-evil-tag ()
       (cond
        ((and (evil-visual-state-p) (eq evil-visual-selection 'block))
         " +V+ ")
        ((and (evil-visual-state-p) (eq evil-visual-selection 'line))
         " -V- ")
        (t
         evil-mode-line-tag)))
     ))

(provide 'powerline-evil)
;;; powerline-evil.el ends here
