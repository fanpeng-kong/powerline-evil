;;; powerline-evil-themes.el --- Evil themes for Powerline

;; Copyright (C) 2014 Chris Johnson

;; Author: Chris Johnson <raugturi@gmail.com>
;; Version: 1.0
;; Keywords: evil, mode-line, powerline
;; URL: http://github.com/raugturi/powerline-evil/

;;; Commentary:
;;
;; Evil themes for Powerline.
;; Included themes: evil-center-color, evil-vim, and evil-vim-color.
;;

;;; Code:


;;;###autoload
(defun powerline-evil-center-color-theme ()
  "Powerline's center-evil them with the evil state in color."
  (interactive)
  (setq-default mode-line-format
        '("%e"
          (:eval
           (let* ((active (powerline-selected-window-active))
                  (mode-line (if active 'mode-line 'mode-line-inactive))
                  (face1 (if active 'powerline-active1 'powerline-inactive1))
                  (face2 (if active 'powerline-active2 'powerline-inactive2))
                  (separator-left (intern (format "powerline-%s-%s"
                                                  (powerline-current-separator)
                                                  (car powerline-default-separator-dir))))
                  (separator-right (intern (format "powerline-%s-%s"
                                                   (powerline-current-separator)
                                                   (cdr powerline-default-separator-dir))))
                  (lhs (list (powerline-raw "%*" nil 'l)
                             (powerline-buffer-size nil 'l)
                             (powerline-buffer-id nil 'l)
                             (powerline-raw " ")
                             (funcall separator-left mode-line face1)
                             (powerline-narrow face1 'l)
                             (powerline-vc face1)))
                  (rhs (list (powerline-raw global-mode-string face1 'r)
                             (powerline-raw "%4l" face1 'r)
                             (powerline-raw ":" face1)
                             (powerline-raw "%3c" face1 'r)
                             (funcall separator-right face1 mode-line)
                             (powerline-raw " ")
                             (powerline-raw "%6p" nil 'r)
                             (powerline-hud face2 face1)))
                  (center (append (list (powerline-raw " " face1)
                                        (funcall separator-left face1 face2)
                                        (when (boundp 'erc-modified-channels-object)
                                          (powerline-raw erc-modified-channels-object face2 'l))
                                        (powerline-major-mode face2 'l)
                                        (powerline-process face2)
                                        (powerline-raw " " face2))
                                  (let ((evil-face (powerline-evil-face)))
                                    (if (split-string (format-mode-line minor-mode-alist))
                                        (append (if evil-mode
                                                    (list (funcall separator-right face2 evil-face)
                                                          (powerline-raw (powerline-evil-tag) evil-face 'l)
                                                          (powerline-raw " " evil-face)
                                                          (funcall separator-left evil-face face2)))
                                                (list (powerline-minor-modes face2 'l)
                                                      (powerline-raw " " face2)
                                                      (funcall separator-right face2 face1)))
                                      (list (powerline-raw (powerline-evil-tag) evil-face)
                                            (funcall separator-right evil-face face1)))))))
             (concat (powerline-render lhs)
                     (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                     (powerline-render center)
                     (powerline-fill face1 (powerline-width rhs))
                     (powerline-render rhs)))))))

;;;###autoload
(defun powerline-evil-vim-theme ()
  "Powerline's Vim-like mode-line with evil state at the beginning."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (if evil-mode
                                         (powerline-raw (powerline-evil-tag) mode-line))
                                     (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
                                     (powerline-raw "[" mode-line 'l)
                                     (powerline-major-mode mode-line)
                                     (powerline-process mode-line)
                                     (powerline-raw "]" mode-line)
                                     (when (buffer-modified-p)
                                       (powerline-raw "[+]" mode-line))
                                     (when buffer-read-only
                                       (powerline-raw "[RO]" mode-line))
                                     (powerline-raw "[%z]" mode-line)
                                     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-raw "[" mode-line 'l)
                                     (powerline-minor-modes mode-line) (powerline-raw "%n" mode-line)
                                     (powerline-raw "]" mode-line)
                                     (when (and vc-mode buffer-file-name)
                                       (let ((backend (vc-backend buffer-file-name)))
                                         (when backend
                                           (concat (powerline-raw "[" mode-line 'l)
                                                   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
                                                   (powerline-raw "]" mode-line)))))))
                          (rhs (list (powerline-raw '(10 "%i"))
                                     (powerline-raw global-mode-string mode-line 'r)
                                     (powerline-raw "%l," mode-line 'l)
                                     (powerline-raw (format-mode-line '(10 "%c")))
                                     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

;;;###autoload
(defun powerline-evil-vim-color-theme ()
  "Powerline's Vim-like mode-line with evil state at the beginning in color."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)))
                                     (powerline-buffer-id `(mode-line-buffer-id ,mode-line) 'l)
                                     (powerline-raw "[" mode-line 'l)
                                     (powerline-major-mode mode-line)
                                     (powerline-process mode-line)
                                     (powerline-raw "]" mode-line)
                                     (when (buffer-modified-p)
                                       (powerline-raw "[+]" mode-line))
                                     (when buffer-read-only
                                       (powerline-raw "[RO]" mode-line))
                                     (powerline-raw "[%z]" mode-line)
                                     ;; (powerline-raw (concat "[" (mode-line-eol-desc) "]") mode-line)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-raw "[" mode-line 'l)
                                     (powerline-minor-modes mode-line)
                                     (powerline-raw "%n" mode-line)
                                     (powerline-raw "]" mode-line)
                                     (when (and vc-mode buffer-file-name)
                                       (let ((backend (vc-backend buffer-file-name)))
                                         (when backend
                                           (concat (powerline-raw "[" mode-line 'l)
                                                   (powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
                                                   (powerline-raw "]" mode-line)))))))
                          (rhs (list (powerline-raw '(10 "%i"))
                                     (powerline-raw global-mode-string mode-line 'r)
                                     (powerline-raw "%l," mode-line 'l)
                                     (powerline-raw (format-mode-line '(10 "%c")))
                                     (powerline-raw (replace-regexp-in-string  "%" "%%" (format-mode-line '(-3 "%p"))) mode-line 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill mode-line (powerline-width rhs))
                             (powerline-render rhs)))))))

;; Powerline themes
(defcustom powerline-display-buffer-size t
  "When non-nil, display the buffer size."
  :group 'powerline
  :type 'boolean)

(defcustom powerline-display-mule-info t
  "When non-nil, display the mule info."
  :group 'powerline
  :type 'boolean)

(defcustom powerline-display-hud t
  "When non-nil, display the hud."
  :group 'powerline
  :type 'boolean)

(defcustom powerline-utf-8-subseparator-left #x2b81
  "The unicode character number for the left facing subseparator."
  :group 'powerline
  :type  '(choice integer (const nil)))

(defcustom powerline-utf-8-subseparator-right #x2b83
  "The unicode character number for the right facing subseparator."
  :group 'powerline
  :type  '(choice integer (const nil)))

 (defface powerline-file-name-active
   '((t (:foreground "#ffffff" :background "#585858" :weight bold :inherit mode-line)))
   "Powerline file name face"
   :group 'powerline)

 (defface powerline-file-name-inactive
   '((t (:foreground "#808080" :background "#262626" :weight bold :inherit mode-line)))
   "Powerline file name face"
   :group 'powerline)

 (defface powerline-major-mode-active
   '((t (:foreground "#ffffff" :background "#303030" :inherit mode-line)))
   "Powerline major mode face"
   :group 'powerline)

 (defface powerline-major-mode-inactive
   '((t (:foreground "#808080" :background "#121212" :inherit mode-line)))
   "Powerline major mode face"
   :group 'powerline)

 (defface powerline-background-active
   '((t (:foreground "#bcbcbc" :background "#303030" :inherit mode-line)))
   "Powerline background face"
   :group 'powerline)

 (defface powerline-background-inactive
   '((t (:foreground "#585858" :background "#121212" :inherit mode-line)))
   "Powerline background face"
   :group 'powerline)

 (defface powerline-line-percent-active
   '((t (:foreground "#51b000" :background "#585858" :inherit mode-line)))
   "Powerline line percent face"
   :group 'powerline)

 (defface powerline-line-percent-inactive
   '((t (:foreground "#626262" :background "#262626" :inherit mode-line)))
   "Powerline line percent face"
   :group 'powerline)

 (defface powerline-position-active
   '((t (:foreground "#262626" :background "#d0d0d0" :inherit mode-line)))
   "Powerline position face"
   :group 'powerline)

 (defface powerline-position-inactive
   '((t (:foreground "#262626" :background "#626262" :inherit mode-line)))
   "Powerline position face"
   :group 'powerline)

 (defface powerline-line-current-active
   '((t (:foreground "#262626" :background "#d0d0d0" :weight bold :inherit mode-line)))
   "Powerline current line face"
   :group 'powerline)

 (defface powerline-line-current-inactive
   '((t (:foreground "#262626" :background "#626262" :weight bold :inherit mode-line)))
   "Powerline current line face"
   :group 'powerline)

 (defface powerline-col-current-active
   '((t (:foreground "#006000" :background "#d0d0d0" :inherit mode-line)))
   "Powerline current column face"
   :group 'powerline)

 (defface powerline-col-current-inactive
   '((t (:foreground "#006000" :background "#626262" :inherit mode-line)))
   "Powerline current column face"
   :group 'powerline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these next faces are for the status indicator
;; read-only buffer
(defface powerline-read-only-active
  '((t (:foreground "#cebff3" :background "#585858" :inherit mode-line)))
  "Powerline buffer read only face"
  :group 'powerline)

(defface powerline-read-only-inactive
  '((t (:foreground "#cebff3" :background "#262626" :inherit mode-line)))
  "Powerline buffer read only face"
  :group 'powerline)

;; modified buffer
(defface powerline-modified-active
  '((t (:foreground "#FF2600" :background "#585858" :inherit mode-line)))
  "Powerline buffer modified face"
  :group 'powerline)

(defface powerline-modified-inactive
  '((t (:foreground "#FF2600" :background "#262626" :inherit mode-line)))
  "Powerline buffer modified face"
  :group 'powerline)

;; unmodified buffer
(defface powerline-unmodified-active
  '((t (:foreground  "#cebff3" :background  "#585858" :inherit mode-line)))
  "Powerline buffer unmodified face"
  :group 'powerline)

(defface powerline-unmodified-inactive
  '((t (:foreground "#cebff3" :background "#262626" :inherit mode-line)))
  "Powerline buffer unmodified face"
  :group 'powerline)

;; the remote indicator
;; (defface powerline-remote-active
;;   '((t (:foreground (face-attribute 'font-lock-comment-face :foreground) :background (face-attribute 'default :background) :inherit mode-line)))
;;   "Powerline buffer remote face"
;;   :group 'powerline)
;; 
;; (defface powerline-remote-inactive
;;   '((t (:foreground (face-attribute 'font-lock-comment-face :foreground) :background (face-attribute 'default :background) :inherit mode-line)))
;;   "Powerline buffer remote face"
;;   :group 'powerline)

(defface powerline-remote-active
  '((t (:foreground "#cebff3" :background "#585858" :inherit mode-line)))
  "Powerline buffer remote face"
  :group 'powerline)

(defface powerline-remote-inactive
  '((t (:foreground "#cebff3" :background "#585858" :inherit mode-line)))
  "Powerline buffer remote face"
  :group 'powerline)

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defpowerline kong-powerline-narrow
  (let (real-point-min real-point-max)
    (save-excursion
      (save-restriction
        (widen)
        (setq real-point-min (point-min) real-point-max (point-max))))
    (when (or (/= real-point-min (point-min))
              (/= real-point-max (point-max)))
      (propertize (concat (char-to-string #x2691) " Narrow")
                  'mouse-face 'mode-line-highlight
                  'help-echo "mouse-1: Remove narrowing from the current buffer"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 'mode-line-widen)))))

(defpowerline kong-powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if window-system
        (let ((backend (vc-backend (buffer-file-name (current-buffer)))))
          (when backend
            (format "%s %s: %s"
                    (char-to-string #xe0a0)
                    backend
                    (vc-working-revision (buffer-file-name (current-buffer)) backend)))))))

;;;###autoload
(defun powerline-evil-vim-powerline-theme ()
  "Setup a mode-line with major, evil mode on the left."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))

			  ;; toggle faces between active and inactive
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (mode-line (if active 'mode-line 'mode-line-inactive))

			  (face-file-name (if active 'powerline-file-name-active 'powerline-file-name-inactive))
			  (face-major-mode (if active 'powerline-major-mode-active 'powerline-major-mode-inactive))
			  (face-background (if active 'powerline-background-active 'powerline-background-inactive))
			  (face-line-percent (if active 'powerline-line-percent-active 'powerline-line-percent-inactive))
			  (face-position (if active 'powerline-position-active 'powerline-position-inactive))
			  (face-line-current (if active 'powerline-line-current-active 'powerline-line-current-inactive))
			  (face-col-current (if active 'powerline-col-current-active 'powerline-col-current-inactive))

			  (face-read-only (if active 'powerline-read-only-active 'powerline-read-only-inactive))
			  (face-modified (if active 'powerline-modified-active 'powerline-modified-inactive))
			  (face-unmodified (if active 'powerline-unmodified-active 'powerline-unmodified-inactive))
			  (face-remote (if active 'powerline-remote-active 'powerline-remote-inactive))

			  ;; get the separators
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
			  ;; the left side
                          (lhs
			   (append
			    ;; Evil states
			    (let ((evil-face (powerline-evil-face active)))
			      (list (powerline-raw (powerline-evil-tag) evil-face)
				    (funcall separator-left evil-face face-file-name)))

			    (list (cond (buffer-read-only
			     		 (powerline-raw (concat (char-to-string #xe0a2) " ") face-read-only 'l))
			     		((buffer-modified-p)
			     		 (if (not (bound-and-true-p ml-interactive?))
			     		     (powerline-raw (concat (char-to-string #x2621) " ") face-modified 'l)
			     		   (powerline-raw (concat (char-to-string #x259e) " ") face-unmodified 'l)))
			     		((not (buffer-modified-p))
			     		 (powerline-raw (concat (char-to-string #x26c1) " ") face-unmodified 'l)))
				  ;; remote indicator
				  (when (file-remote-p default-directory)
				    (powerline-raw (concat " " (char-to-string #x211b)) face-remote))
				  
				  (powerline-buffer-id face-file-name 'r)
				  ;; (when (and (boundp 'which-func-mode) which-func-mode)
				  ;;   (powerline-raw which-func-format face-file-name 'l))

				  (funcall separator-left face-file-name face-major-mode)

				  ;; (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
				  ;;   (powerline-raw erc-modified-channels-object face1 'l))
				  (powerline-major-mode face-major-mode 'l)
				  (powerline-process face-major-mode)
				  (powerline-raw (char-to-string powerline-utf-8-subseparator-left) face-background 'l)

				  (powerline-vc face-background 'r)
				  (when (bound-and-true-p nyan-mode)
				    (powerline-raw (list (nyan-create)) face-background 'l)))))

                          (rhs (list (powerline-raw mode-line-misc-info face-background 'r)

				(powerline-raw (char-to-string powerline-utf-8-subseparator-right) face-background 'l)
				(when powerline-display-buffer-size
				  (powerline-buffer-size face-background 'l))
				(when powerline-display-mule-info
				  (powerline-raw mode-line-mule-info face-background 'l))
				(powerline-raw " " face-background)

				(funcall separator-right face-background face-line-percent)
				(powerline-raw " " face-line-percent)
				(powerline-raw "%p" face-line-percent 'r)
				(funcall separator-right face-line-percent face-position)
				(unless window-system
				  (powerline-raw (char-to-string #xe0a1) face-position 'l))
				(powerline-raw "%2l" face-line-current 'l)
				(powerline-raw ":" face-position 'l)
				(powerline-raw "%2c" face-col-current 'r)
				(when powerline-display-hud
				  (powerline-hud face-background face-major-mode)))))

		     (concat (powerline-render lhs)
			     (powerline-fill face-background (powerline-width rhs))
			     (powerline-render rhs)))))))

(provide 'powerline-evil-themes)
;;; powerline-evil-themes.el ends here
