;;; color-theme-buffer-local.el --- Install color themes by buffer.
;;; Version: 0.0.1
;;; Author: Victor Borja <vic.borja@gmail.com>
;;; URL: http://github.com/vic/color-theme-buffer-local
;;; Description: Set color-theme by buffer.
;;; 

;;;
;;; Usage:
;;;
;;; (add-hook 'java-mode-hook (lambda nil
;;;   (color-theme-buffer-local 'color-theme-robin-hood (current-buffer))))

(require 'color-theme)


(defun color-theme-buffer-local-install-variables (vars buffer)
  (with-current-buffer buffer
    (let ((vars (color-theme-filter vars color-theme-legal-variables)))
      (dolist (var vars)
        (set (make-variable-buffer-local (car var)) (cdr var))))))

(defun color-theme-buffer-local-reset-faces (buffer)
  (with-current-buffer buffer
    (dolist (face (color-theme-get-faces))
      (make-variable-buffer-local 'face-remapping-alist)
      (face-remap-reset-base face))))


(defun color-theme-buffer-local-spec-compat (spec)
    (let ((props (cadar spec)))
      ;; remove stipple attribute because it causes error :( FIXME
      (when (plist-member props :stipple)
        (setq props (color-theme-plist-delete props :stipple)))
      `((t ,props))))

(defvar color-theme-buffer-local-faces-alias-alist
  '((modeline mode-line)))



(defun color-theme-buffer-local-install-face (face spec)
  (or (facep face)
      (make-empty-face face))
  ;; remove weird properties from the default face only
  (when (eq face 'default)
    (setq spec (color-theme-spec-filter spec)))
  ;; Emacs/XEmacs customization issues: filter out :bold when
  ;; the spec contains :weight, etc, such that the spec remains
  ;; "valid" for custom.
  (setq spec (color-theme-spec-compat spec))
  ;; using a spec of ((t (nil))) to reset a face doesn't work
  ;; in Emacs 21, we use the new function face-spec-reset-face
  ;; instead

  (setq spec (color-theme-buffer-local-spec-compat spec))

  (if (eq 'default face)
      (buffer-face-set (cadar spec)))

  (face-remap-set-base face (cadar spec)))

(defvar color-theme-buffer-local-face-alias
  '(
    (modeline . mode-line)
    (modeline-buffer-id . mode-line-buffer-id)
    (modeline-mousable . mode-line-mousable)
    ))

(defun color-theme-buffer-local-install-faces (faces buffer)
  (with-current-buffer buffer
    (make-variable-buffer-local 'face-remapping-alist)
    (when (not color-theme-is-cumulative)
          (color-theme-buffer-local-reset-faces buffer))
    (let ((faces (color-theme-filter faces color-theme-illegal-faces t)))
      (dolist (entry faces)
        (let ((face (nth 0 entry)) (spec (nth 1 entry)))
          (color-theme-buffer-local-install-face face spec)))

      (dolist (alias color-theme-buffer-local-face-alias)
        (when (and (assoc (car alias) faces)
                   (not (assoc (cdr alias) faces)))
          (color-theme-buffer-local-install-face
           (cdr alias)
           (cadr (assoc (car alias) faces)))))
      )))


(defun color-theme-buffer-local-install-params (params buffer)
  (setq params (color-theme-filter
		params color-theme-legal-frame-parameters))
  (make-variable-buffer-local 'buffer-face-mode-face)
  (let (default) 
    (dolist (param params)
      (when (eq (car param) 'foreground-color)
        (setq default (append default (list :foreground (cdr param)))))
      (when (eq (car param) 'background-color)
        (setq default (append default (list :background (cdr param)))))
    )
    (when default
      (setq default (append (if (listp buffer-face-mode-face)
                                (cddr buffer-face-mode-face)
                              (list buffer-face-mode-face))
                            (list  default)))
      (funcall 'buffer-face-set default))))
    

(defun color-theme-buffer-local-install (theme &optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (setq theme (color-theme-canonic theme))
  (color-theme-buffer-local-install-variables (color-theme-variables theme) buffer)
  (color-theme-buffer-local-install-faces (color-theme-faces theme) buffer)
  (color-theme-buffer-local-install-params (color-theme-frame-params theme)
                                           buffer))


;;;###autoload
(defun color-theme-buffer-local (theme-name &optional buffer)
  "Install the color-theme defined by THEME on BUFFER.

     THEME must be a symbol whose value as a function calls
     `color-theme-install' to install a theme.

     BUFFER defaults to the current buffer if not explicitly given."
  (interactive
   (list (intern (ido-completing-read "Color-Theme: "
                                       (mapcar 'symbol-name
                                               (mapcar 'car color-themes))))))
  (flet ((color-theme-install (theme)
                              (color-theme-buffer-local-install
                               theme (or buffer (current-buffer)))))
    (funcall theme-name))) 


(provide 'color-theme-buffer-local)

;;; color-theme-buffer-local.el ends here