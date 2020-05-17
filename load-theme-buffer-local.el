;; this works.
;; from cli:
;;(vt-dev-load-theme-buffer-local 'wombat vt-buf-messages)
;;(vt-dev-load-theme-buffer-local 'wheatgrass vt-buf-advice)
;;(vt-dev-load-theme-buffer-local 'tango vt-buf-advice)
;;
;; and from mini-buffer as well



(defun custom-theme-buffer-local-spec-attrs (spec)
  (let* ((spec (face-spec-choose spec))
         attrs)
    (while spec
      (when (assq (car spec) face-x-resources)
        (push (car spec) attrs)
        (push (cadr spec) attrs))
      (setq spec (cddr spec)))
    (nreverse attrs)))

(defun custom-theme-buffer-local-recalc-face (face buffer)
  (with-current-buffer buffer
    (let (attrs)
    
    (if (get face 'face-alias)
        (setq face (get face 'face-alias)))
    
    ;; first set the default spec
    (or (get face 'customized-face)
        (get face 'saved-face)
        (setq attrs
              (append
               attrs
               (custom-theme-buffer-local-spec-attrs
                (face-default-spec face)))))

    (let ((theme-faces (reverse (get face 'theme-face))))
      (dolist (spec theme-faces)
        (setq attrs (append
                     attrs
                     (custom-theme-buffer-local-spec-attrs (cadr spec))))))

    (and (get face 'face-override-spec)
         (setq attrs (append
                      attrs
                      (custom-theme-buffer-local-spec-attrs
                       (get face 'face-override-spec)))))

    (face-remap-set-base face attrs))))

(defun custom-theme-buffer-local-recalc-variable (variable buffer)
  (with-current-buffer buffer
    (make-variable-buffer-local variable)
    (let ((valspec (custom-variable-theme-value variable)))
      (if valspec
          (put variable 'saved-value valspec)
        (setq valspec (get variable 'standard-value)))
      (if (and valspec
               (or (get variable 'force-value)
                   (default-boundp variable)))
          (funcall (or (get variable 'custom-set) 'set-default) variable
                   (eval (car valspec)))))))

(defun load-theme-buffer-local (theme &optional buffer no-confirm no-enable)
  "Load an Emacs24 THEME only in BUFFER."
  (interactive
   (list (intern (completing-read
                  "Install theme: "
                  (mapcar 'symbol-name (custom-available-themes))))
         (read-buffer "on Buffer: " (current-buffer) t)))
  (or buffer (setq buffer (current-buffer)))
  ;;vt-new
      (defadvice custom-theme-recalc-face (around vt-recalc-face-around)
      (progn
        (message "vt: entered vt-custom-theme-recalc-face-around advice, symbol=%s, buffer=%s" symbol buffer)
        ;; Note: 'buffer is bound at definition time e.g when this devadvice is created
        ;; and 'symbol is bound at runtime, when 'load-theme -> 'enable-theme is called
        ;; We obviously have to bind the buffer at definition time, since 'buffer will
        ;; not be available at 'enable-theme run time, since it is *not* a buffer level
        ;; function
        (custom-theme-buffer-local-recalc-face symbol buffer)
        ;; Note: we do not call the advised function at all.  All the logic 
        ;; is in the monkey patch itelf (this devadvice call).  We leave
        ;; the 'ad-do-it' line commented out to make this clear.  Normally, ad-do-it
        ;; will transfer control to the overridden function.
        ;;ad-do-it
        (message "vt vt-custom-theme-recalc-face-around-advice: exited")))
      (ad-enable-advice 'custom-theme-recalc-face 'around 'vt-recalc-face-around)
      (ad-activate 'custom-theme-recalc-face)
      (load-theme theme))

(provide 'load-theme-buffer-local)

  ;;vt-end
  ;; (flet ((custom-theme-recalc-face
  ;;         (symbol) (custom-theme-buffer-local-recalc-face symbol buffer))
  ;;        (custom-theme-recalc-variable
  ;;         (symbol) (custom-theme-buffer-local-recalc-variable symbol buffer)))
  ;;   (load-theme theme no-confirm no-enable)))

;;vt end
