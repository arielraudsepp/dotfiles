(defgroup blinders nil
  "Put on your blinders so you can focus on one line at a time"
  :group 'convenience)

(defun blinders-mode--remove-blinders ()
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'blinders))

(defun blinders-mode--refresh-blinders ()
  (interactive)
  (let ((before-ov (make-overlay (point-min)
                                 (line-beginning-position 0)))
        (after-ov (make-overlay (line-end-position 2)
                                (point-max))))
    (blinders-mode--remove-blinders)
    (overlay-put before-ov 'category 'blinders)
    (overlay-put before-ov 'face 'blinders-hidden-face)
    (overlay-put after-ov 'category 'blinders)
    (overlay-put after-ov 'face 'blinders-hidden-face)))

;;;###autoload
(define-minor-mode blinders-mode
  "Mode for showing only one line of the buffer at a time"
  :init-value nil
  :lighter " blinders"
  (if blinders-mode
      (progn
        (defface blinders-hidden-face
          (list (list t (list :background (face-attribute 'default :background)
                              :foreground (face-attribute 'default :background))))
          "blinders-mode hidden face"
          :group 'blinders)
        (setq-local post-command-hook (cons 'blinders-mode--refresh-blinders post-command-hook)))
    (progn
      (setq-local post-command-hook (delete 'blinders-mode--refresh-blinders post-command-hook))
      (blinders-mode--remove-blinders))))

(provide 'blinders-mode)
