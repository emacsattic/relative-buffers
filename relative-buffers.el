(defun relative-buffers--rename-buffer ()
  (when (relative-buffers-project-p)
    (cond ((buffer-file-name) (relative-buffers-rename-file))
          ((eq major-mode 'dired-mode) (relative-buffers-rename-directory)))))

(defun relative-buffers--rename-file ()
  "Rename current file to project relative name."
  (rename-buffer
   (s-chop-prefix (relative-buffers-project-root)
                  (buffer-file-name))))

(defun relative-buffers--rename-directory ()
  "Rename current directory to project relative name."
  (let ((root (relative-buffers-project-root))
        (directory (file-truename dired-directory)))
    (and (not (s-equals-p root directory))
         (rename-buffer (s-chop-prefix root directory)))))
