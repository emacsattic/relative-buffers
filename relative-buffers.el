;;; relative-buffers.el --- Emacs buffers naming convention

;; Copyright (C) 2014 by Malyshev Artem

;; Author: Malyshev Artem <proofit404@gmail.com>
;; URL: https://github.com/proofit404/relative-buffers
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun relative-buffers-name ()
  "Give current buffer a relative name."
  (cl-case major-mode
    (python-mode (relative-buffers-python-package))
    (dired-mode (relative-buffers-directory))
    (otherwise (relative-buffers-file-name))))

(defun relative-buffers-python-package ()
  "Python module relative to package."
  (error "Not implemented"))

(defun relative-buffers-directory ()
  "Directory relative to project root."
  (let ((root (relative-buffers-project-root))
        (directory (file-truename dired-directory)))
    (unless (s-equals-p root directory)
      (s-chop-prefix root directory))))

(defun relative-buffers-file-name ()
  "File name relative to project root."
  (s-chop-prefix
   (relative-buffers-project-root)
   (buffer-file-name)))

(provide 'relative-buffers)

;;; relative-buffers.el ends here
