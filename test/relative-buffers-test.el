;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'relative-buffers)
(require 'ert)
(require 's)
(require 'f)

(defvar test-directory (f-dirname load-file-name)
  "Current directory.")

;; Python.

(ert-deftest test-python-package ()
  (find-file (f-join test-directory "fixtures/python/package/subpackage/module.py"))
  (should (s-equals? (relative-buffers-python-package)
                     "package.subpackage.module")))

(ert-deftest test-python-package-init ()
  (find-file (f-join test-directory "fixtures/python/package/__init__.py"))
  (should (s-equals? (relative-buffers-python-package)
                     "package")))

(ert-deftest test-python-script ()
  (find-file (f-join test-directory "fixtures/python/nopackage.py"))
  (should (null (relative-buffers-python-package))))

;; Dired.

(ert-deftest test-dired-vc ()
  (dired (f-join test-directory "fixtures/vc/subdir/dir"))
  (should (s-equals? (relative-buffers-directory)
                     "subdir/dir/")))

(ert-deftest test-dired-vc-topdir ()
  (dired (f-join test-directory "fixtures/vc/subdir"))
  (should (s-equals? (relative-buffers-directory)
                     "subdir/")))

(ert-deftest test-dired-simple-directory ()
  (dired (f-join (f-root) "tmp"))
  (should (null (relative-buffers-directory))))

;; File.

(ert-deftest test-file-name-vc ()
  (find-file (f-join test-directory "fixtures/vc/subdir/dir/test"))
  (should (s-equals? (relative-buffers-file-name)
                     "subdir/dir/test")))

(ert-deftest test-file-name-simple-file ()
  (find-file (f-join (f-root) "tmp" "simple"))
  (should (null (relative-buffers-file-name))))

(ert-deftest test-file-name-without-file ()
  (let ((default-directory (f-join test-directory "fixtures/vc/subdir/dir/test")))
    (switch-to-buffer (generate-new-buffer "foo"))
    (should (null (buffer-file-name)))
    (should (null (relative-buffers-file-name)))))

;; Project root.

(ert-deftest test-project-root ()
  (find-file (f-join test-directory "fixtures/vc/subdir/dir/test"))
  (should (s-equals? (relative-buffers-project-root)
                     (f-slash (f-join test-directory "fixtures" "vc")))))

(ert-deftest test-not-project-root ()
  (dired (f-join (f-root) "tmp"))
  (should (null (relative-buffers-project-root))))

;; Global mode.

(ert-deftest test-open-differnt-files-with-same-name ()
  "Check if renaming work correctly for complex layout.
- each file has same name
- each file has same relative path
- each file placed in different project
README files on top of any vcs project root may cause this error."
  (unwind-protect
      (progn
        (global-relative-buffers-mode +1)
        (find-file (f-join test-directory "fixtures/same-name/a/README.rst"))
        (find-file (f-join test-directory "fixtures/same-name/b/README.rst"))
        (should (get-buffer "README.rst<2>")))
    (ignore-errors
      (global-relative-buffers-mode -1))))

(provide 'relative-buffers-test)

;;; relative-buffers-test.el ends here
