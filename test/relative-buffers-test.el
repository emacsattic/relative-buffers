;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'relative-buffers)
(require 'ert)
(require 's)
(require 'f)

(defvar test-directory (f-dirname load-file-name)
  "Current directory.")

(ert-deftest test-python-package ()
  (find-file "test/fixtures/python/package/subpackage/module.py")
  (should (s-equals? (relative-buffers-python-package)
                     "package.subpackage.module")))

(ert-deftest test-python-package-init ()
  (find-file "test/fixtures/python/package/__init__.py")
  (should (s-equals? (relative-buffers-python-package)
                     "package")))

(ert-deftest test-python-script ()
  (find-file "test/fixtures/python/nopackage.py")
  (should (null (relative-buffers-python-package))))

(ert-deftest test-dired-vc ()
  (dired "test/fixtures/vc/subdir/dir")
  (should (s-equals? (relative-buffers-directory)
                     "subdir/dir/")))

(ert-deftest test-dired-vc-topdir ()
  (dired "test/fixtures/vc/subdir")
  (should (s-equals? (relative-buffers-directory)
                     "subdir/")))

(ert-deftest test-dired-simple-directory ()
  (dired (f-join (f-root) "tmp"))
  (should (null (relative-buffers-directory))))

(ert-deftest test-file-name-vc ()
  (find-file "test/fixtures/vc/subdir/dir/test")
  (should (s-equals? (relative-buffers-file-name)
                     "subdir/dir/test")))

(ert-deftest test-file-name-simple-file ()
  (find-file (f-join (f-root) "tmp" "simple"))
  (should (null (relative-buffers-file-name))))

(ert-deftest test-file-name-without-file ()
  (let ((default-directory (f-full "test/fixtures/vc/subdir/dir/test")))
    (switch-to-buffer (generate-new-buffer "foo"))
    (should (null (buffer-file-name)))
    (should (null (relative-buffers-file-name)))))

(ert-deftest test-project-root ()
  (find-file "test/fixtures/vc/subdir/dir/test")
  (should (s-equals? (relative-buffers-project-root)
                     (f-slash (f-join test-directory "fixtures" "vc")))))

(ert-deftest test-not-project-root ()
  (dired (f-join (f-root) "tmp"))
  (should (null (relative-buffers-project-root))))

(provide 'relative-buffers-test)

;;; relative-buffers-test.el ends here
