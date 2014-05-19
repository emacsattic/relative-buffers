;;; relative-buffers-test.el --- relative-buffers test suite

;;; Commentary:

;;; Code:

(require 'relative-buffers)
(require 'ert)
(require 's)

(ert-deftest test-python-package ()
  (find-file "test/fixtures/python/package/subpackage/module.py")
  (should (s-equals? (relative-buffers-python-package)
                     "package.subpackage.module")))

(ert-deftest test-python-script ()
  (find-file "test/fixtures/python/nopackage.py")
  (should (null (relative-buffers-python-package))))

(provide 'relative-buffers-test)

;;; relative-buffers-test.el ends here
