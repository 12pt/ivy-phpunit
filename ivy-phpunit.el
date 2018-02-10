;; ivy-phpunit.el --- Ivy integration for phpunit.el

;; Copyright (C) 2018, 12pt

;; Author: 12pt
;; URL: https://github.com/12pt/ivy-phpunit.el
;; Version: 0.0.1
;; Keywords: ivy phpunit php

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You should always be able to find the latest version here at <URL:https://github.com/12pt/ivy-phpunit.el/>
;; At the moment this is mostly a port of helm-phpunit to ivy, though more features are planned.

;; Usage:

;; Require the package:
;; (require 'ivy-phpunit)
;; And then call ivy-phpunit-select-test - you'll probably want to assign it to a key combo.

(require 'phpunit)
(require 'ivy)

(defgroup ivy-phpunit nil
  "Quick running of PHPUnit tests."
  :group 'convenience)

(defcustom ivy-phpunit-ignorelist '("setUp" "tearDown")
  "Functions to ignore when listing test candidates."
  :type '(repeat string)
  :options '("setUpBeforeClass" "tearDownAfterClass")
  :group 'ivy-phpunit)

;; in the future maybe we'll check for a phpunit.xml file and get the config there.
(defcustom ivy-phpunit-test-regex "Test.php$"
  "What filenames must match for ivy-phpunit to consider them a test class."
  :type 'string
  :options '(".php$")
  :group 'ivy-phpunit)

(defun ivy-phpunit-find-funcs ()
  "Find all the PHP function names in the current buffer and insert them into a list."
  (let (funcs '())
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward-regexp php-beginning-of-defun-regexp nil t)
        (unless (member (match-string-no-properties 1) ivy-phpunit-ignorelist)
          (add-to-list 'funcs (match-string-no-properties 1)))))
    funcs))

(defun ivy-phpunit-test-func (func-name &optional filename)
  "Test a single test by its name.
If non-nil, use FILENAME as the name of the file the test class/functionName exists in."
  (let* ((filename (or filename buffer-file-name))
         (args (s-concat
                filename 
                " --filter '" (phpunit-get-current-class) "::" func-name "'"))) ; select the test
    (phpunit-run args)))

;;-----------------------------------------------------------------------------------------------

(defun ivy-phpunit-list-test-classes ()
  (interactive)
  "Find all the test classes in this directory. If called interactively, allow the user
to quick-switch via ivy to the class. If not, just return a list of classes."
  (let ((tests
         (directory-files default-directory nil ivy-phpunit-test-regex)))
    (if (called-interactively-p 'any)
        (ivy-read "Select a class: " tests
                  :sort t
                  :caller 'ivy-phpunit-list-test-classes
                  :action (lambda (file) (find-file file)))
      tests)))

(defun ivy-phpunit-test-class ()
  (interactive)
  "Find all test classes in the current directory and enable the user to run tests on one."
  (ivy-read "Class to test: " (ivy-phpunit-list-test-classes)
            :sort t
            :caller 'ivy-phpunit-test-class
            :action (lambda (x) (phpunit-run (file-truename x)))))

(defun ivy-phpunit-test-function ()
  (interactive)
  "Find all the test functions in the buffer."
  (ivy-read "Function to test: " (ivy-phpunit-find-funcs)
            :sort t
            :caller 'ivy-phpunit-select-test
            :action (lambda (x) (ivy-phpunit-test-func x))))

(provide 'ivy-phpunit)

;;; ivy-phpunit.el ends here
