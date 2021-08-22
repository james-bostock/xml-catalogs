#!/bin/sh
":"; exec emacs -batch -L . -l ert -l test-xml-catalogs.el -f ert-run-tests-batch-and-exit # -*- mode: emacs-lisp; -*-

;; Copyright (C) 2020, 2021 James Bostock

;; Author: James Bostock <james.bostock@gmail.com>
;; Keywords: lisp xml catalog
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; Unit tests for xml-catalogs.el.
;;
;; https://www.oasis-open.org/committees/download.php/14809/xml-catalogs.html

;;; Code

(require 'xml-catalogs)

(ert-deftest xml-catalogs-test-resolve-uri-1 ()
  "Test the resolution of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-resolve-uri-2 ()
  "Test the resolution of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/xmlbase/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/xmlbase/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-resolve-uri-3 ()
  "Test the resolution of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should-not (xml-catalogs-resolve-uri "http://www.example.org/not-in-catalog" catalogs))))

(ert-deftest xml-catalogs-test-resolve-uri-4 ()
  "Test the resolution of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/group/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/group/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-rewrite-uri-1 ()
  "Test the rewriting of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///usr/share/example/rewrite/test"
		   (xml-catalogs-resolve-uri "http://www.example.org/rewrite/test" catalogs)))))

(ert-deftest xml-catalogs-test-rewrite-uri-2 ()
  "Test the rewriting of URIs."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///usr/share/example/rewrite/elsewhere/test"
		   (xml-catalogs-resolve-uri "http://www.example.org/rewrite/more-specific/test" catalogs)))))

(ert-deftest xml-catalogs-test-uri-suffix-1 ()
  "Test the rewriting of URI suffixes."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "http://www.example2.org/suffix/test.html"
		   (xml-catalogs-resolve-uri "http://www.example.org/suffix/test.html" catalogs)))))

(ert-deftest xml-catalogs-test-uri-suffix-2 ()
  "Test the rewriting of URI suffixes."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "http://www.example2.org/suffix/xmlbase/test.html"
		   (xml-catalogs-resolve-uri "http://www.example.org/suffix/xmlbase/test.html" catalogs)))))

(ert-deftest xml-catalogs-test-uri-suffix-3 ()
  "Test the rewriting of URI suffixes."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "http://www.example2.org/suffix/elsewhere/test.html"
		   (xml-catalogs-resolve-uri "http://www.example.org/suffix/more-specific/test.html" catalogs)))))

(ert-deftest xml-catalogs-test-uri-suffix-4 ()
  "Test the rewriting of URI suffixes."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/group/suffix/group/test.html"
		   (xml-catalogs-resolve-uri "http://www.example.org/suffix/group/elsewhere/test.html" catalogs)))))

(ert-deftest xml-catalogs-test-delegate-uri-1 ()
  "Test the delegation of URI resolution."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/delegate/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/delegate/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-delegate-uri-2 ()
  "Test the delegation of URI resolution."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/delegate/more-specific/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/delegate/more-specific/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-delegate-uri-3 ()
  "Test the delegation of URI resolution."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should-not (xml-catalogs-resolve-uri "http://www.example.org/delegate/next/" catalogs))))

(ert-deftest xml-catalogs-test-delegate-uri-4 ()
  "Test the delegation of URI resolution."
  (let ((catalogs (list (xml-catalogs-load-catalog "next-catalog.xml"))))
    (should (equal "file:///projects/example/delegate/next/"
		   (xml-catalogs-resolve-uri "http://www.example.org/delegate/next/" catalogs)))))

(ert-deftest xml-catalogs-test-next-catalog-1 ()
  "Test the searching of an additional catalog, specified by a
nextCatalog element."
  (let ((catalogs (list (xml-catalogs-load-catalog "catalog.xml"))))
    (should (equal "file:///projects/example/next/uri/"
		   (xml-catalogs-resolve-uri "http://www.example.org/next/uri/" catalogs)))))

(ert-deftest xml-catalogs-test-non-existent-file ()
  "Test the handling of a non-existent catalog file."
  (should-error (xml-catalogs-load-catalog "non-existent-file.xml")))

(ert-deftest xml-catalogs-test-circular-catalog-1 ()
  "Test the handling of circular catalogs."
  (let ((catalogs (list (xml-catalogs-load-catalog "circular-catalog-1.xml"))))
    (let ((error (should-error (xml-catalogs-resolve-uri "http://www.example.org/not-in-catalog" catalogs))))
      (should (equal 'error (car error)))
      (should (equal "Lisp nesting exceeds ‘max-lisp-eval-depth’" (cadr error))))))

(ert-deftest xml-catalogs-test-unwrap-urn ()
  "Test the unwrapping of URNs as described in section 6.4 of the
OASIS XML Catalogs specification."
  (should (equal "http://www.example.org"
		 (xml-catalogs--unwrap-urn "http://www.example.org")))
  (should (equal "-//OASIS//DTD DocBook XML V4.1.2//EN"
		 (xml-catalogs--unwrap-urn "urn:publicid:-:OASIS:DTD+DocBook+XML+V4.1.2:EN")))
  (should (equal "++:://;;'??#%"
		 (xml-catalogs--unwrap-urn "urn:publicid:%2b%2B%3a%3A%2f%2F%3b%3B%27%3f%3F%23%25"))))

(ert-deftest xml-unset-attribute-1 ()
  "Test the unsetting of an attribute."
  (let* ((orig '("foo" (("attr1" . "val1") ("attr2" . "val2")) . ("body text")))
	 (new (xml-unset-attribute orig "attr1")))
    (should (string= "" (xml-get-attribute new "attr1")))
    (should (string= "val2" (xml-get-attribute new "attr2")))
    (should (stringp (car (xml-node-children new))))
    (should (string= "body text" (car (xml-node-children new))))))

(ert-deftest xml-unset-attribute-2 ()
  "Test the unsetting of an attribute."
  (let* ((orig '("foo" (("attr1" . "val1") ("attr2" . "val2")) . ()))
	 (new (xml-unset-attribute orig "attr1")))
    (should (string= "" (xml-get-attribute new "attr1")))
    (should (string= "val2" (xml-get-attribute new "attr2")))
    (should (not (xml-node-children new)))))

(ert-deftest xml-set-attribute ()
  "Test the setting of an attribute."
  (let* ((orig '("foo" (("attr1" . "val1") ("attr2" . "val2")) ()))
	 (new (xml-set-attribute orig "attr1" "val3")))
    (should (string= "val3" (xml-get-attribute new "attr1")))
    (should (string= "val2" (xml-get-attribute new "attr2")))))
