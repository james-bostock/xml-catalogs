#!/bin/sh
":"; exec emacs -batch -L . -l ert -l test-xml-catalog.el -f ert-run-tests-batch-and-exit # -*- mode: emacs-lisp; -*-

;; Copyright (C) 2020 James Bostock

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

;; Unit tests for xml-catalog.el.
;;
;; https://www.oasis-open.org/committees/download.php/14809/xml-catalogs.html

;;; Code

(require 'xml-catalog)

(ert-deftest xml-catalog-test-resolve-uri-1 ()
  "Test the resolution of URIs."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "file:///projects/example/uri/"
		   (xml-catalog-resolve-uri "http://www.example.org/uri/" catalog)))))

(ert-deftest xml-catalog-test-resolve-uri-2 ()
  "Test the resolution of URIs."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "file:///projects/example/xmlbase/uri/"
		   (xml-catalog-resolve-uri "http://www.example.org/xmlbase/uri/" catalog)))))

(ert-deftest xml-catalog-test-resolve-uri-3 ()
  "Test the resolution of URIs."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "http://www.example.org/not-in-catalog"
		   (xml-catalog-resolve-uri "http://www.example.org/not-in-catalog" catalog)))))

(ert-deftest xml-catalog-test-rewrite-uri-1 ()
  "Test the rewriting of URIs."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "file:///usr/share/example/rewrite/test"
		   (xml-catalog-resolve-uri "http://www.example.org/rewrite/test" catalog)))))

(ert-deftest xml-catalog-test-rewrite-uri-2 ()
  "Test the rewriting of URIs."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "file:///usr/share/example/rewrite/elsewhere/test"
		   (xml-catalog-resolve-uri "http://www.example.org/rewrite/more-specific/test" catalog)))))

(ert-deftest xml-catalog-test-uri-suffix-1 ()
  "Test the rewriting of URI suffixes."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "http://www.example2.org/suffix/test.html"
		   (xml-catalog-resolve-uri "http://www.example.org/suffix/test.html" catalog)))))

(ert-deftest xml-catalog-test-uri-suffix-2 ()
  "Test the rewriting of URI suffixes."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "http://www.example2.org/suffix/xmlbase/test.html"
		   (xml-catalog-resolve-uri "http://www.example.org/suffix/xmlbase/test.html" catalog)))))

(ert-deftest xml-catalog-test-uri-suffix-3 ()
  "Test the rewriting of URI suffixes."
  (let ((catalog (xml-catalog-load-catalog "catalog.xml")))
    (should (equal "http://www.example2.org/suffix/elsewhere/test.html"
		   (xml-catalog-resolve-uri "http://www.example.org/suffix/more-specific/test.html" catalog)))))

(ert-deftest xml-catalog-test-unwrap-urn ()
  "Test the unwrapping of URNs as described in section 6.4 of the
OASIS XML Catalogs specification."
  (should (equal "http://www.example.org"
		 (xml-catalog--unwrap-urn "http://www.example.org")))
  (should (equal "-//OASIS//DTD DocBook XML V4.1.2//EN"
		 (xml-catalog--unwrap-urn "urn:publicid:-:OASIS:DTD+DocBook+XML+V4.1.2:EN")))
  (should (equal "++:://;;'??#%"
		 (xml-catalog--unwrap-urn "urn:publicid:%2b%2B%3a%3A%2f%2F%3b%3B%27%3f%3F%23%25"))))
