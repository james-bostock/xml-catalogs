;;; xml-catalog.el --- an implementation of XML Catalogs for Emacs -*- lexical-binding: t; -*-

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

;; Code for resolving entities using XML Catalogs
;;
;; https://www.oasis-open.org/committees/download.php/14809/xml-catalogs.html
;;
;; At present, only resolution of URIs is supported.

;;; Code

(require 'nxml-util)
(require 'rng-loc)
(require 'rng-parse)
(require 'xml)

; use defcustom?
(defvar xml-catalog-files nil
  "A list of XML Catalog files that should be searched when
  resolving entities.")

(defvar xml-catalogs nil
  "A list of XML catalogs.")

(defcustom xml-catalog-prefer 'public
  "Indicates whether public or system entry matches are perferred.")

; TODO: fix hard-coded absolute path
(defvar xml-catalog-rng-schema "/home/jsb/src/emacs/xml-catalog/catalog.rnc"
  "The Relax NG schema for OASIS XML Catalogs")

(defconst xml-catalog-catalog-namespace-uri
  (nxml-make-namespace "urn:oasis:names:tc:entity:xmlns:xml:catalog"))

(defun xml-catalog-load-catalogs ()
  "Load the XML catalogs listed in xml-catalog-files."
  (dolist ctlg (parse-colon-path (getenv "XML_CATALOG_FILES"))
	  (xml-catalog-load-catalog ctlg)))

(defun xml-catalog-load-catalog (ctlg-file)
  "Load the XML catalog contained in CTLG-FILE."
  (let ((schema (rng-load-schema xml-catalog-rng-schema)))
    (let ((parsed-file (rng-parse-validate-file schema ctlg-file)))
      parsed-file)))

(defun xml-catalog--elem-match-p (object tagname &optional ns)
  "Return t if OBJECT is an NXML element with the specified TAGNAME"
  (and (consp object)
       (eq (or ns xml-catalog-catalog-namespace-uri) (caar object))
       (string= tagname (cdar object))))

(defun xml-catalog-resolve-uri (uri &optional ctlg)
  "Resolve URI. If CTLG is provided, use it instead of the
catalog(s) in XML-CATALOG-FILES."
  (let ((resolved-uri (xml-catalog--resolve-uri uri ctlg)))
    (if resolved-uri
	resolved-uri
      (let ((resolved-uri (xml-catalog--rewrite-uri uri ctlg)))
	(if resolved-uri
	    resolved-uri
	  (let ((resolved-uri (xml-catalog--uri-suffix uri ctlg)))
	    (if resolved-uri
		resolved-uri
	      uri)))))))

(defun xml-catalog--resolve-uri (uri ctlg)
  "Resolve a URI in CTLG."
  (let ((entry (seq-find (lambda (a)
			   (and (xml-catalog--elem-match-p a "uri")
				(string= (xml-get-attribute a "name") uri)))
			 (cddr ctlg))))
    (xml-get-attribute-or-nil entry "uri")))


(defun xml-catalog--rewrite-uri (uri ctlg)
  "Rewrite URI"
  ; We take advantage of lisp's dynamic typing: we set the initial
  ; value to the empty string but, if we find a matching rewriteUri
  ; entry, the reduced value becomes that entry.
  (let ((entry (seq-reduce (lambda (a b)
			     (if (> (length a) (length (xml-get-attribute b "uriStartString")))
				 a
			       b))
			   (seq-filter (lambda (a)
				       (and (xml-catalog--elem-match-p a "rewriteURI")
					    (string-prefix-p (xml-get-attribute a "uriStartString") uri)))
				     (cddr ctlg))
			   "")))
    ; if entry is a string, it will be the empty string used as the
    ; initial value for seq-reduce.
    (if (stringp entry)
	nil
      (concat (xml-get-attribute-or-nil entry "rewritePrefix")
	      (substring uri (length (xml-get-attribute-or-nil entry "uriStartString")))))))


(defun xml-catalog--uri-suffix (uri ctlg)
  "URI suffix"
  ; We take advantage of lisp's dynamic typing: we set the initial
  ; value to the empty string but, if we find a matching uriSuffix
  ; entry, the reduced value becomes that entry.
  (let ((entry (seq-reduce (lambda (a b)
			     (if (> (length a) (length (xml-get-attribute b "uriSuffix")))
				 a
			       b))
			   (seq-filter (lambda (a)
				       (and (xml-catalog--elem-match-p a "uriSuffix")
					    (string-suffix-p (xml-get-attribute a "uriSuffix") uri)))
				     (cddr ctlg))
			   "")))
    ; TODO: if uri is relative, need to make it absolute with the base
    ; URI currently in effect
    ; if entry is a string, it will be the empty string used as the
    ; initial value for seq-reduce.
    (if (stringp entry)
	nil
      (xml-get-attribute-or-nil entry "uri"))))


(defun xml-catalog--unwrap-urn (urn)
  "Unwrap URN as specified in section 6.4 of the OASIS XML
Catalogs specification."
  (if (string-prefix-p "urn:publicid:" urn)
      (progn
	(setq urn (substring urn (length "urn:publicid:")))
	(setq i 0)
	(setq unwrapped "")
	(while (< i (length urn))
	  (let ((c (aref urn i)))
	    (cond ((char-equal c ?+) (setq unwrapped (concat unwrapped " ")))
		  ((char-equal c ?:) (setq unwrapped (concat unwrapped "//")))
		  ((char-equal c ?\;) (setq unwrapped (concat unwrapped "::")))
		  ((char-equal c ?\%) (let ((s (substring urn (1+ i) (+ i 3))))
				       (cond ((string= s "23")
					      (setq unwrapped (concat unwrapped "#"))
					      (setq i (+ i 2)))
					     ((string= s "25")
					      (setq unwrapped (concat unwrapped "%"))
					      (setq i (+ i 2)))
					     ((string= s "27")
					      (setq unwrapped (concat unwrapped "'"))
					      (setq i (+ i 2)))
					     ((string= (upcase s) "2B")
					      (setq unwrapped (concat unwrapped "+"))
					      (setq i (+ i 2)))
					     ((string= (upcase s) "2F")
					      (setq unwrapped (concat unwrapped "/"))
					      (setq i (+ i 2)))
					     ((string= (upcase s) "3A")
					      (setq unwrapped (concat unwrapped ":"))
					      (setq i (+ i 2)))
					     ((string= (upcase s) "3B")
					      (setq unwrapped (concat unwrapped ";"))
					      (setq i (+ i 2)))
					     ((string= (upcase s) "3F")
					      (setq unwrapped (concat unwrapped "?"))
					      (setq i (+ i 2)))
					     (t
					      (setq unwrapped (concat unwrapped "%"))))))
		  (t (setq unwrapped (concat unwrapped (char-to-string c))))))
	  (setq i (1+ i)))
	unwrapped)
    urn))

(provide 'xml-catalog)

;;; xml-catalog.el ends here
