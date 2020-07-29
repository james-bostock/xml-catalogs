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

(when load-file-name
  (let* ((dir (file-name-directory load-file-name))
         (file (expand-file-name "catalog.rnc" dir)))
    (defvar xml-catalog-rng-schema file
      "The Relax NG schema for OASIS XML Catalogs")))

(defconst xml-catalog-catalog-namespace-uri
  (nxml-make-namespace "urn:oasis:names:tc:entity:xmlns:xml:catalog"))

(defconst xml-catalog-xml-namespace-uri
  (nxml-make-namespace "http://www.w3.org/XML/1998/namespace"))

(defconst xml-catalog--xml-base-attr
  (cons xml-catalog-xml-namespace-uri "base"))

(defun xml-catalog-load-catalogs ()
  "Load the XML catalogs listed in xml-catalog-files."
  (dolist ctlg (parse-colon-path (getenv "XML_CATALOG_FILES"))
	  (xml-catalog-load-catalog ctlg)))

(defun xml-catalog-load-catalog (ctlg-file)
  "Load the XML catalog contained in CTLG-FILE."
  (let ((schema (rng-load-schema xml-catalog-rng-schema)))
    (let ((parsed-file (rng-parse-validate-file schema ctlg-file)))
      (xml-catalog--flatten parsed-file))))

(defun xml-catalog--elem-match-p (object tagname &optional ns)
  "Return t if OBJECT is an NXML element with the specified TAGNAME"
  (and (consp object)
       (eq (or ns xml-catalog-catalog-namespace-uri) (caar object))
       (string= tagname (cdar object))))

(defun xml-catalog-resolve-uri (uri &optional ctlg)
  "Resolve URI. If CTLG is provided, use it instead of the
catalog(s) in XML-CATALOG-FILES. Returns NIL if URI cannot be
resolved."
  (let ((resolved-uri (xml-catalog--resolve-uri uri ctlg)))
    (if resolved-uri
	resolved-uri
      (let ((resolved-uri (xml-catalog--rewrite-uri uri ctlg)))
	(if resolved-uri
	    resolved-uri
	  (let ((resolved-uri (xml-catalog--uri-suffix uri ctlg)))
	    (if resolved-uri
		resolved-uri
	      nil)))))))

(defun xml-catalog--resolve-uri (uri ctlg)
  "Resolve a URI in CTLG."
  (let ((entry (seq-find (lambda (a)
			   (and (xml-catalog--elem-match-p a "uri")
				(string= (xml-get-attribute a "name") uri)))
			 (xml-node-children ctlg))))
    (if entry
	(let ((base (xml-get-attribute entry xml-catalog--xml-base-attr))
	      (resolved (xml-get-attribute-or-nil entry "uri")))
	  (if resolved
	      (concat base resolved)
	    resolved))
      entry)))


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
				     (xml-node-children ctlg))
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
				       (xml-node-children ctlg))
			   "")))
    ; If entry is a string, it will be the empty string used as the
    ; initial value for seq-reduce.
    (if (stringp entry)
	nil
      (let ((base (xml-get-attribute entry xml-catalog--xml-base-attr)))
	(concat base (xml-get-attribute entry "uri"))))))


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

(defun xml-catalog--flatten (ctlg)
  "Flatten CTLG. Any group elements are replaced by the elements
  they contain. If the group element has an xml:base attribute,
  it is copied to the containing elements."
  (let ((newbody (xml-catalog--flatten-body (xml-node-children ctlg))))
    (cons (xml-node-name ctlg)
	  (cons (xml-node-attributes ctlg)
		newbody))))

(defun xml-catalog--flatten-body (body)
  "Flatten BODY (the body of an XML  catalog)"
  (mapcan #'xml-catalog--flatten-elem (seq-remove #'stringp body)))

(defun xml-catalog--flatten-elem (elem)
     "Flatten ELEM. If ELEM is a group element, its children are
returned (setting their xml:base attribute to the value this
attribute has on the group element). If ELEM is not a group
element, return a list with ELEM as its only member."
     (if (xml-catalog--group-p elem)
	 (let ((xmlbase (xml-get-attribute-or-nil elem xml-catalog--xml-base-attr))
	       (children (seq-remove #'stringp (xml-node-children elem))))
	   (if xmlbase
	       (seq-map (lambda (x)
			  (xml-set-attribute x xml-catalog--xml-base-attr xmlbase))
			children)
	     children))
       (list elem)))

(defun xml-catalog--group-p (node)
  "Return non-nil if NODE is a group element"
  (and (eq xml-catalog-catalog-namespace-uri (caar node))
       (string= "group" (cdar node))))

; Should these be added to xml.el? That is why I have named them
; without the xml-catalog prefix.

(defun xml-unset-attribute (node attribute)
  "Delete ATTRIBUTE from NODE."
  (let ((name (xml-node-name node))
	(newattrs (assoc-delete-all attribute (xml-node-attributes node)))
	(children (xml-node-children node)))
    (cons name (cons newattrs children))))

(defun xml-set-attribute (node attribute value)
  "Set ATTRIBUTE in NODE to VALUE."
  (let* ((node2 (xml-unset-attribute node attribute))
	 (name (xml-node-name node2))
	 (newattrs (cons (cons attribute value)
			 (xml-node-attributes node2)))
	 (children (xml-node-children node2)))
    (cons name (cons newattrs children))))

(provide 'xml-catalog)

;;; xml-catalog.el ends here
