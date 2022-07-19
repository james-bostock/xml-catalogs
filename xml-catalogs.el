;;; xml-catalogs.el --- an implementation of XML Catalogs for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021, 2022 James Bostock

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
(require 'url)
(require 'xml)

; use defcustom?
(defvar xml-catalogs-files nil
  "A list of XML Catalog files that should be searched when
  resolving entities.")

(defvar xml-catalogs nil
  "A list of XML catalogs.")

(defcustom xml-catalogs-prefer 'public
  "Indicates whether public or system entry matches are perferred.")

(defconst xml-catalogs-catalog-namespace-uri
  (nxml-make-namespace "urn:oasis:names:tc:entity:xmlns:xml:catalog"))

(defconst xml-catalogs-xml-namespace-uri
  (nxml-make-namespace "http://www.w3.org/XML/1998/namespace"))

(defconst xml-catalogs--xml-base-attr
  (cons xml-catalogs-xml-namespace-uri "base"))

(defun xml-catalogs-load-catalogs ()
  "Load the XML catalogs listed in xml-catalogs-files."
  (dolist (ctlg (split-string (getenv "XML_CATALOG_FILES")))
	  (xml-catalogs-load-catalog ctlg)))

(defun xml-catalogs-load-catalog (ctlg-file)
  "Load the XML catalog contained in CTLG-FILE."
  (let ((parsed-file (rng-parse-validate-file xml-catalogs--schema ctlg-file)))
    (let ((based-file (if (xml-get-attribute-or-nil parsed-file xml-catalogs--xml-base-attr)
			    parsed-file
			  (xml-set-attribute parsed-file xml-catalogs--xml-base-attr
					     (file-name-directory (expand-file-name ctlg-file))))))
	(xml-catalogs--flatten based-file))))

(defun xml-catalogs--elem-match-p (object tagname &optional ns)
  "Return t if OBJECT is an NXML element with the specified TAGNAME"
  (and (consp object)
       (eq (or ns xml-catalogs-catalog-namespace-uri) (caar object))
       (string= tagname (cdar object))))

(defun xml-catalogs-resolve-uri (uri &optional ctlgs)
  "Resolve URI. If CTLGS is provided, use it as the list of
catalogs to use instead of XML-CATALOGS-FILES. Returns NIL if URI
cannot be resolved."
  (catch 'delegate-fail
    (seq-some (lambda (ctlg)
		(let ((resolved-uri (xml-catalogs--resolve-uri uri ctlg)))
		  (if resolved-uri
		      resolved-uri
		    (let ((resolved-uri (xml-catalogs--rewrite-uri uri ctlg)))
		      (if resolved-uri
			  resolved-uri
			(let ((resolved-uri (xml-catalogs--uri-suffix uri ctlg)))
			  (if resolved-uri
			      resolved-uri
			    (let ((resolved-uri (xml-catalogs--delegate-uri uri ctlg)))
			      (if resolved-uri
				  resolved-uri
				(let ((resolved-uri (xml-catalogs--next-catalogs uri ctlg
										#'xml-catalogs-resolve-uri)))
				  (if resolved-uri
				      resolved-uri
				    nil)))))))))))
	      (if ctlgs
		  ctlgs
		xml-catalogs))))

(defun xml-catalogs--resolve-uri (uri ctlg)
  "Resolve a URI in CTLG."
  (let ((entry (seq-find (lambda (a)
			   (and (xml-catalogs--elem-match-p a "uri")
				(string= (xml-get-attribute a "name") uri)))
			 (xml-node-children ctlg))))
    (if entry
	(xml-catalogs--get-attr-with-base ctlg entry "uri")
      nil)))


(defun xml-catalogs--rewrite-uri (uri ctlg)
  "Rewrite URI"
  ; We take advantage of lisp's dynamic typing: we set the initial
  ; value to the empty string but, if we find a matching rewriteUri
  ; entry, the reduced value becomes that entry.
  (let ((entry (seq-reduce (lambda (a b)
			     (if (> (length a) (length (xml-get-attribute b "uriStartString")))
				 a
			       b))
			   (seq-filter (lambda (a)
				       (and (xml-catalogs--elem-match-p a "rewriteURI")
					    (string-prefix-p (xml-get-attribute a "uriStartString") uri)))
				     (xml-node-children ctlg))
			   "")))
    ; if entry is a string, it will be the empty string used as the
    ; initial value for seq-reduce.
    (if (stringp entry)
	nil
      (concat (xml-get-attribute-or-nil entry "rewritePrefix")
	      (substring uri (length (xml-get-attribute-or-nil entry "uriStartString")))))))


(defun xml-catalogs--uri-suffix (uri ctlg)
  "URI suffix"
  ; We take advantage of lisp's dynamic typing: we set the initial
  ; value to the empty string but, if we find a matching uriSuffix
  ; entry, the reduced value becomes that entry.
  (let ((entry (seq-reduce (lambda (a b)
			     (if (> (length a) (length (xml-get-attribute b "uriSuffix")))
				 a
			       b))
			   (seq-filter (lambda (a)
					 (and (xml-catalogs--elem-match-p a "uriSuffix")
					      (string-suffix-p (xml-get-attribute a "uriSuffix") uri)))
				       (xml-node-children ctlg))
			   "")))
    ; If entry is a string, it will be the empty string used as the
    ; initial value for seq-reduce.
    (if (stringp entry)
	nil
      (xml-catalogs--get-attr-with-base ctlg entry "uri"))))
;      (let ((base (xml-get-attribute entry xml-catalogs--xml-base-attr)))
;	(concat base (xml-get-attribute entry "uri"))))))

(defun xml-catalogs--delegate-uri (uri ctlg)
  "Handle delegation of URI resolution. Searches CTLG for
delegateURI entries whose uriStartString is a prefix of URI. If
none are found, returns nil. If one or more are found, the
associated catalogs are recursively passed to
xml-catalogs-resolve-uri. If the recursive invocation is not
successful, we throw 'delegate-fail."

  (let* ((delegate-catalog-elems (seq-filter (lambda (a)
					       (and (xml-catalogs--elem-match-p a "delegateURI")
						    (string-prefix-p (xml-get-attribute a "uriStartString") uri)))
					     (xml-node-children ctlg)))
	 (sorted (seq-sort (lambda (a b)
			     (> (length (xml-get-attribute a "uriStartString"))
				(length (xml-get-attribute b "uriStartString"))))
			   delegate-catalog-elems))
	 (delegate-catalog-files (seq-map (lambda (a)
					    (xml-catalogs--get-attr-with-base ctlg a "catalog"))
					  sorted))
	 (delegate-catalogs (seq-map #'xml-catalogs-load-catalog delegate-catalog-files)))
    (if delegate-catalogs
	(let ((resolved (xml-catalogs-resolve-uri uri delegate-catalogs)))
	  (if resolved
	      resolved
	    (throw 'delegate-fail nil)))
      nil)))

(defun xml-catalogs--next-catalogs (uri ctlg resolve-fn)
  "Process nextCatalog elements in CTLG."
  (let* ((next-catalog-elems (seq-filter (lambda (a)
					    (xml-catalogs--elem-match-p a "nextCatalog"))
					 (xml-node-children ctlg)))
	 (next-catalog-files (seq-map (lambda (a)
					(xml-catalogs--get-attr-with-base ctlg a "catalog"))
				      next-catalog-elems))
	 (next-catalogs (seq-map #'xml-catalogs-load-catalog next-catalog-files)))
    (funcall resolve-fn uri next-catalogs)))

(defun xml-catalogs--get-attr-with-base (ctlg elem attr)
  "Return the value of ATTR, prepending the current xml:base
value (which will either be the value of the xml:base attribute
on ELEM or, if this is not present, on CTLG, which will always be
present as we set it when we load the catalog."
  (let* ((elem-base (xml-get-attribute-or-nil elem xml-catalogs--xml-base-attr))
	 (base (if elem-base
		   elem-base
		 (xml-get-attribute ctlg xml-catalogs--xml-base-attr)))
	 (val (xml-get-attribute-or-nil elem attr))
	 ;; Note: if val is nil, url-generic-parse-url will return
	 ;; #s(url nil nil nil nil nil nil nil nil nil nil t t)
	 (url (url-generic-parse-url val)))
    (if val
	;; If url has a type, we assume that its path is absolute.
	(if (url-type url)
	    val
	  (if (file-name-absolute-p val)
	      val
	    (concat base val)))
      nil)))


(defun xml-catalogs--unwrap-urn (urn)
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

(defun xml-catalogs--flatten (ctlg)
  "Flatten CTLG. Any group elements are replaced by the elements
  they contain. If the group element has an xml:base attribute,
  it is copied to the containing elements."
  (let ((newbody (xml-catalogs--flatten-body (xml-node-children ctlg))))
    (cons (xml-node-name ctlg)
	  (cons (xml-node-attributes ctlg)
		newbody))))

(defun xml-catalogs--flatten-body (body)
  "Flatten BODY (the body of an XML  catalog)"
  (mapcan #'xml-catalogs--flatten-elem (seq-remove #'stringp body)))

(defun xml-catalogs--flatten-elem (elem)
     "Flatten ELEM. If ELEM is a group element, its children are
returned (setting their xml:base attribute to the value this
attribute has on the group element). If ELEM is not a group
element, return a list with ELEM as its only member."
     (if (xml-catalogs--group-p elem)
	 (let ((xmlbase (xml-get-attribute-or-nil elem xml-catalogs--xml-base-attr))
	       (children (seq-remove #'stringp (xml-node-children elem))))
	   (if xmlbase
	       (seq-map (lambda (x)
			  (xml-set-attribute x xml-catalogs--xml-base-attr xmlbase))
			children)
	     children))
       (list elem)))

(defun xml-catalogs--group-p (node)
  "Return non-nil if NODE is a group element"
  (and (eq xml-catalogs-catalog-namespace-uri (caar node))
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

;; The following is the output of rng-load-schema when run on the
;; RELAX NG Compact Syntax definition (generated by Trang from the
;; RELAX NG grammar in Appendix B of the XML Catalogs specification).
(defvar xml-catalogs--schema '(ref
 (element
  (name
   (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "catalog"))
  (group #2=(ref
             (group
              (optional
               (attribute
                (name
                 (nil . "id"))
                (data
                 (http://www.w3.org/2001/XMLSchema-datatypes . ID)
                 nil)))
              (zero-or-more
               (attribute
                (any-name-except
                 (choice
                  (ns-name nil)
                  (ns-name :urn:oasis:names:tc:entity:xmlns:xml:catalog)))
                #4=(text))))
             "OptionalAttributes")
          (optional #3=(ref
                        (attribute
                         (name
                          (nil . "prefer"))
                         (ref
                          (choice
                           (value #1=(http://relaxng.org/ns/structure/1.0 . token)
                                   "system" nil)
                           (value #1# "public" nil))
                          "systemOrPublic"))
                        "PreferAttribute"))
          (one-or-more
           (choice
            (ref
             (element
              (name
               (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "group"))
              (group #2#
                      (optional #3#)
                      (one-or-more
                       (choice #8=(ref
                                   (element
                                    (name
                                     (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "public"))
                                    (group
                                     (attribute
                                      (name
                                       (nil . "publicId"))
                                      (ref
                                       (ref
                                        (data
                                         (http://www.w3.org/2001/XMLSchema-datatypes . string)
                                         ((pattern . "[a-zA-Z0-9\\-'\\(\\)+,./:=?;!*#@$_% ]*")))
                                        "pubIdChars")
                                       "publicIdentifier"))
                                     (attribute
                                      (name
                                       (nil . "uri"))
                                      #5=(ref
                                          (data
                                           (http://www.w3.org/2001/XMLSchema-datatypes . anyURI)
                                           nil)
                                          "uriReference"))
                                     #2# #6=(empty)))
                                   "Public")
                                #9=(ref
                                    (element
                                     (name
                                      (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "system"))
                                     (group
                                      (attribute
                                       (name
                                        (nil . "systemId"))
                                       #4#)
                                      (attribute
                                       (name
                                        (nil . "uri"))
                                       #5#)
                                      #2# #6#))
                                    "System")
                                #10=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "uri"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "name"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "uri"))
                                        #5#)
                                       #2# #6#))
                                     "Uri")
                                #11=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "rewriteSystem"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "systemIdStartString"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "rewritePrefix"))
                                        #4#)
                                       #2# #6#))
                                     "RewriteSystem")
                                #12=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "rewriteURI"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "uriStartString"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "rewritePrefix"))
                                        #4#)
                                       #2# #6#))
                                     "RewriteURI")
                                #13=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "systemSuffix"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "systemIdSuffix"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "uri"))
                                        #5#)
                                       #2# #6#))
                                     "SystemSuffix")
                                #14=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "uriSuffix"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "uriSuffix"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "uri"))
                                        #5#)
                                       #2# #6#))
                                     "UriSuffix")
                                #15=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "delegatePublic"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "publicIdStartString"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "catalog"))
                                        #4#)
                                       #2# #6#))
                                     "DelegatePublic")
                                #16=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "delegateSystem"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "systemIdStartString"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "catalog"))
                                        #4#)
                                       #2# #6#))
                                     "DelegateSystem")
                                #17=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "delegateURI"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "uriStartString"))
                                        #4#)
                                       (attribute
                                        (name
                                         (nil . "catalog"))
                                        #4#)
                                       #2# #6#))
                                     "DelegateURI")
                                #18=(ref
                                     (element
                                      (name
                                       (:urn:oasis:names:tc:entity:xmlns:xml:catalog . "nextCatalog"))
                                      (group
                                       (attribute
                                        (name
                                         (nil . "catalog"))
                                        #4#)
                                       #2# #6#))
                                     "NextCatalog")
                                #7=(ref
                                    (element
                                     (any-name-except
                                      (choice
                                       (ns-name nil)
                                       (ns-name :urn:oasis:names:tc:entity:xmlns:xml:catalog)))
                                     (group
                                      (zero-or-more
                                       (attribute
                                        (any-name)
                                        #4#))
                                      (choice #4# #7#)))
                                    "AnyOtherElement")))))
             "Group")
            #8# #9# #10# #11# #12# #13# #14# #15# #16# #17# #18# #7#))))
 "Catalog"))

(provide 'xml-catalogs)

;;; xml-catalog.el ends here
