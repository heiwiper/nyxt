;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests)

(define-test toggle-search-buffer-mode ()
  (let ((buffer (make-instance 'modable-buffer)))
    (with-current-buffer buffer
      (assert-true (enable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer))
      (assert-true (disable-modes* 'nyxt/search-buffer-mode:search-buffer-mode buffer)))))

;; notice the whiteSpaces in the dom!
(defmacro with-dom (&body body)
  (let ((spinneret:*html-style* :tree))
    `(plump:parse (spinneret:with-html-string (:body ,@body)))))

(defun match-count (pattern node)
  (length (nyxt/search-buffer-mode:find-matches pattern
                                                (make-instance 'document-buffer)
                                                :node node
                                                :js-highlight-matches-p nil)))

;; A simple node is a node that has a single text node child
(define-test single-match-in-simple-node ()
  (assert= 1 (match-count "match" (with-dom (:p "match")))))

(define-test matches-in-simple-node ()
  (assert= 2 (match-count "match" (with-dom (:p "match match")))))

(define-test matches-in-node ()
  (assert= 1 (match-count "match" (with-dom (:p "match" (:a "foo")))))
  (assert= 2 (match-count "match" (with-dom (:p (:a "foo") "match" (:a "bar") "match")))))

(define-test match-spanning-sibling-nodes ()
  (assert= 0 (match-count "a match" (with-dom (:p "a") (:p "match"))))
  (assert= 0 (match-count "a match" (with-dom (:p "a") (:p "foo") (:p "match")))))

(define-test match-spanning-nested-nodes ()
  ;; TODO Tweak nyxt/search-buffer-mode:find-matches to make these assertions true.
  ;; (assert= 1
  ;;          (match-count "a match"
  ;;                       (with-dom (:p "a" (:b "match")))))
  ;; (assert= 1 (match-count "a match"
  ;;                         (with-dom (:p "a" (:span) (:b "match")))))
  (assert= 0 (match-count "a match" (with-dom (:p "a" (:b "non") (:b "match"))))))



;; invariant: the matches are collected from the back

;; test logic of node-index, match-index, beg, and end?

;; test that matches aren't taken from script or style elements.
