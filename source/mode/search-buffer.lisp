;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/search-buffer-mode
    (:documentation "Incremental in-buffer search."))
(in-package :nyxt/search-buffer-mode)

(define-mode search-buffer-mode ()
  "Mode for searching text withing."
  ((visible-in-status-p nil)
   (rememberable-p nil)
   (show-search-hint-scope-p
    nil
    :type boolean
    :documentation "Whether `style' is applied to parent element where a match
is found.")
   (style
    (theme:themed-css (theme *browser*)
      `("[nyxt-search-hint]"
        :background-color ,(str:concat theme:secondary " !important")
        :color ,(str:concat theme:on-secondary " !important")
        :z-index #.(1- (expt 2 31)))
      `(".nyxt-current-search-hint"
        :background-color ,(str:concat theme:accent " !important")
        :color ,(str:concat theme:on-accent " !important"))
      `(".nyxt-search-element-hint"
        ;; Black with opacity set to 20%
        ;; TODO If the theme colors would be always specified in HEX, it would
        ;; be trivial to add opacity to theme:on-background:
        ;; :background-color ,(str:concat theme:on-background "14")
        :background-color "#00000014"))
    :documentation "The style of the search overlays.")
   (keyscheme-map
    (define-keyscheme-map "search-buffer-mode" ()
      keyscheme:cua
      (list
       "C-f" 'search-buffer
       "f3" 'search-buffer
       "M-f" 'remove-search-hints)
      keyscheme:emacs
      (list
       "C-s s" 'search-buffer
       "C-s k" 'remove-search-hints)
      keyscheme:vi-normal
      (list
       "/" 'search-buffer
       "?" 'remove-search-hints))))
  (:toggler-command-p nil))

(define-class search-match ()
  ((identifier)
   (element)
   (node-index)
   (match-index)
   (beg)
   (end)
   (body)
   (buffer)
   (js-ready-p))
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod (setf js-ready-p) (value (match search-match))
  (setf (slot-value match 'js-ready-p) value)
  (when value (add-hint-elements (identifier match)
                                 (node-index match)
                                 (match-index match)
                                 (beg match)
                                 (end match))))

;; move to utilities.lisp?
;; the min length is (+ len-match (* len-ellipsis 2))
;; if len-max is lower than that, scream with an error
(defun format-match-string (str beg end &optional (len-max 40) (ellipsis "[...]"))
  "TODO"
  (let ((len-str (length str))
        (len-ellipsis (length ellipsis))
        (len-match (1+ (- end beg))))
    (cond ((or (< beg 0) (> end len-str)) (error "Match out of bounds."))
          ((>= len-max len-str) str)
          ((> len-match len-max)
           (str:concat (subseq str 0 (- len-max len-ellipsis))
                       ellipsis))
          ((> (length str) len-max)
           (let* ((delta (floor (/ (- len-max len-match) 2)))
                  (new-beg (max 0 (- beg delta)))
                  (new-end (min (length str) (+ end delta)))
                  (beg-omitted-p (not (zerop new-beg)))
                  (end-omitted-p (not (= new-end (length str)))))
             (str:concat (when beg-omitted-p ellipsis)
                         (subseq str
                                 (if beg-omitted-p (+ new-beg len-ellipsis) new-beg)
                                 (if end-omitted-p (- new-end len-ellipsis) new-end))
                         (when end-omitted-p ellipsis)))))))

(defmethod prompter:object-attributes ((match search-match) (source prompter:source))
  (declare (ignore source))
  `(("Text" ,(format-match-string (body match) (beg match) (end match)) nil 3)
    ("Buffer ID" ,(id (buffer match)))
    ("Buffer title" ,(title (buffer match)) nil 2)))

;; move to utilities.lisp?
(defun find-all-matches (pattern str)
  "TODO"
  (loop with idxs
        with s = 0
        with len = (length pattern)
        while s
        do (when (setf s (search pattern str :start2 s))
             (push (list s (+ len s)) idxs)
             (incf s len))
        finally (return (nreverse idxs))))

(export-always 'find-matches)
(defun find-matches (input buffer &key (node (elt (clss:select "body" (document-model buffer)) 0)) (js-highlight-matches-p t))
  (with-current-buffer buffer
    (let ((all-matches))
      (labels ((r (node)
                 (loop for child across (plump:children node) and index from 0
                       do (typecase child
                            (plump:text-node
                             (alex:when-let*
                                 ((_ (and (not (plump:fulltext-element-p node))
                                          (not (nyxt/dom:noscript-element-p node))
                                          (not (nyxt/dom:span-element-p node))
                                          (not (nyxt/dom:div-element-p node))))
                                  (text (plump:text child))
                                  (matches (find-all-matches input text)))
                               (loop for match in matches
                                     do (push (make-instance
                                               'search-match
                                               :element node
                                               :identifier (nyxt/dom:get-nyxt-id node)
                                               :node-index index
                                               :beg (first match)
                                               :end (second match)
                                               :body text
                                               :buffer buffer)
                                              all-matches))))
                            (plump:nesting-node (r child))))))
        (r node))
      (when js-highlight-matches-p
        (loop for match in all-matches and index from (length all-matches) downto 1
              do (setf (match-index match) (write-to-string index))
              do (setf (js-ready-p match) t)))
      (nreverse all-matches))))

;; TODO how to cancel JS code has that been sent to run async?
(define-parenscript-async add-hint-elements (id node-index match-index beg end)
  (let ((elem (nyxt/ps:qs-nyxt-id document (ps:lisp id)))
        (range (ps:chain document (create-range)))
        (match (ps:chain document (create-element "span"))))
    ;; Ensure text nodes aren't empty and adjacent ones are concatenated.
    (ps:chain elem (normalize))
    (let ((child (ps:@ elem child-nodes (ps:lisp node-index))))
      (ps:chain range (set-start child (ps:lisp beg)))
      (ps:chain range (set-end child (ps:lisp end)))
      (ps:chain range (surround-contents match))
      (ps:chain match (set-attribute "nyxt-search-hint" (ps:lisp match-index))))
    ;; TODO Extract so that it's not evaluated so many times.
    (when (ps:lisp (show-search-hint-scope-p (find-submode 'search-buffer-mode)))
      (ps:chain elem class-list (add "nyxt-search-element-hint")))))

(define-command remove-search-hints ()
  "Remove all search hints."
  (ps-eval
    (dolist (match (nyxt/ps:qsa document "[nyxt-search-hint]"))
      (ps:chain match parent-node (insert-before (ps:chain match first-child) match))
      (ps:chain match (remove)))
    (when (ps:lisp (show-search-hint-scope-p (find-submode 'search-buffer-mode)))
      (ps:dolist (element (nyxt/ps:qsa document ".nyxt-search-element-hint"))
        (ps:chain element class-list (remove "nyxt-search-element-hint"))))))

(define-parenscript highlight-current-hint (&key selector scroll)
  ;; There should be, at most, one element with the
  ;; "nyxt-current-search-hint" class.
  ;; querySelectAll, unlike querySelect, handles the case when none are
  ;; found.
  (ps:dolist (elem (nyxt/ps:qsa document ".nyxt-current-search-hint"))
    (ps:chain elem class-list (remove "nyxt-current-search-hint")))
  (let ((elem (nyxt/ps:qs document (ps:lisp selector))))
    (when elem
      (unless (ps:chain elem class-list (contains "nyxt-current-search-hint"))
        (ps:chain elem class-list (add "nyxt-current-search-hint")))
      (when (ps:lisp scroll)
        (ps:chain elem (scroll-into-view (ps:create block "center")))))))

(define-class search-buffer-source (prompter:source)
  ((case-sensitive-p nil)
   (buffer (current-buffer))
   (minimum-search-length 1)
   (prompter:name "Search buffer")
   (prompter:actions-on-current-suggestion-enabled-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (remove-search-hints)
      (when (>= (length input) (minimum-search-length source))
        ;; KLUDGE This is a hacky solution to the fact we don't cancel the
        ;; execution of JS defined by add-hint-elements
        (sleep 0.15)
        (find-matches input (buffer source)))))
   (prompter:actions-on-current-suggestion
    (lambda-command highlight-match (suggestion)
      "Scroll to search match."
      (with-current-buffer (set-current-buffer (buffer suggestion) :focus nil)
        (highlight-current-hint :selector (format nil "[nyxt-search-hint=\"~a\"]"
                                                  (match-index suggestion))
                                :scroll t))))
   (prompter:constructor (lambda (source)
                           (declare (ignore source))
                           (add-stylesheet (style (find-submode 'search-buffer-mode)))))
   (prompter:destructor (lambda (prompter source)
                          (declare (ignore prompter source))
                          (unless (keep-search-hints-p (current-buffer))
                            (remove-search-hints)))))
  (:export-accessor-names-p t)
  (:export-class-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:metaclass user-class))

(defmethod initialize-instance :after ((source search-buffer-source) &key)
  (setf (prompter:name source)
        (format nil "~a (~a+ characters)"
                (prompter:name source)
                (minimum-search-length source))))

(define-command search-buffer (&key case-sensitive-p)
  "Search on the current buffer.
If you want to remove the search hints when you close the search
prompt, Set BUFFER's `keep-search-hints-p' slot to nil.

Example:

  (define-configuration buffer
    ((keep-search-hints-p nil)))"
  (prompt :prompt "Search text"
          :sources (make-instance 'search-buffer-source
                                  :case-sensitive-p case-sensitive-p
                                  :actions-on-return
                                  (lambda (search-match)
                                    (unless (keep-search-hints-p (current-buffer))
                                      (remove-search-hints))
                                    search-match))))

(define-command search-buffers (&key case-sensitive-p)
  "Search multiple buffers."
  (let ((buffers (prompt :prompt "Search buffer(s)"
                         :sources (make-instance 'buffer-source ; TODO: Define class?
                                                 :actions-on-return #'identity
                                                 :enable-marks-p t))))
    (prompt
     :prompt "Search text"
     :sources (mapcar (lambda (buffer)
                        (make-instance 'search-buffer-source
                                       :name (format nil "Search ~a" (if (url-empty-p (url buffer))
                                                                         (title buffer)
                                                                         (url buffer)))
                                       :case-sensitive-p case-sensitive-p
                                       :buffer buffer))
                      buffers))))
