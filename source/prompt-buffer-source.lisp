;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-class prompt-buffer-source (prompter:source)
  ;; Override prompter:actions-* slots.  In Nyxt, we want actions to be function
  ;; symbols instead of functions, since the later are opaque.
  ;; Concretely, it's important when issuing command set-action-on-return.  See
  ;; `nyxt/prompt-buffer-mode::make-action-suggestion'.
  ((prompter:actions-on-return
    'identity
    :type (or null
              function-symbol
              (cons function-symbol *))
    :accessor nil
    :export nil
    ;; TODO docs
    :documentation "List of function symbols that can be run on `suggestion's of
this source.  This is the low-level implementation, see the
`prompter:actions-on-return' function for the public interface.
For convenience, it may be initialized with a single function or symbol, in
which case it will be automatically turned into a list.")
   (actions-ono-current-suggestion
    'identity
    :type (or null function-symbol
              (cons (or function-symbol) *))
    :accessor nil
    :export nil
    ;; TODO
    :documentation "The first function symbol of this list is called
automatically on the current-suggestion when it's changed.
It does not interrupt or return the prompter.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list.")
   (actions-on-marks
    'identity
    :type (or null function-symbol
              (cons (or function-symbol) *))
    :accessor nil
    :export nil
    ;; TODO
    :documentation "The first function symbol of this list is called
automatically when the marks change.
It does not interrupt or return the `prompt-buffer'.
For convenience, it may be initialized with a single function, in which case it
will be automatically turned into a list."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (class*:make-name-transformer name))
  (:documentation "A prompt buffer source is meant to be used by `prompt-buffer'
objects.

For a detailed description see `prompter:source', the parent class of
`prompt-buffer-source'.")
  (:metaclass user-class))
