(in-package :core)

(defun prompt-eval (message &key (stream *query-io*))
  "Ask the user for a value by printing MESSAGE to STREAM, reading an
expression, evaluating it, and returning the list of values yielded by the
evaluation."
  (format stream message)
  (finish-output stream)
  (multiple-value-list (eval (read stream))))
