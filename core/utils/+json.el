;;; core/utils/+json.el -*- lexical-binding: t; -*-

(defun +json/read-file (file)
  "Parse a JSON file into a hash table."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file file)))
    json))
