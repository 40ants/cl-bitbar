(defpackage cl-bitbar.utils
  (:use :cl)
  (:export :push-right
           :escape-tilda))
(in-package cl-bitbar.utils)


(defmacro push-right (place item)
  "Добавляет item в конец списка place.
   При этом модифицируется список."
  `(setf ,place (append ,place (list ,item))))


(defun escape-tilda (text)
  (cl-strings:replace-all text "~" "~~"))
