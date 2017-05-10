(defpackage cl-bitbar
  (:use :cl
        :cl-bitbar.utils
        :iterate)
  (:export :*version*
           :menu-header
           :menu-item
           :menu-items
           :ensure-item
           :submenu-item
           :menu-spacer))
(in-package cl-bitbar)

(cl-interpol:enable-interpol-syntax)


(defvar *version* "0.1.0")


(defun menu-item (text &key href
                         command
                         terminal
                         color
                         ansi
                         refresh
                         prefix)
  (let ((pieces
         ;; Start text with newline and prefix if it was given
         (list "~&"
               (or prefix "")
               text)))
    
    (when (or command href refresh color)
      (push-right pieces " |"))

    (when href
      (push-right pieces #?" href=${href}"))
    
    (when color
      (push-right pieces #?" color=${color}"))

    (when refresh
      (push-right pieces " refresh=true"))
    
    (when ansi
      (push-right pieces " ansi=true"))

    (when command
      (push-right pieces " bash=")
      (push-right pieces
                  (escape-tilda
                   (etypecase command
                     (string command)
                     (cons (first command)))))
      (when (listp command)
        (iterate (for param :in (rest command))
                 (for param-number :upfrom 1)
                 (push-right pieces
                             #?" param${param-number}=${param}"))))

    (when command
      (push-right pieces
                  (if terminal
                      " terminal=true"
                      " terminal=false")))
    
    ;; Finish item with newline
    (push-right pieces "~%")
    
    (let ((full-message (apply #'concatenate 'string pieces)))
      (format t full-message))))

(defun submenu-item (text &rest rest)
  "Выводит подменю возможно с shell командой.

   Команда должна быть просто строкой или списком строк, если
   у команды есть параметры."
  (apply #'menu-item
         text
         :prefix "--"
         rest))


(defun menu-spacer ()
  "Выводит разделитель меню."
  (format t "~&---~%"))


(defun menu-header (icon &key color)
  "Draws a first line of output for BitBar.

  Argument icon should be a string."
  (menu-item icon :color color)
  (menu-spacer))


(eval-when (:compile-toplevel :load-toplevel)
 (defun ensure-item (item)
   "Returns item in a form:
   (list a-text :some-param t :other-param \"have value\".

  If given argument is a string, just a (list item) returned.
  If it is a list, then it's structure checked and error
  raised if there are some problems.
  For other types, function raises error."

   (etypecase item
     (string (list item))
     (list (etypecase (first item)
             (string
              (if (or (null (rest item))
                      (rutils:plistp (rest item)))
                  item
                  (error "Rest of the item is not a plist.")))
             (t (list item))))))


 (defun merge-defaults (item defaults)
   "Merges values from 'defaults' plist into a menu item.

   Here item is a list where first element is a string and
   rest is a plist.

   For example:

   CL-USER> (merge-defaults '(\"command\" :a \"blah\")
                            '(:a t :b \"value\"))
   (\"command\" :A \"blah\" :B \"value\")
   "
   (let ((result (reverse item)))
     (iterate (for (key value) :on defaults :by #'cddr)
              (unless (getf (cdr result) key)
                (push key result)
                (push value result)))
     (nreverse result))))


(defmacro menu-items (&rest items)
  "Transforms items into calls to (menu-item).

   Here are few examples.

   In simplest case, items are usual strings:

   CL-USER> (menu-items \"Foo\"
                        \"Bar\")
   (PROGN (MENU-ITEM \"Foo\")
          (MENU-ITEM \"Bar\"))

   But some of them could be a lists with same parameters
   as menu-item function accepts:

   CL-USER> (menu-items (\"Foo\" :color \"black\")
                        \"Bar\")
   (PROGN (MENU-ITEM \"Foo\" :COLOR \"black\")
          (MENU-ITEM \"Bar\"))

   And if you need to give some attributes to all items,
   then you can specify them as a list before the strings:

   CL-USER> (menu-items (:color \"black\")
                        \"Foo\"
                        \"Bar\")
   (PROGN (MENU-ITEM \"Foo\" :COLOR \"black\")
          (MENU-ITEM \"Bar\" :COLOR \"black\"))

"
  (let* ((defaults (if (and (listp (car items))
                            (keywordp (caar items)))
                       (car items)
                       nil))
         (items (if defaults
                    (cdr items)
                    items))
         (items (iterate (for line :in items)
                         (collect `(menu-item ,@(merge-defaults (cl-bitbar:ensure-item line)
                                                                defaults))))))
    `(progn ,@items)))
