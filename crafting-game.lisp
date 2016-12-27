;;;;
;;;; Title: Crafting Game
;;;; Author: Scott Madera
;;;; Created: 12/25/16
;;;; Last edited: 12/27/16
;;;; TODO: Add more items to craft
(in-package :cl-user)

(defpackage #:crafting-game
  (:use :cl :cl-user)
  (:export #:main))

(in-package :crafting-game)

(defvar *input* nil)
(defvar *inventory* (make-hash-table :test 'equalp))

(defun can-craft-p (item)
  ;; TODO: fill out requirements for each item
  (string-case item
               ("axe" (and (<= 2 (gethash "stone" *inventory*))
                           (<= 3 (gethash "wood" *inventory*))))
               ("pickaxe" (and (<= 3 (gethash "stone" *inventory*))
                               (<= 3 (gethash "wood" *inventory*))))
               ("stone" (<= 1 (gethash "pickaxe" *inventory*)))
               ("wood" (<= 1 (gethash "axe" *inventory*)))
               ("charcoal" (<= 1 (gethash "pickaxe" *inventory*)))
               ("iron" (and (<= 1 (gethash "stone" *inventory*))
                            (<= 1 (gethash "charcoal" *inventory*))))))

;;; 1 axe - 3 wood, 2 stone
;;; 1 pickaxe - 3 wood, 3 stone
;;; 13 stone - 1 pickaxe
;;; 13 wood - 1 axe
;;; 13 charcoal - 1 pickaxe
;;; 1 iron - 1 stone, 1 charcoal
;;;
(defun craft (item)
  (if (can-craft-p item)
      (if (or (string= item "stone") (string= item "wood")
              (string= item "charcoal"))
          (dotimes (x 13)
            (inchv item))
          (inchv item)))
  (string-case item
               ("axe" (dotimes (x 3)
                        (dechv "wood"))
                      (dotimes (x 2)
                        (dechv "stone")))
               ("pickaxe" (dotimes (x 3)
                            (dechv "wood")
                            (dechv "stone")))
               ("stone" (dechv "pickaxe"))
               ("wood" (dechv "axe"))
               ("charcoal" (dechv "pickaxe"))
               ("iron" (dechv "charcoal")
                       (dechv "stone"))))

(defun crafting-table ()
  (loop while (not (string= *input* "q"))
     do (format t "What craft? ")
       (setq *input* (read-line *query-io*))
       (craft *input*))
  (setq *input* nil))

;; Extra hash iterator function... for fun
(defun hash-table-iterator (hash-table)
  (with-hash-table-iterator (my-iterator hash-table)
    (loop
       (multiple-value-bind (entry-p key value)
           (my-iterator)
         (if entry-p
             (format t "~a: ~a~%" key value)
             (return))))))

;; increment hash value
(defmacro inchv (item)
  `(setf (gethash ,item *inventory*) (1+ (or (gethash ,item *inventory*) 0))))

;; decrement hash value
(defmacro dechv (item)
  `(setf (gethash ,item *inventory*) (if (>= (gethash ,item *inventory*) 1)
                                         (1- (gethash ,item *inventory*))
                                         0)))
;; starting game inventory
(defun initialize-inventory ()
  (setf (gethash "wood" *inventory*) 10)
  (setf (gethash "charcoal" *inventory*) 10)
  (setf (gethash "stone" *inventory*) 10))

;; Another way to iterate through a hash table
(defun print-hash-entry (key value)
  (format t "~a: ~a~%" key value))

(defun show-inventory ()
  (maphash #'print-hash-entry *inventory*))

(defmacro string-case (str &rest forms)
  (let* ((strval (gensym "STRVAL"))
         (cond-body (loop for (s . f) in forms
                       collect `((string= ,strval ,s) ,@f))))
    `(let ((,strval ,str)) (cond ,@cond-body))))

(defun main ()
  (initialize-inventory)
  (loop while (not (string= *input* "q"))
     do (format t "Enter a value: ")
       (setq *input* (read-line *query-io*))
       (format t "The value is ~a~%" *input*)
       (string-case *input*
                    ("c" (format t "Crafting:~%")
                         (crafting-table))
                    ("i" (format t "Inventory:~%")
                         (hash-table-iterator *inventory*))
                    ("m" (format t "Map:~%"))))
  (setq *input* nil) t)
