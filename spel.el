;;; spel.el -*- lexical-binding: t; -*-
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'cl)

(setq objects '(whiskey-bottle bucket frog chain))

(setq map '((living-room
             (you are in the living room of a wizards house -
                  there is a wizard snoring loudly on the couch -)
             (west door garden)
             (upstairs stairway attic))
            (garden
             (you are in a beautiful garden -
                  there is a well in front of you -)
             (east door living-room))
            (attic
             (you are in the attic of the wizards house -
                  there is a giant welding torch in the corner -)
             (downstairs stairway living-room))))

(setq object-locations '((whiskey-bottle living-room)
                         (bucket living-room)
                         (chain garden)
                         (frog garden)))

(setq location 'living-room)

(defun describe-location (location map)
  (second (assoc location map)))

(defun describe-path (path)
  `(there is a ,(second path) going ,(first path) from here -))

(defun describe-paths (location map)
  (apply #'append
         (mapcar #'describe-path
                 (cddr (assoc location map)))))

(defun is-at (obj loc obj-loc)
  (eq (second (assoc obj obj-loc)) loc))

(defun describe-floor (loc objs obj-loc)
  (apply #'append (mapcar (lambda (x)
                            `(you see a ,x on the floor -))
                          (remove-if-not (lambda (x)
                                           (is-at x loc obj-loc))
                                         objs))))

(defun look ()
  (append (describe-location location map)
          (describe-paths location map)
          (describe-floor location objects object-locations)))

(defun walk-direction (direction)
  (let ((next (assoc direction (cddr (assoc location map)))))
    (cond (next (setf location (third next)) (look))
          (t '(you cannot go that way -)))))

(defun pickup-object (object)
  (cond ((is-at object location object-locations)
         (push (list object 'body) object-locations)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (remove-if-not (lambda (x)
                   (is-at x 'body object-locations))
                 objects))

(defun have-object (object)
  (member object (inventory)))

; SPEL: Semantic Program Enhancement Logic
; Yes, it is just a macro to have defmacro be named defspel
; I like casting spells. See this for more:
; https://lisperati.com/casting-spells-emacs/html/
; <continued> /casting-spells-emacs-33.html
(defmacro defspel (&rest rest) `(defmacro ,@rest))

; walk is walk-direction, and direction, e.g. west is 'west
; so (walk west) becomes (walk-direction 'west)
(defspel walk (direction)
  `(walk-direction ',direction))

(defspel pickup (object)
  `(pickup-object ',object))

(defspel have (object)
  `(have-object ',object))

(provide 'spel)
;;; spel.el ends here
