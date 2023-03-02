;;; spel.el --- A simple text-based adventure game.

;; Author: Orestis Ousoultzoglou <orousoultzoglou@gmail.com>
;; Keywords: emacs lisp tutorial games

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTNABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Disclaimer:
;; This is just an extension of, and therefore
;; heavily based on, wizard-adventure-emacs.el

;; Copyright (C) 2007 Conrad Barski

;; Author: Conrad Barski <drcode@gmail.com>
;; Editor: James A. Webb <uberkoder@gmail.com>
;; Created: 16 September 2007
;; Version: 1.0

;;; Commentary:
;; This is a simple text adventure game meant
;; as nothing more than a fun means to practice
;; one's elisp-fu.

;;; Code:

(require 'cl)

(setq eval-expression-print-length nil)

(setq help '((look
                  (you can take a gander at where you are -
                       determine where you can head towards -
                       see if there are any objects on the floor))
                 (walk
                  (you can walk towards a direction -
                       the world is your oyster))
                 (pickup
                  (you can pick an object from the floor -
                       to carry it with you wherever you go))
                 (inventory
                  (you can determine what you are carrying -
                       it would be silly if you could not))
                 (dunk
                  (you can dunk something in something else -
                       no this is not basketball time))
                 (weld
                  (you can weld something and something else together -
                       there is an engineer hidden in every one of us))
                 (splash
                  (you can splash something on something -
                       does not have to be summer season))))

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

(setq location 'living-room)
(setq objects '(whiskey-bottle bucket frog chain))
(setq object-locations '((whiskey-bottle living-room)
                         (bucket living-room)
                         (chain garden)
                         (frog garden)))

(setq chain-welded nil)
(setq bucket-filled nil)

(defun help ()
  `(these are your spells: ,(mapcar #'car help)
    you can ask me to (explain spell)))

(defun describe-spell (spell help)
  (cadr (assoc spell help)))

(defun describe-location (location map)
  (cadr (assoc location map)))

(defun describe-path (path)
  `(there is a ,(cadr path) going ,(car path) from here -))

(defun describe-paths (location map)
  (apply #'append
         (mapcar #'describe-path
                 (cddr (assoc location map)))))

(defun is-at (obj loc obj-loc)
  (eq (cadr (assoc obj obj-loc)) loc))

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

; SPEL: Semantic Program Enhancement Logic
; Yes, it is just a macro to have defmacro be named defspel
; I like casting spells. See this for more:
; https://lisperati.com/casting-spells-emacs/html/
; <continued> /casting-spells-emacs-33.html
(defmacro defspel (&rest rest) `(defmacro ,@rest))

(defspel explain (spell)
  `(describe-spell ',spell help))

; walk is walk-direction, and direction, e.g. west is 'west
; so (walk west) becomes (walk-direction 'west)
(defspel walk (direction)
  `(walk-direction ',direction))

(defun pickup-object (object)
  (cond ((is-at object location object-locations)
         (push (list object 'body) object-locations)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defspel pickup (object)
  `(pickup-object ',object))

(defun inventory ()
  (remove-if-not (lambda (x)
                   (is-at x 'body object-locations))
                 objects))

(defun have (object)
  (member object (inventory)))

; A SPEL can cast another SPEL.
(defspel game-action (command subj obj place &rest rest)
  `(defspel ,command (subject object)
     `(cond ((and (eq location ',',place)
                  (eq ',subject ',',subj)
                  (eq ',object ',',obj)
                  (have ',',subj))
             ,@',rest)
            (t '(i cannot ,',command like that -)))))

(game-action weld chain bucket attic
             (cond ((and (have 'bucket) (setq chain-welded 't))
                    '(the chain is now securely welded to the bucket -))
                   (t '(you do not have a bucket -))))

(game-action dunk bucket well garden
             (cond (chain-welded (setq bucket-filled 't)
                                 '(the bucket is now full of water))
                   (t '(the water level is too low to reach -))))

(game-action splash bucket wizard living-room
             (cond ((not bucket-filled)
                    '(the bucket has nothing in it -))
                   ((have 'frog)
                    '(the wizard awakens and sees that you stole
                        his frog -
                        he is so upset he banishes you to the
                        netherworlds - you lose! the end -))
                   (t
                    '(the wizard awakens from his slumber and greets you warmly -
                        he hands you the magic low-carb donut - you win! the end -))))

;;; spel.el ends here
