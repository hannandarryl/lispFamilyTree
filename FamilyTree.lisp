;;;; -*- Mode:LISP; -*-
;;;; Family Tree program

(defstruct person
	name
	spouse
	parents
	children
)
( setf FamilyTree (make-hash-table :test 'equal))

;;; Get the spouses of the person
(defun getSpouses(p)
  (loop for spouse in (person-spouse (gethash p FamilyTree))
        collect spouse))

;;; Get the parents of the person
(defun getParents(p)
	(loop for parent in (person-parents (gethash p FamilyTree))
		collect parent))

;;; Get the siblings of the person
(defun getSiblings(p)
  (if (null (person-parents (gethash p FamilyTree)))
      '()
    (remove p (intersection 
               (loop for child in (person-children (gethash (first (person-parents (gethash p FamilyTree))) FamilyTree))
                     collect child)
               (loop for child in (person-children (gethash (second (person-parents (gethash p FamilyTree))) FamilyTree))
                     collect child)))))

;;; Get the half siblings of the person
(defun getHalfSiblings(p)
  (if (null (person-parents (gethash p FamilyTree)))
      '()
    (progn (setq temp (union (loop for child in (person-children (gethash (first (person-parents (gethash p FamilyTree))) FamilyTree))
                                 collect child)
                           (loop for child in (person-children (gethash (second (person-parents (gethash p FamilyTree))) FamilyTree))
                      collect child)))
    (remove p (set-difference temp (getSiblings p))))))

;;; Get cousins
(defun getCousins (p deg rem)
  (loop for person in (remove (second (person-parents (gethash p FamilyTree))) (remove (first (person-parents (gethash p FamilyTree))) (remove p (getKeys))))
        when (progn(setf theLevel (getCousinLevel (gethash p FamilyTree) (gethash person FamilyTree)))
                 (and (equal (first theLevel) deg) (equal (second theLevel) rem)))
                     collect person))

;;; Get ancestors
(defun getAncestors (human)
	(if (not (person-parents human))
	nil
	(union (union (getAncestors(gethash (first (person-parents human)) FamilyTree)) (getAncestors(gethash (second (person-parents human)) FamilyTree))) (person-parents human))))

;;; Get unrelated
(defun getUnrelated(p)
  (set-difference (loop for person in (remove p (getKeys))
        when (null (intersection (getAncestors (gethash person FamilyTree)) (getAncestors (gethash p FamilyTree))))
        collect person) (person-children (gethash p FamilyTree))))
  
(defun getKeys()
  (loop for key being the hash-keys of FamilyTree collect key))

;;; returns list of lists where sub list: name, depth
(defun getAncestorsDeep (p n)
  (if (null(person-parents p))
      nil
    (union (union(getAncestorsDeep(gethash (first(person-parents p)) FamilyTree) (+ n 1))
                 (getAncestorsDeep(gethash (second(person-parents p)) FamilyTree) (+ n 1)))	
           (list (list(first(person-parents p))(+ 1 n)
                      (second(person-parents p)) (+ n 1))))))


(defun getCousinLevel(human1 human2)
  (setf common (intersection (getAncestors human1) (getAncestors human2)))
  (if (null common)
      '()
  (progn (setf anc1 (getAncestorsDeep human1 0))
  (setf anc2 (getAncestorsDeep human2 0))
  (setf firstDeg nil)
  (loop for closest in common
                       do (loop for sub in anc1
                               do (if (equal (first sub) closest)
                                       (setf firstDeg (second sub)))))
  (setf secondDeg nil)
  (loop for closest in common
                        do (loop for sub in anc2
                                 do (if (equal (first sub) closest)
                                         (setf secondDeg (second sub)))))
  (list (- (min (min firstDeg) (min secondDeg)) 1) (- (max (min firstDeg) (min secondDeg)) (min (min firstDeg) (min secondDeg)))))))

;;; Handle the E query
(defun E(p1 p2 &optional c)
	;;; See if the first parent exists and add them if they do not
	(if (not (gethash p1 FamilyTree))
		(setf firstperson (make-person :name p1 :spouse '() :children '()))
		(setf firstperson (gethash p1 FamilyTree)))
	;;; See if the second parent exists and add them if they do not
	(if (not (gethash p2 FamilyTree))
		(setf secondperson (make-person :name p2 :spouse '() :children '()))
		(setf secondperson (gethash p2 FamilyTree)))
	;;; Add the spouses
	(if (not(find p2 (person-spouse firstperson)))
		(setf (person-spouse firstperson) (append (person-spouse firstperson) (list p2))))
	(if (not(find p1 (person-spouse secondperson)))
		(setf (person-spouse secondperson) (append (person-spouse secondperson) (list p1))))
	;;; Add the child if they exist(
	(if (and (not(null c)) (not(gethash c FamilyTree)))
		(progn (setf child (make-person :name c :parents (list p1 p2)))
			(setf (person-children firstperson) (append (person-children firstperson) (list c)))
			(setf (person-children secondperson) (append (person-children secondperson) (list c))))
          (if (gethash c FamilyTree)
              (setf child (gethash c FamilyTree))))
	;;; Add all the people to the hashtable
	(if (not (gethash p1 FamilyTree))
		(setf (gethash p1 FamilyTree) firstperson))
	(if (not (gethash p2 FamilyTree))
		(setf (gethash p2 FamilyTree) secondperson))
	(if (and (not(null c)) (not (gethash c FamilyTree)))
		(setf (gethash c FamilyTree) child)))

;;; Handle the W query
(defun W(r p &optional deadVar)
  (if (null (gethash p FamilyTree))
      (print '())
    (if (not (typep r 'cons))
        (progn
          ;;; Handle spouse query
          (if (string= r "SPOUSE")
              (loop for theperson in (sort (getSpouses p) #'string-lessp)
                    do (print theperson)))
          ;;; Handle parent query
          (if (string= r "PARENT")
              (loop for theperson in (sort (getParents p) #'string-lessp)
                    do (print theperson)))
          ;;; Handle sibling query
          (if (string= r "SIBLING")
              (loop for theperson in (sort (getSiblings p) #'string-lessp)
                    do (print theperson)))
          ;;; Handle half-sibling query
          (if (string= r "HALF-SIBLING")
              (loop for theperson in (sort (getHalfSiblings p) #'string-lessp)
                    do (print theperson)))
          ;;; Handle the ancestor query
          (if (string= r "ANCESTOR")
              (loop for theperson in (sort (getAncestors p) #'string-lessp)
                    do (print theperson)))
          (if (string= r "UNRELATED")
              (loop for theperson in (sort (getUnrelated p) #'string-lessp)
                    do (print theperson))))
      (if (string= (first r) "COUSIN")
          (loop for theperson in (sort (getCousins p (second r) (third r)) #'string-lessp)
                do (print theperson))))))

;;; Handle X query
(defun X(p1 r p2)
  (if (or (null (gethash p1 FamilyTree)) (null (gethash p2 FamilyTree)))
      (print "NO")
    (if (not (typep r 'cons))
        (progn
          ;;; Handle spouse query
          (if (string= r "SPOUSE")
              (if (member p1 (getSpouses p2))
                  (print "YES")
                (print "NO")))
          ;;; Handle parent query
          (if (string= r "PARENT")
              (if (member p1 (getParents p2))
                  (print "YES")
                (print "NO")))
          ;;; Handle sibling query
          (if (string= r "SIBLING")
              (if (member p1 (getSiblings p2))
                  (print "YES")
                (print "NO")))
          ;;; Handle half-sibling query
          (if (string= r "HALF-SIBLING")
              (if (member p1 (getHalfSiblings p2))
                  (print "YES")
                (print "NO")))
          ;;; Handle the ancestor query
          (if (string= r "ANCESTOR")
              (if (member p1 (getAncestors p2))
                  (print "YES")
                (print "NO")))
          (if (string= r "UNRELATED")
              (if (member p1 (getUnrelated p2))
                  (print "YES")
                (print "NO"))))
      (if (string= (first r) "COUSIN")
          (if (member p1 (getCousins p2 (second r) (third r)))
              (print "YES")
            (print "NO"))))))

;;; Handle R query
(defun R(p1 p2 &optional deadVar)
  (if (or (null (gethash p1 FamilyTree)) (null (gethash p2 FamilyTree)))
      (print "UNRELATED")
    ;;; Handle spouse query
    (if (member p1 (getSpouses p2))
        (print "SPOUSE")
      ;;; Handle parent query
      (if (member p1 (getParents p2))
          (print "PARENT")
        ;;; Handle sibling query
        (if (member p1 (getSiblings p2))
            (print "SIBLING")
          ;;; Handle half-sibling query
          (if (member p1 (getHalfSiblings p2))
              (print "HALF-SIBLING")
            ;;; Handle the ancestor query
            (if (member p1 (getAncestors (gethash p2 FamilyTree)))
                (print "ANCESTOR")
              (if (not (null (getCousinLevel (gethash p1 FamilyTree) (gethash p2 FamilyTree))))
                  (progn (print "(COUSIN ") (prin1 (first (getCousinLevel (gethash p1 FamilyTree) (gethash p2 FamilyTree)))) (prin1 " ") (prin1 (second (getCousinLevel (gethash p1 FamilyTree) (gethash p2 FamilyTree)))) (prin1  ")"))
                (print "UNRELATED")))))))))
	
;;; Handle the various queries
;;; Cannot figure out how to call line from file as a function
;;; This should happen below
(defun processFile()
	(loop for line = (read *standard-input* nil nil)
		while line
		do (funcall (first line) (second line) (third line) (fourth line))))

;;; Run the process
(processFile)

;;; Count of how many are in hashtable to see if they were really added
(print "The count of people in the hash table is ")
(print (hash-table-count FamilyTree))
