;;;; -*- Mode: Lisp; -*- 
;;;; Team Members: <PUT YOUR NAMES HERE>
;;;;
;;;;
;;;; Submission Deadline: Sunday, December 8, 11:59:59pm
;;;; Report Deadline: Monday, December 9, 11:59:59pm
;;;; 
;;;; Please submit your code as a .lisp file in Blackboard.
;;;;
;;;;



;;;HELPFUL TOPLEVEL HINTS:

;;; To run your program, first load it into the LispWorks Editor,
;;; then click on the "Compile Buffer" button on the top of the
;;; Editor window.  Then, in the LISTENER, you can call any of
;;; the functions in the program file.
;;;
;;; If you find yourself in an error, and you want to know how
;;; you got to that point in the program, when the textual debugger
;;; message appears, you can click on the "Debug" button at the
;;; top of the LISTENER window.  You'll get a GUI debugger that
;;; will show you things like the call stack, the values of the
;;; local variables, and, if you click on a call stack entry,
;;; if the entry corresponds to a function in your program, the
;;; Editor will immediately jump you to the line of code where
;;; the error occured.  Ask in class if you have trouble with
;;; this neat feature. 


;;;HELPFUL PROGRAMMING HINTS:
;;; To create a person structure, use the automatically-generated
;;; function "make-person" as follows:
;;;
;;; (make-person :name xxx :parent1 yyy :parent2 zzz)
;;;
;;; where "xxx" is the string or symbol (or a variable holding it)
;;; for the name of the person, "yyy" is the string or symbol for
;;; the name of the person's first parent, and "zzz" is of course
;;; the name of the person's second parent.
;;;
;;; for example, to store a new person in a variable p, use this:
;;;
;;; (SETF p (make-person :name "Barbara" :parent1 "Fred" :parent2 "Carol"))
;;;
;;;

;;; The DEFSTRUCT function tells Lisp to autmatically create
;;; getter functions for each slot.  Their names are based on
;;; the names of the slots:
;;;
;;;  "person-name" will get the value stored in the NAME slot
;;;  "person-parent1" will get the value in the PARENT1 slot
;;;

;;; The LOOP function (macro) is used to iterate in many ways.
;;; Here are some examples:
;;;
;;; (LET ((newlist nil)
;;;       (mylist (LIST 1 2 3 4 5 6 7 8)))
;;;   (LOOP for i in mylist DOING
;;;     (SETF newlist (APPEND newlist (LIST (+ i 1)))))
;;;   newlist)
;;;
;;;  The above will make a new list that contains
;;;  numbers that are one more than their corresponding
;;;  elements in mylist.  Notice that the new sum is added
;;;  at the END of the growing new list!
;;;  This could also be done more elegantly in Lisp using
;;;  a nameless lambda function
;;;
;;;  (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;    (MAPCAR #'(lambda (x) (+ x 1)) mylist))
;;;
;;; MAPCAR applies its first argument (a function) to each
;;; element in the second argument (a list), and collects
;;; the results of all the function calls into a new list
;;; and returns that list.
;;;
;;; Here is another LOOP example that does the same thing:
;;;
;;; (LET ((mylist (LIST 1 2 3 4 5 6 7 8)))
;;;  (LOOP for x in mylist collecting
;;;     (+ x 1)))
;;;


;;;-------------------------------
;;;PROJECT CODE STARTS HERE.
;;;-------------------------------

(DEFSTRUCT (person
            (:print-function print-person))
  (parent1 NIL) ; a symbol or string or NIL
  (parent2 NIL) ; a symbol or string or NIL
  (name NIL))   ; a symbol or string or NIL


;;NOTE: This function is complete, no need to change it unless you
;;want to update it to show other slots you add to the person struct
;;definition.
(DEFUN print-person (item stream depth)
  "A helper function for Lispworks to be able to show you what is
in a person structure concisely."
    (DECLARE (IGNORE depth))
    (FORMAT stream "#<P name:~S p1:~S p2:~S>"
            (person-name item) (person-parent1 item) (person-parent2 item))
    item)


;;;NOTE: This function is complete. No need to change it.
(DEFUN lookup-person (name tree)
  "Returns a PERSON structure corresponding to the key NAME in the hashtable TREE.
NAME must be a STRING or a SYMBOL. If there is no one in the tree with the name
in NAME, returns NIL."
  (GETHASH name tree nil))


;;;NOTE: This function is complete. No need to change it.
(DEFUN person-exists (name tree)
  "Returns T when the key NAME has an actual person struct stored in TREE.
Returns NIL (false) otherwise."
  (WHEN (lookup-person name tree)
    t))


;;; Recursively get all ancestors of the specified person.
;;;
;;; Each ancestor name is mapped to a list of "degrees", which indicate the
;;; number of branches back that the ancestor appears in the query target's
;;; family tree
;;;
;;; It's a list because inbreeding can cause an ancestor to have multiple
;;; degrees of relation to a descendant, e.g. father and great-grandfather.
;;;
;;; Don't call this function directly.
;;; Call the higher-level get_ancestors function instead.
(defun get-ancestor-map (name tree &optional (degree 0))
  (let ((ancestor-map (make-hash-table :test #'equal))
        (p nil)
        (parent1 nil)
        (parent2 nil)
        (parent1-ancestors nil)
        (parent2-ancestors nil))

    (when (not (person-exists name tree))
      (return-from get-ancestor-map nil))

    (setq p (gethash name tree))

    (when (person-parent1 p)
      (setq parent1 (person-parent1 p) parent2 (person-parent2 p))
      (setf (gethash parent1 ancestor-map) (list degree) (gethash parent2 ancestor-map) (list degree))
      (setq parent1-ancestors (get-ancestor-map parent1 tree (+ degree 1)) parent2-ancestors (get-ancestor-map parent2 tree (+ degree 1)))

      ;;; If one parent is an ancestor of the other...
      ;;; Ancestors might have multiple degrees of relation
      (when (gethash parent2 parent1-ancestors)
        ;;; Add the other degrees for this ancestor
        (setf (gethash parent2 ancestor-map) (append (gethash parent2 ancestor-map) (gethash parent2 parent1-ancestors)))
        (remhash parent2 parent1-ancestors))

      ;;; Do the same for the other parent
      (when (gethash parent1 parent2-ancestors)
        (setf (gethash parent1 ancestor-map) (append (gethash parent1 ancestor-map) (gethash parent1 parent2-ancestors)))
        (remhash parent1 parent2-ancestors))

      ;;; Remove duplicate degrees, to be safe
      (setf (gethash parent1 ancestor-map) (remove-duplicates (gethash parent1 ancestor-map)))
      (setf (gethash parent2 ancestor-map) (remove-duplicates (gethash parent2 ancestor-map)))

      ;;; Now add the rest of the ancestors that weren't already present
      (loop for key being the hash-keys of parent1-ancestors
            do (setf (gethash key ancestor-map) (gethash key parent1-ancestors)))
      (loop for key being the hash-keys of parent2-ancestors
            do (setf (gethash key ancestor-map) (gethash key parent2-ancestors))))

    ancestor-map))


;;; Get all ancestors of the specified degree.
;;;
;;; Degree of -1 (default) means get all ancestors.
(defun get-ancestors(name tree &optional (degree -1))
  (let ((ancestor-map (get-ancestor-map name tree)))
    (loop for key being the hash-keys of ancestor-map
          when (or (eql degree -1) (member degree (gethash key ancestor-map)))
            collect key)))


;;; Checks if 'ancestor' is an ancestor of 'name'.
(defun is-ancestor(ancestor name tree)
  (let ((ancestors (get-ancestors name tree)))
    ;;; Returns t if list is not empty; nil otherwise
    (not (not (member ancestor ancestors :test #'string=))))) 


;;; Checks if 'unrelated' is unrelated to 'name'.
(defun is-unrelated(unrelated name tree)
  (let ((ancestors1 (get-ancestors unrelated tree))
        (ancestors2 (get-ancestors name tree)))
    (not (or 
          (member name ancestors1) 
          (member unrelated ancestors2) 
          (string= unrelated name)
          (intersection ancestors1 ancestors2)))))


;;; Gets all family tree members that are unrelated to 'name'.
(defun get-unrelated(name tree)
  (loop for key being the hash-keys of tree
        when (is-unrelated key name tree)
          collect key))


;;; Checks if 'child' is a child of 'name'.
(defun is-child(child name tree)
  (let ((c (lookup-person child tree)))
    (when (not (person-parent1 c))
        (return-from is-child nil))
    (or (string= (person-parent1 c) name) (string= (person-parent2 c) name))))


;;; Gets all children of 'name'.
(defun get-children(name tree)
  (loop for key being the hash-keys of tree
        when (is-child key name tree)
          collect key))
  

;;; Checks if 'sibling' is a sibling of 'name'.
(defun is-sibling(sibling name tree)
  (let ((s (lookup-person sibling tree))
        (p (lookup-person name tree)))
    (and (not (string= sibling name))
         (and (person-parent1 s) (person-parent1 p))
         (intersection (list (person-parent1 s) (person-parent2 s)) 
                       (list (person-parent1 p) (person-parent2 p))))))


;;; Gets all siblings of 'name'.
(defun get-siblings(name tree)
  (loop for key being the hash-keys of tree
        when (is-sibling key name tree)
          collect key))


;;; Adds an entry to hash table 'tree'.
;;;
;;; key: 'name'
;;; value: new person struct representing person identified by 'name'
;;;
;;; If 'parents' is provided as an argument, it must be a two-element list
;;; containing the person's parents, which will be represented in the new
;;; person struct.
(DEFUN add-person (tree name &optional (parents))
  (let ((p nil)
        (parent1 nil)
        (parent2 nil))

    (WHEN (NOT (HASH-TABLE-P tree))
      (ERROR "ADD-PERSON called with TREE (~A) that is not a HASH-TABLE." tree))

    (when (not (or (symbolp name) (stringp name)))
      (error "ADD-PERSON called with NAME (~A) that is not a SYMBOL or a STRING" name))

    (when (person-exists name tree)
      (error "ADD-PERSON: ~A already exists in the tree" name))

    (when parents
      (when (not (listp parents))
        (error "ADD-PERSON called with PARENTS (~A) that is not a LIST" parents))

      (when (not (eq (list-length parents) 2))
        (error "ADD-PERSON called with ~D parents (must be 2)" (list-length parents)))

      (setq parent1 (nth 0 parents) parent2 (nth 1 parents))

      (when (not (or (symbolp parent1) (stringp parent1)))
        (error "ADD-PERSON called with PARENTS[0] (~A) that is not a SYMBOL or a STRING" parent1))

      (when (not (or (symbolp parent2) (stringp parent2)))
        (error "ADD-PERSON called with PARENTS[1] (~A) that is not a SYMBOL or a STRING" parent2))

      (when (not (person-exists parent1 tree))
        (add-person tree parent1))

      (when (not (person-exists parent2 tree))
        (add-person tree parent2)))

    (setq p (make-person :parent1 parent1 :parent2 parent2 :name name))
    (setf (gethash name tree) p)

    name))

  
(DEFUN handle-E (names tree)
  "NAMES is a LIST of strings. TREE is a hash-table."
  (LET ()
    (cond ((eql (list-length names) 2)
           (add-person tree (nth 0 names))
           (add-person tree (nth 1 names)))
          ((eql (list-length names) 3)
           (add-person tree (nth 2 names) (list (nth 0 names) (nth 1 names))))
          (t (error "HANDLE-E must be called with two or three parameters")))
  ))


(DEFUN handle-X (tokens tree)
  "TOKENS is a LIST of strings. TREE is a hash-table."
  (LET ((relation (nth 1 tokens))
        (name1 (nth 0 tokens))
        (name2 nil)
        (degree -1)
        (result nil))

    (cond ((eql (list-length tokens) 3) (setq name2 (nth 2 tokens)))
          ((eql (list-length tokens) 4) (setq name2 (nth 3 tokens) degree (parse-integer (nth 2 tokens))))
          (t (error "HANDLE-X must be called with three or four parameters")))

    (when (not (person-exists name1 tree))
      (error "HANDLE-X: ~A does not exist in the tree" name1))

    (when (not (person-exists name2 tree))
      (error "HANDLE-X: ~A does not exist in the tree" name2))

    ;;; case... strings...
    (cond ((string= relation "ancestor") (setq result (is-ancestor name1 name2 tree)))
          ((string= relation "unrelated") (setq result (is-unrelated name1 name2 tree)))
          ((string= relation "child") (setq result (is-child name1 name2 tree)))
          ((string= relation "sibling") (setq result (is-sibling name1 name2 tree)))
          ((string= relation "cousin") (setq result (is-cousin name1 name2 tree degree)))
          (t (error "HANDLE-X: Invalid relation: ~A~%" relation)))

    result
    ))


(DEFUN handle-W (tokens tree)
  "TOKENS is a LIST of strings. TREE is a hash-table."
  (LET ((relation (nth 0 tokens))
        (name nil)
        (degree -1)
        (result nil))

    (cond ((eql (list-length tokens) 2) (setq name (nth 1 tokens)))
          ((eql (list-length tokens) 3) (setq name (nth 2 tokens) degree (parse-integer (nth 1 tokens))))
          (t (error "HANDLE-W must be called with two or three parameters")))

    (when (not (person-exists name tree))
      (error "HANDLE-W: ~A does not exist in the tree" name))

    ;;; Again, case doesn't work on strings...
    (cond ((string= relation "ancestor") (setq result (get-ancestors name tree)))
          ((string= relation "unrelated") (setq result (get-unrelated name tree)))
          ((string= relation "child") (setq result (get-children name tree)))
          ((string= relation "sibling") (setq result (get-siblings name tree)))
          ((string= relation "cousin") (setq result (get-cousins name tree degree)))
          (t (error "HANDLE-W: Invalid relation: ~A~%" relation)))

    (when result
        (setq result (sort result #'string-lessp)))

    result
    ))


;;; Gets all cousins of specified degree (-1 for all cousins).
(defun get-cousins(name tree &optional (degree -1))
  (let ((all-cousins nil)
        (common-ancestors nil)
        (p-ancestor-map (get-ancestor-map name tree))
        (c-ancestor-map nil)
        (common-ancestor-map nil)
        (ancestor-degrees nil))

    (when (< degree -1)
      (error "GET-COUSINS: DEGREE must be >= -1"))

    (loop for key being the hash-keys of tree
          when (and (not (string= key name))
                    (not (is-unrelated key name tree))
                    (not (is-ancestor key name tree))
                    (not (is-ancestor name key tree)))
            collect key into l
          finally (setq all-cousins l))

    (when (eql degree -1)
        (return-from get-cousins all-cousins))

    ;;; Replace with the minimum degree for each ancestor
    (loop for key being the hash-keys of p-ancestor-map
          do (setf (gethash key p-ancestor-map) (apply #'min (gethash key p-ancestor-map))))

    (loop for cousin in all-cousins
          ;;; Same common ancestor might have different degrees for each cousin
          do (setq common-ancestors (intersection (get-ancestors name tree) (get-ancestors cousin tree) :test 'string=))

          (setq c-ancestor-map (get-ancestor-map cousin tree))

          (loop for key being the hash-keys of c-ancestor-map
                do (setf (gethash key c-ancestor-map) (apply #'min (gethash key c-ancestor-map))))

          ;;; If ancestor has different degree for each cousin, replace with
          ;;; minimum degree between the two
          (setq common-ancestor-map (make-hash-table :test #'equal))
          (loop for ancestor in common-ancestors
                do (setf (gethash ancestor common-ancestor-map) (min (gethash ancestor p-ancestor-map) (gethash ancestor c-ancestor-map))))

          ;;; The minimum of all common ancestor degrees is the cousin degree
          (loop for key being the hash-keys of common-ancestor-map
                collect (gethash key common-ancestor-map) into l
                finally (setq ancestor-degrees l))

          ;;; Degree of cousin is minimum degree across all common ancestors
          when (eql (apply #'min ancestor-degrees) degree)
            collect cousin)))


;;; Checks if 'cousin' is a cousin of 'name'.
;;;
;;; If degree is provided as an argument, checks for a matching degree of
;;; cousin as well.
(defun is-cousin(cousin name tree &optional (degree -1))
  (not (not (member cousin (get-cousins name tree degree)))))


;;;THE TOP LEVEL FUNCTION OF THE WHOLE PROGRAM
(DEFUN family (stream)
  "This is the top-level function for the whole Lisp program. Reads
each line from the file opened in STREAM."
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal))
        (line nil)
        (tokens nil)
        (result nil))

    (loop for line = (read-line stream nil)
          while line
          do (setq tokens (split-sequence " " line :test #'equal))

          ;;; Can't use CASE macro on strings apparently (probably uses eql)
          ;;; so have to use a conditional instead
          (cond ((string= (first tokens) "E")
                 (handle-E (rest tokens) tree))
                ((string= (first tokens) "W")
                 (format t "~A~%" line)
                 (setq result (handle-W (rest tokens) tree))
                 (if result
                     (progn
                       (loop for name in result
                           do (format t "~A~%" name))
                       (terpri))
                   (format t "None~%~%")))
                ((string= (first tokens) "X")
                 (format t "~A~%" line)
                 (setq result (handle-X (rest tokens) tree))
                 (if result
                     (format t "Yes~%~%")
                   (format t "No~%~%")))
                (t (error "FAMILY: Invalid query type: ~A~%" (first tokens)))))

    (format t "End of File.~%")
  )
)


;;How Dr. Klassner and Jenish will test your code in the Listener:
;;
;;(family (open "~/Documents/School/CSC\ 1800-002/Projects/Project3/tests/test.txt"))
;;
;; NOTE: The FilePath for OPEN is just an example. 
;; Use your own laptop directory to where you keep
;; your project's test files.


;;;A helpful tester function for debugging your tree.
(DEFUN test-tree ()
  (LET ((tree (MAKE-HASH-TABLE :size 1000 :test #'equal)))
    (add-person tree "Zebulon")
    (add-person tree "Zenobia")
    (add-person tree "Fred")
    (add-person tree "Mary" (list "Zebulon" "Zenobia"))
    (add-person tree "Karen" (list "Fred" "Mary"))
    (add-person tree "Kelly" (list "Fred" "Mary"))
    (add-person tree "Brenda" (list "Fred" "Mary"))
    (add-person tree "Bill")
    (add-person tree "Benjamin" (list "Karen" "Bill"))
    (add-person tree "Alex" (list "Karen" "Bill"))
    (add-person tree "Thomas" (list "Benjamin" "Laura"))
    (add-person tree "Jeffrey" (list "Alex" "Stacy"))

    ;; this last call should make test-tree return a list containing the following
    ;; in some arbitrary order when you call test-tree in the Listener:
    ;;   ("Karen" "Bill" "Fred" "Mary" "Zebulon" "Zenobia")

    (format t "Ancestors of Alex: ~A~%" (sort (get-ancestors "Alex" tree) #'string-lessp))
    (terpri)

    (if (is-ancestor "Kelly" "Alex" tree)
        (format t "Kelly is an ancestor of Alex.~%")
      (format t "Kelly is not an ancestor of Alex.~%"))

    (if (is-ancestor "Karen" "Alex" tree)
        (format t "Karen is an ancestor of Alex.~%")
      (format t "Karen is not an ancestor of Alex.~%"))
    (terpri)
    
    (format t "Cousins of Benjamin: ~A~%" (get-cousins "Benjamin" tree))
    (format t "0-cousins of Benjamin: ~A~%" (get-cousins "Benjamin" tree 0))
    (format t "1-cousins of Benjamin: ~A~%" (get-cousins "Benjamin" tree 1))
    (format t "2-cousins of Benjamin: ~A~%" (get-cousins "Benjamin" tree 2))
    (terpri)

    (format t "Cousins of Alex: ~A~%" (get-cousins "Alex" tree))
    (format t "0-cousins of Alex: ~A~%" (get-cousins "Alex" tree 0))
    (format t "1-cousins of Alex: ~A~%" (get-cousins "Alex" tree 1))
    (format t "2-cousins of Alex: ~A~%" (get-cousins "Alex" tree 2))
    (terpri)
    
    (format t "Cousins of Jeffrey: ~A~%" (get-cousins "Jeffrey" tree))
    (format t "0-cousins of Jeffrey: ~A~%" (get-cousins "Jeffrey" tree 0))
    (format t "1-cousins of Jeffrey: ~A~%" (get-cousins "Jeffrey" tree 1))
    (format t "2-cousins of Jeffrey: ~A~%" (get-cousins "Jeffrey" tree 2))
    (terpri)

    (format t "Cousins of Thomas: ~A~%" (get-cousins "Thomas" tree))
    (format t "0-cousins of Thomas: ~A~%" (get-cousins "Thomas" tree 0))
    (format t "1-cousins of Thomas: ~A~%" (get-cousins "Thomas" tree 1))
    (format t "2-cousins of Thomas: ~A~%" (get-cousins "Thomas" tree 2))
    (terpri)))


    
