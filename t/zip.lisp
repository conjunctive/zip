(defpackage #:zip-test
  (:use #:cl
        #:prove)
  (:import-from #:fset
                #:seq
                #:empty-seq)
  (:import-from #:zip
                #:->zipper
                #:branchp
                #:seq-zip
                #:node
                #:root
                #:children
                #:make-node
                #:up
                #:down
                #:left
                #:right
                #:leftmost
                #:rightmost
                #:lefts
                #:rights
                #:insert-left
                #:insert-right
                #:replace-node
                #:edit-node
                #:insert-child
                #:append-child
                #:next
                #:prev
                #:end-walk-p
                #:remove-node))

(in-package :zip-test)

(setf *default-reporter* :tap)

(plan nil)

(subtest "Moving downward on a sequence zipper should focus on the leftmost item"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (node (down z))
        1
        :test #'eq)))

(subtest "Moving down, right, and left"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (is (node (left (right (right (right (down z))))))
        3
        :test #'eq)))

(subtest "Moving down then up should be a no-op equality-wise"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (up (down z))
        z
        :test #'fset:equal?)))

(subtest "Moving past a downward boundary should return nil"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (down (down z))
        nil
        :test #'eq)))

(subtest "Moving past an upward boundary should return nil"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (up (up (down z)))
        nil
        :test #'eq)))

(subtest "Moving past a leftward boundary should return nil"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (left (down z))
        nil
        :test #'eq)))

(subtest "Moving past a rightward boundary should return nil"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (right (right (right (down z))))
        nil
        :test #'eq)))

(subtest "When there is nothing to the left, lefts should return an empty seq"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (lefts (down z))
        (empty-seq)
        :test #'fset:equal?)))

(subtest "When there is nothing to the right, rights should return an empty seq"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (rights (right (right (down z))))
        (empty-seq)
        :test #'fset:equal?)))

(subtest "Lefts should return a seq of leftward items"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (lefts (right (right (down z))))
        (seq 1 2)
        :test #'fset:equal?)))

(subtest "Rights should return a seq of rightward items"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (rights (down z))
        (seq 2 3)
        :test #'fset:equal?)))

(subtest "Return the root node of the root"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (root z)
        (seq 1 2 3)
        :test #'fset:equal?)))

(subtest "Moving to the root once or upwards the required amount of times should yield equivalent results"
  (let* ((z (seq-zip (seq 1 2 (seq 3 4 (seq 5)))))
         (bottom (down (right (right (down (right (right (down z)))))))))
    (is (root bottom)
        (node (up (up (up bottom))))
        :test #'fset:equal?)))

(subtest "Moving to the leftmost once or leftwards the required amount of times should yield equivalent results"
  (let* ((z (seq-zip (seq 1 2 3 4))))
    (is (leftmost (right (right (down z))))
        (left (left (right (right (down z)))))
        :test #'fset:equal?)))

(subtest "Moving to the rightmost once or rightwards the required amount of times should yield equivalent results"
  (let* ((z (seq-zip (seq 1 2 3 4))))
    (is (rightmost (down z))
        (right (right (right (down z))))
        :test #'fset:equal?)))

(subtest "Inserting an item to the left"
  (let ((z (seq-zip (seq 1 2 3 5))))
    (is (root (left (insert-left (rightmost (down z)) 4)))
        (seq 1 2 3 4 5)
        :test #'fset:equal?)))

(subtest "Inserting an item to the right"
  (let ((z (seq-zip (seq 1 3 4 5))))
    (is (root (insert-right (down z) 2))
        (seq 1 2 3 4 5)
        :test #'fset:equal?)))

(subtest "Replacing a node"
  (let ((z (seq-zip (seq 1 2 4 4))))
    (is (root (replace-node (right (right (down z))) 3))
        (seq 1 2 3 4)
        :test #'fset:equal?)))

(subtest "Editing a node"
  (let ((z (seq-zip (seq 1 2 1 4))))
    (is (root (edit-node (right (right (down z))) #'+ 2))
        (seq 1 2 3 4)
        :test #'fset:equal?)))

(subtest "Inserting the leftmost node"
  (let ((z (seq-zip (seq 2 3 4))))
    (is (root (insert-child z 1))
        (seq 1 2 3 4)
        :test #'fset:equal?)))

(subtest "Appending the rightmost node"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (root (append-child z 4))
        (seq 1 2 3 4)
        :test #'fset:equal?)))

(subtest "End of walk"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (ok (end-walk-p (next (next (next (next (next z)))))))))

(subtest "Constructing then replacing a node"
  (let ((z (seq-zip (seq 2 3 4))))
    (is
     (root (replace-node z (make-node z (node z) (fset:with-first (children z) 1))))
     (seq 1 2 3 4)
     :test #'fset:equal?)))

(subtest "Removing a middle node"
  (let ((z (seq-zip (seq 1 2 3))))
    (is (root (remove-node (right (down z))))
        (seq 1 3)
        :test #'fset:equal?)))

(subtest "Node movement and equality - prev/next"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (is (prev (prev (next (next z))))
        z
        :test #'fset:equal?)))

(subtest "Moving past the end with next should return an end path"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (ok (end-walk-p (next (next (next (next (next z)))))))))

(subtest "Moving past the end with prev should return nil"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (ok (null (prev z)))))

(subtest "Calling prev on the leftmost node should move upwards"
  (let ((z (seq-zip (seq 1 2 3 4))))
    (is (prev (next z))
        z
        :test #'fset:equal?)))

(subtest "Implementing a zipper"
  (flet ((tree-zip (root)
           (flet ((branchp (loc)
                    (nth-value 1 (fset:lookup (node loc) :nodes)))
                  (children (loc)
                    (fset:lookup loc :nodes))
                  (make-node (node children)
                    (let ((name (fset:lookup node :name)))
                      (fset:map (:name name)
                                (:nodes children)))))
             (->zipper #'branchp
                       #'children
                       #'make-node
                       root))))
    (let* ((inner-nodes (fset:seq (fset:map (:name "a")
                                            (:value "a"))
                                  (fset:map (:name "b")
                                            (:value "b"))
                                  (fset:map (:name "c")
                                            (:value "c"))))
           (outer-nodes (fset:seq (fset:map (:name "one")
                                            (:value 1))
                                  (fset:map (:name "two")
                                            (:nodes inner-nodes))
                                  (fset:map (:name "three")
                                            (:value 3))))
           (root (fset:map (:name "root")
                           (:nodes outer-nodes)))
           (zipper (tree-zip root)))
      (is (fset:lookup (node (down zipper)) :value)
          1
          :test #'eq)

      (is (rights (down zipper))
          (fset:less-first outer-nodes)
          :test #'fset:equal?)

      (is (right (right (down zipper)))
          (rightmost (down zipper))
          :test #'fset:equal?)

      (is (lefts (rightmost (down zipper)))
          (fset:less-last outer-nodes)
          :test #'fset:equal?)

      (is (branchp (right (down zipper)))
          (branchp (next (next zipper)))
          :test #'eq)

      (is (node (down (right (down zipper))))
          (node (next (next (next zipper))))
          :test #'fset:equal?)

      (is (zip:children (right (down zipper)))
          inner-nodes
          :test #'fset:equal?)

      (is (root (down (right (down zipper))))
          (node (prev (prev (prev (down (right (down zipper)))))))
          :test #'fset:equal?))))

(finalize)
