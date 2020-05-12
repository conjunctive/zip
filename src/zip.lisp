(defpackage #:zip
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:import-from #:arrows
                #:->
                #:->>
                #:some->)
  (:import-from #:fset
                #:with
                #:subset?
                #:seq
                #:seq?
                #:empty-seq
                #:concat
                #:with-first
                #:with-last
                #:less-first
                #:less-last)
  (:export #:->zipper
           #:seq-zip
           #:node
           #:branchp
           #:children
           #:make-node
           #:path
           #:lefts
           #:rights
           #:down
           #:up
           #:end-walk-p
           #:root
           #:right
           #:rightmost
           #:left
           #:leftmost
           #:insert-left
           #:insert-right
           #:replace-node
           #:edit-node
           #:insert-child
           #:append-child
           #:next
           #:prev
           #:remove-node))

(in-package #:zip)

(declaim (optimize (speed 3)))

(deftype maybe (&optional type)
  "Nullable values"
  `(or null ,type))

;; Alias #'@ to #'fset:lookup
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition '@) #'fset:lookup))

;; Annotate #'fset:subset?
(declaim (ftype (function (fset:set fset:set) boolean) subset?))

(deftype children ()
  "Type alias for sequences denoting children of a location"
  `(satisfies seq?))

(declaim (ftype (function (seq) boolean) some-seq?))
(defun some-seq? (xs)
  "Populated sequence predicate"
  (not (null (fset::wb-seq-contents xs))))

(declaim (ftype (function (fset:map) fset:set) domain))
(defun domain (m)
  "Monomorphized #'fset:domain for maps"
  (-> m
      (fset::wb-map-contents)
      (fset::WB-Map-Tree-Domain)
      (fset::make-wb-set)))

(defvar path-keys
  (fset:set :left :pnodes :ppath :changedp :right)
  "Set of keys for a path")

(defvar location-keys
  (fset:set :path :tree :branchp :children :make-node)
  "Set of keys for a location")

(declaim (ftype (function (fset:map) boolean) pathp))
(defun pathp (m)
  "Is the provided map a path?"
  (subset? path-keys (domain m)))

(declaim (ftype (function (fset:map) boolean) locationp))
(defun locationp (m)
  "Is the provided map a location?"
  (subset? location-keys (domain m)))

(deftype path ()
  '(and fset:map (satisfies pathp)))

(deftype location ()
  '(and fset:map (satisfies locationp)))

(declaim (ftype (function (seq seq (maybe path) boolean seq) path) ->path))
(defun ->path (left pnodes ppath changedp right)
  "Create a new path"
  (let ((p (fset:map (:left left)
                     (:pnodes pnodes)
                     (:ppath ppath)
                     (:changedp changedp)
                     (:right right))))
    (the path p)))

(declaim (ftype (function ((function (location) boolean)
                           (function (location) children)
                           (function (t children) t)
                           t)
                          location)
                ->zipper))
(defun ->zipper (branchp children make-node root)
  "Create a new zipper"
  (let ((loc (fset:map (:branchp branchp)
                       (:children children)
                       (:make-node make-node)
                       (:tree root)
                       (:path nil))))
    (the location loc)))

(declaim (ftype (function (t) location) seq-zip))
(defun seq-zip (root)
  "A zipper for nested sequences"
  (flet ((branchp (loc)
           (seq? (node loc)))
         (make-node (node children)
           (declare (ignore node)
                    (type children children))
           children))
    (->zipper #'branchp
              #'identity
              #'make-node
              root)))

(defun node (loc)
  "Return the node at the given location"
  (declare (type location loc))
  (@ loc :tree))

(declaim (ftype (function (location) boolean) branchp))
(defun branchp (loc)
  "Is the node at the given location a branch?"
  (funcall (the (function (location) boolean)
                (@ loc :branchp))
           loc))

(declaim (ftype (function (location) (maybe children)) children))
(defun children (loc)
  "From a branch node at the provided location, return a sequence of children"
  (when (branchp loc)
    (the children
         (funcall (the (function (location) children)
                       (@ loc :children))
                  (@ loc :tree)))))

(defun make-node (loc node children)
  "Using the provided locations constructor,
instantiate a new branch node from an exisiting node and new children"
  (declare (type location loc)
           (type children children))
  (funcall (the function (@ loc :make-node))
           node
           children))

(declaim (ftype (function (location) (maybe seq)) path))
(defun path (loc)
  "Return a sequence of nodes leading to the provided location"
  (some-> loc
          (@ :path)
          (@ :pnodes)))

(declaim (ftype (function (location) (maybe seq)) lefts))
(defun lefts (loc)
  "Return a sequence of leftward siblings"
  (some-> loc
          (@ :path)
          (@ :left)))

(declaim (ftype (function (location) (maybe seq)) rights))
(defun rights (loc)
  "Return a sequence of rightward siblings"
  (some-> loc
          (@ :path)
          (@ :right)))

(declaim (ftype (function (location) (maybe location)) down))
(defun down (loc)
  "Attempt to descend to the location of the leftmost child"
  (when (branchp loc)
    (let ((tree (@ loc :tree))
          (path (@ loc :path))
          (children (children loc)))
      (when (some-seq? children)
        (let ((down
                (-> loc
                    (with :tree (fset:first children))
                    (with :path (->path (empty-seq)
                                        (if path
                                            (with-last (@ path :pnodes) tree)
                                            (seq tree))
                                        path
                                        nil
                                        (less-first children))))))
          (the location down))))))

(declaim (ftype (function (location) (maybe location)) up))
(defun up (loc)
  "Attempt to ascend to the location of the parent"
  (when-let ((path (@ loc :path)))
    (let ((tree (@ loc :tree))
          (left (@ path :left))
          (pnodes (@ path :pnodes))
          (ppath (@ path :ppath))
          (changedp (@ path :changedp))
          (right (@ path :right)))
      (when (some-seq? pnodes)
        (let ((pnode (fset:last pnodes)))
          (if changedp
              (let ((up (-> loc
                            (with :tree (->> tree
                                             (with-first right)
                                             (concat left)
                                             (make-node loc pnode)))
                            (with :path (when ppath
                                          (with ppath :changedp t))))))
                (the location up))
              (let ((up (-> loc
                            (with :tree pnode)
                            (with :path ppath))))
                (the location up))))))))

(declaim (ftype (function (location) boolean) end-walk-p))
(defun end-walk-p (loc)
  "Has the provided location reached the end of a depth-first walk?"
  (eq :end (@ loc :path)))

(defun root (loc)
  "Attempt to recursively ascend to the root node"
  (declare (type location loc))
  (labels ((f (loc)
             (if (end-walk-p loc)
                 (node loc)
                 (if-let ((p (up loc)))
                   (f p)
                   (node loc)))))
    (f loc)))

(declaim (ftype (function (location) (maybe location)) right))
(defun right (loc)
  "Attempt to move to the location of the rightward sibling"
  (when-let ((path (@ loc :path)))
    (let ((tree (@ loc :tree))
          (left (@ path :left))
          (right (@ path :right)))
      (when (some-seq? right)
        (let ((right
                (-> loc
                    (with :tree (fset:first right))
                    (with :path (-> path
                                    (with :left (with-last left tree))
                                    (with :right (less-first right)))))))
          (the location right))))))

(declaim (ftype (function (location) location) rightmost))
(defun rightmost (loc)
  "Move to the location of the rightmost sibling"
  (let ((tree (@ loc :tree))
        (path (@ loc :path)))
    (if (and path (some-seq? (@ path :right)))
        (let ((left (@ path :left))
              (right (@ path :right)))
          (declare (type seq left right))
          (let ((rightmost
                  (-> loc
                      (with :tree (fset:last right))
                      (with :path (-> path
                                      (with :left (concat left (with-first (less-last right) tree)))
                                      (with :right (empty-seq)))))))
            (the location rightmost)))
        loc)))

(declaim (ftype (function (location) (maybe location)) left))
(defun left (loc)
  "Attempt to move to the location of the leftward sibling"
  (when-let ((path (@ loc :path)))
    (let ((tree (@ loc :tree))
          (left (@ path :left))
          (right (@ path :right)))
      (when (some-seq? left)
        (let ((left
                (-> loc
                    (with :tree (fset:last left))
                    (with :path (-> path
                                    (with :left (less-last left))
                                    (with :right (with-first right tree)))))))
          (the location left))))))

(declaim (ftype (function (location) location) leftmost))
(defun leftmost (loc)
  "Move to the location of the leftmost sibling"
  (let ((tree (@ loc :tree))
        (path (@ loc :path)))
    (if (and path (some-seq? (@ path :left)))
        (let ((left (@ path :left))
              (right (@ path :right)))
          (declare (type seq left right))
          (let ((leftmost
                  (-> loc
                      (with :tree (fset:first left))
                      (with :path (-> path
                                      (with :left (empty-seq))
                                      (with :right (concat (with-last (less-first left) tree) right)))))))
            (the location leftmost)))
        loc)))

(declaim (ftype (function (location t) (maybe location)) insert-left))
(defun insert-left (loc item)
  "Attempt to insert an item as the leftward sibling of the provided location"
  (when-let ((path (@ loc :path)))
    (let ((left
            (with loc :path (-> path
                                (with :left (with-last (@ path :left) item))
                                (with :changedp t)))))
      (the location left))))

(declaim (ftype (function (location t) (maybe location)) insert-right))
(defun insert-right (loc item)
  "Attempt to insert an item as the rightward sibling of the provided location"
  (when-let ((path (@ loc :path)))
    (let ((right
            (with loc :path (-> path
                                (with :changedp t)
                                (with :right (with-first (@ path :right) item))))))
      (the location right))))

(declaim (ftype (function (location t) location) replace-node))
(defun replace-node (loc node)
  "Replace the node of the provided location"
  (let ((replaced
          (-> loc
              (with :tree node)
              (with :path (when-let ((path (@ loc :path)))
                            (with path :changedp t))))))
    (the location replaced)))

(defun edit-node (loc fn &rest args)
  "Replace the node of the provided location with the result of a function"
  (declare (type location loc)
           (type function fn))
  (replace-node loc (apply fn (node loc) args)))

(declaim (ftype (function (location t) (maybe location)) insert-child))
(defun insert-child (loc item)
  "Attempt to insert an item as the leftmost child of the provided location"
  (when-let ((children (children loc)))
    (let ((child (->> item
                      (with-first children)
                      (make-node loc (node loc))
                      (replace-node loc))))
      (the location child))))

(declaim (ftype (function (location t) (maybe location)) append-child))
(defun append-child (loc item)
  "Attempt to insert an item as the rightmost child of the provided location"
  (when-let ((children (children loc)))
    (let ((child (->> item
                      (with-last children)
                      (make-node loc (node loc))
                      (replace-node loc))))
      (the location child))))

(declaim (ftype (function (location) (maybe location)) next))
(defun next (loc)
  "Attempt to move to the location of the next sibling"
  (labels ((f (p)
             (if (up p)
                 (or (right (up p))
                     (f (up p)))
                 (-> p
                     (with :tree (node p))
                     (with :path :end)))))
    (let ((next
            (if (end-walk-p loc)
                loc
                (or (and (branchp loc)
                         (down loc))
                    (right loc)
                    (f loc)))))
      (the location next))))

(declaim (ftype (function (location) (maybe location)) prev))
(defun prev (loc)
  "Attempt to move to the location of the previous sibling"
  (if-let ((l (left loc)))
    (labels ((f (loc)
               (if-let ((child (and (branchp loc)
                                    (down loc))))
                 (f (rightmost child))
                 loc)))
      (f l))
    (up loc)))

(declaim (ftype (function (location) (maybe location)) remove-node))
(defun remove-node (loc)
  "Remove the node of the provided location"
  (when-let ((path (@ loc :path)))
    (let ((left (@ path :left))
          (pnodes (@ path :pnodes))
          (ppath (@ path :ppath))
          (right (@ path :right)))
      (if (some-seq? left)
          (let ((removed
                  (labels ((f (loc)
                             (if-let ((child (and (branchp loc) (down loc))))
                               (f (rightmost child))
                               loc)))
                    (-> loc
                        (with :tree (fset:last left))
                        (with :path (-> path
                                        (with :left (less-last left))
                                        (with :changedp t)))
                        (f)))))
            (the location removed))
          (let ((removed
                  (-> loc
                      (with :tree (make-node loc (fset:last pnodes) right))
                      (with :path (when ppath (with ppath :changedp t))))))
            (the location removed))))))
