(ns toydb.treemerge
  ;;(:require [clojure.reflect :as r])
  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure.pprint])
  (:use [clojure.repl :exclude [root-cause]]))


;;                        1
;;               2     3     4      5
;;             6 7 8         9   10  11
;;                           12

'(1 (2 (6) (7) (8)) (3) (4 (9 (12))) (5 (10) (11)))





;; a child is always also a tree
;; use make-tree when adding atomic things

(defn make-tree [head]
  (list head))

(defn make-tree-from-leaves 
  "Makes head into tree and listifies children"
  [head & leaf-children]
  (apply list head (map make-tree leaf-children)))

(defn make-tree-from-trees 
  "Makes head into tree and adds children"
  [head & tree-children]
  (apply list head tree-children))

(defn make-tree-from-chain
  "Makes tree where each item in coll is a child of the previous one"
  [coll]
  (loop [acc '()
         h coll]
    (if (empty? h)
      acc
      (if (empty? acc)
        (recur (list (first h)) (rest h))
        (recur (list (first h) acc) (rest h))))))

(defn count-children 
  "Returns how many first-level children there are"
  [tree]
  (count (rest tree)))

(defn children [tree]
  (rest tree))

(defn first-child 
  "Returns leftmost child as a tree"
  [tree]
  (first (rest tree)))

(defn nth-child [tree n]
  (nth (rest tree) n))

(defn data [tree]
  (first tree))

(defn add-tree 
  "Adds childtree to end of tree"
  [tree childtree]
  (reverse (cons childtree (reverse tree))))

(defn add-leaf 
  "Listifies childleaf, then adds to end of tree"
  [tree childleaf]
  (reverse (cons (make-tree childleaf) (reverse tree))))

(defn replace-tree
  "Ignores tree and returns newtree"
   [tree newtree]
  newtree)

(defn replace-leaf
  "Ignores tree, then listifies and returns newleaf"
   [tree newleaf]
  (list newleaf))

(defn tree-add-in
  "Finds subtree via invec, which is a vecor of children for each
  level of tree.  When subtree has been found, adds child using
  whichfn, which should be either add-tree or add-leaf"
  [tree invec dofn arg]
  ;; Works by rebuilding tree from the top recursively.  At each level
  ;; it separates the target subtree from the others, then calling
  ;; itself with the target subtree
  (if (empty? invec)
    (dofn tree arg)
    (let [other-nths (remove #(== % (first invec)) (range (count-children tree)))
          other-subtrees (map #(nth-child tree %) other-nths)
          target-subtree (nth-child tree (first invec))
          new-subtree (tree-add-in target-subtree (rest invec) dofn arg)]
      (apply make-tree-from-trees (first tree) new-subtree other-subtrees))))
  


(def one (make-tree-from-trees
          1
          (make-tree-from-leaves 2 6 7 8)
          (make-tree 3)
          (make-tree-from-trees 4
                                (make-tree-from-leaves 9 12))
          (make-tree-from-leaves 5 10 11)))

(def one '(1 (2 (4) (5)) (3 (6) (7))))
(def two '(3 (6 (9) (10 (11) (12)))))


(defn find-node 
  "Returns vector of node's location in tree.  node must be the
  actual data to compare, such as x instead of (x) "
  [tree node]
  (letfn [(inner-find [tree node acc]
            (if (= node (data tree)) acc  ;; terminating case
                (loop [chlds (children tree) ;; go through each child
                       idx 0
                       acc acc]
                  (if (seq chlds) ;; If there are children, each one gets searched until one returns non-nil
                    (if-let [res (inner-find (first chlds) node (conj acc idx))]
                      res
                      (recur (rest chlds) (inc idx) acc))
                    nil))))]
    (inner-find tree node [])))

(defn find-common-head 
  "Return [nil rvector] or [lvector nil] or [nil nil] or [[] []] where
  lvector is right's head's location in left tree and vice versa.  If
  there is no match, two nils are returned.  If the heads of both
  trees match right away, the return is two empty vectors."
  [left right]
  [(find-node left (data right))
   (find-node right (data left))])


(defn subtree-at 
  "Returns subtree at location vector loc"
  [tree loc]
  (loop [tree tree
         loc loc]
    (cond (nil? loc) nil
          (empty? loc) tree
          :else (recur (nth-child tree (first loc)) (rest loc)))))


(defn inner-merge 
  "Returns single tree, assuming dst and src have the same head to
  begin with"
  [dst src]
  (let [tree (make-tree (data dst))
        all-children (concat (children dst) (children src))]
    (loop [tree tree
           children all-children]
      (if (empty? children)
        tree
        (let [loc (or (find-node tree (data (first children))) [])
              child-exists (= (count loc) 1) ;; child at least partly already exists ]
              dofn (if child-exists replace-tree add-tree )
              arg (if child-exists
                    (inner-merge (subtree-at tree loc) (first children) )
                    (first children))
              updated-tree (tree-add-in tree loc dofn arg)]        
          (recur updated-tree (rest children)))))))


(defn tree-merge
  "Merges left and right trees, where one tree may be fully,
  partially, or not at all represented in the other"
  [left right]
  (if (= left right) left ;; perfect match
      (let [ch (find-common-head left right)]
        (cond (= ch [nil nil]) (list left right) ;; no merge possible
              (= ch [[] []]) (inner-merge left right) ;; heads match, so arbitrarily pick left as dst
              :else (let [dst (if (nil? (first ch)) right left)
                          src (if (nil? (first ch)) left right)
                          loc (or (first ch) (second ch))
                          subtree (subtree-at dst loc)
                          merged-subtree (inner-merge subtree src)]
                      (tree-add-in dst loc replace-tree merged-subtree))))))

(tree-merge one two)




















