(comment
  (ns junk.core
    (:gen-class )
    (:use jfxutils.core)
    (:use (clojure repl reflect pprint))
    (:use (clojure.tools trace))

    (:import (javafx.beans.value ObservableValue ChangeListener)
             (javafx.stage Stage)
             (javafx.scene Scene SceneBuilder Group Node)
             (javafx.scene.control TextArea TextField)
             (javafx.scene.layout VBox)
             ;;           (java.io PrintStream OutputStream PrintWriter)
             )))


(defn make-console-scene [init_text]
  ;; Creates appropriate writers, etc., and returns PrintWrite and scene
  ;; printwriter should be bound to *out* later
  (let [ta (TextArea. init_text )
        console (proxy [java.io.OutputStream] []
                  (write [bytes offset length ]
                    (.appendText ta (String. bytes))))
        pw (PrintWriter. console true)
        group (Group. )
        scene (Scene. group)]
    (add-to-children group ta)
    [pw scene]))


(defn make-console-scene
  ([] (make-console-scene ""))
  ([init_text]
   ;; Creates appropriate writers, etc., and returns PrintWriter and scene
   ;; printwriter should be bound to *out* later
   (let [ta (TextArea. init_text )
         output-stream (proxy [java.io.OutputStream] []
                         (write
                           ([bite] ;; bite is an int
                            (.println *err* "in write-1")
                            (.println *err* (str "type is: " (type bite)))
                            ;;(.println *err* (str "thread is:" (Thread/currentThread)))
                            ;;(.println *err* (str "\"" (char bite) "\", type is: " (type bite) ", thread is:" (Thread/currentThread)))
                            ;;(.println *err* (Thread/currentThread))
                            (.appendText ta (str (char bite))))
                           ([buf offset length] ;; buf is bytes[]
                            (let [bytestr (String. (byte-array (take length buf)))]
                              (.println *err* "in write-3")
                              (.println *err* (str "type is: " (type buf)))
                              (.println *err* (str "length is: " (count buf)))
                              (.println *err* bytestr)
                              ;;(.println *err* (str "thread is:" (Thread/currentThread)))
                              ;;(.println *err* (str "\"" (String. buf) "\", " offset ", " length ", type is: " (type buf) ", thread is: " (Thread/currentThread)))
                              ;;(.println *err* (Thread/currentThread))
                              (.appendText ta bytestr))
                            ))
                         (flush []))
         writer (proxy [java.io.Writer] []
                  (write
                    ([bite] ;; an int
                     (.print *err* "in write-1")
                     (.println *err* (str "\"" (char bite) "\", type is: " (type bite) ", thread is:" (Thread/currentThread)))
                     ;;(.println *err* (Thread/currentThread))
                     (.appendText ta (str (char bite))))
                    ([buf offset length] ;; buf is string
                     (.print *err* "in write-3")
                     (.println *err* (str "\"" buf "\", " offset ", " length ", type is: " (type buf) ", thread is: " (Thread/currentThread)))
                     ;;(.println *err* (Thread/currentThread))
                     (.appendText ta buf)
                     ))
                  (flush []))
         console output-stream
         pw (PrintWriter. console true)
         group (Group. )
         scene (Scene. group)]
     (add-to-children group ta)
     [pw scene])))


(defn make-vbox-scene [& items]
  (let [vb (VBox.)
        scene (Scene. vb)]
    (apply add-to-children vb items)
    scene))

(defn make-window [scene & [width height]]
  (let [stage (Stage.)]
    (.setScene stage scene)
    (when width (.setWidth stage width))
    (when height (.setHeight stage height))
    stage)
  )



(defprotocol IObservers
  (update [this newval] [this from newval])) ;; notifies everyone else from someone 

(defrecord obsrecord [state watchfn observers]
  javafx.beans.value.ObservableValue
  (^void addListener  [this ^javafx.beans.value.ChangeListener listener] (println "added listener" listener))
  (^void removeListener [this ^javafx.beans.value.ChangeListener listener] (println "remote listener" listener))
  (getValue [this] (println "getting value:" @state))

  javafx.beans.value.ChangeListener
  (^void changed [this ^javafx.beans.value.ObservableValue obsval oldval newval]
    (println "Change listener detected change:" obsval oldval newval)
    (println "Thread" (Thread/currentThread) ", *out* is " *out*))
  
  IObservers
  (update [this from newval] (println "in IObservers update for obsrecord")))

(extend-protocol IObservers
  javafx.beans.value.ObservableValue
  (update
    ([this from newval]
     (println "In IObservers update for ObservableValue")
     (println "From: " from)
     (println "Newval: " newval))
    ([this newval]
     (println "In IObservers update for ObservableValue")
     (println "Newval: " newval))))


;;; method 1
;;; UI has local ChangeListener which calls reset/alter/etc.
;;; watch method is associated with one ChangeListener only
;;; watch method changes value based on ChangeListener parameters
;;; When observer var is changed by program, watch method is called again automatically
;;; Clojure runtime keeps track of "clients", ie watch targets

;;; method 2
;;; observer var is a ChangeListener with list of observer JFX properties
;;; changes to property call observer var.changed directly
;;; changed() goes through observers list and updates other JFX properties
;;; When observer var is changed by program, nothing happens unless...
;;; observer var also implements ObservableValue




(def text1 (TextField. "hi"))
(.setOnAction text1 (event-handler [evt] (println "in event handler, out is " *out*)))


(defn -start [out]
  (println "-start thread is " (Thread/currentThread) ",-start *out* is " *out*)
  (let [ ;;text1 (TextField. "hi")
        [pw console-scene] (make-console-scene "xxx")
        debugwin (make-window console-scene 400 300)]
    (binding [*out* out]
      (let [myrecord (obsrecord. {} (fn []) [])
            text2 (TextField. "bye")
            vbscene1 (make-vbox-scene text1 text2)
            window1 (make-window vbscene1)]


        ;;(-> text1 .textProperty (.addListener myrecord))
        (println "After second binding first, thread is " (Thread/currentThread) "*out* is " *out*)
        (-> text1 .textProperty (.addListener (proxy [ChangeListener] []
                                                (changed [obsval oldval newval]
                                                  (println "in change listener, thread is " (Thread/currentThread) "*out* is " *out*)))))
        ;;text3 (TextField. "hola")
        ;;text4 (TextField. "ciao")
        ;;vbscene2 (make-vbox-scene text3 text4)
        ;;window2 (make-window vbscene2)
        (println "After second binding second, thread is " (Thread/currentThread) "*out* is " *out*)
        (.show debugwin)
        (.show window1))
      ;;(.show window2)
      )))

(defn main []
  (println "main thread is " (Thread/currentThread) ", *out* is " *out*)
  (let [out *out*]
    (run-now (-start out))))

(defn -main []
  (javafx.application.Platform/setImplicitExit true)
  (main))


(defn font-window [size]
  (let [names (vec (Font/getFamilies))
        texts (map #(jfxnode Text % :font (Font. % size)) names)
        vb (jfxnode VBox :children texts :spacing 10)
        sp (jfxnode ScrollPane :content vb)]
    (doto (Stage.)
      (.setScene (Scene. sp)))))


(defn bind!
  ;; Create a ChangeListener on the JFX Property and a watch on the
  ;; atom/ref (a map), return nil.  The Property is associated with a
  ;; specific nested sub-element of the atom/ref map.  Argument is a
  ;; conversion map, with keys :var, :accesspath, :prop, :toprop,
  ;; :errfn!, :clrfn!.  :var is the atom/ref in question. :access is a
  ;; vector of arguments to assoc-in or update-in or get-in. :prop is
  ;; the UI property. :toprop is a fn that converts from the
  ;; underlying var map element to the property type.  errfn! and
  ;; clrfn! are fns that cause the UI to indicate or clear an error,
  ;; respectively.
  [ & {:keys [var accesspath prop toprop errfn! clrfn!]}]
  (println "binding" prop)
  (let [changed-by (ref nil) ;; Hate this! but it works
        original-item-type (type (get-in @var accesspath))
        change-listener (reify ChangeListener
                          ;; Here we convert from property type eg String to the underlying model type,
                          ;; eg. Double, Long, Boolean, etc.  clrfun is called when done;
                          ;; errfun is called instead if exception occurs
                          (changed [this obsval old-prop-val new-prop-val]
                            (println "change listener")
                            (try
                              (let [existing-var-item (get-in @var accesspath)
                                    tovarfn #(si/convert-to original-item-type %)
                                    new-var-val (assoc-in @var accesspath (tovarfn new-prop-val))]
                                (dosync
                                 (ref-set changed-by prop)
                                 (ref-set var new-var-val)
                                 (when clrfn! (clrfn!))))
                              (catch Exception e
                                (println "oops")
                                (pst)
                                (when errfn! (errfn!))))))]
   
    (.addListener prop change-listener)
    ;; use property as key for this watch so there is one watch per
    ;; property. Triggered when the underlying var changes, then
    ;; updates the various properties with proper type conversion.
    ;; Typically this means converting to some type of string or
    ;; watever is the underlying property type.
    (add-watch var prop (fn [key ref old-var-val new-var-val]
                          (if (= prop @changed-by) ;; don't change if this property triggered change
                            (dosync (alter changed-by (constantly nil)))
                            (do ;; otherwise change the property to match
                              (.removeListener prop change-listener)
                              (.setValue prop (toprop (get-in new-var-val accesspath)))
                              (.addListener prop change-listener)
                              (when clrfn! (clrfn!))))))
    nil))

(comment
  (defn update-properties!
    "Calculates conversions, origin, and updates properties"
    [{:keys [ppg-prop upg-prop upp-prop ppu-prop
             zoom-level-prop total-zoom-prop
             originX-prop originY-prop]} canvas]
    (let [specs (.getUserData canvas)
          zoomspecs (:zoom specs)
          canvasspecs (:canvas specs)
          [ppgX upgX uppX ppuX] (calc-conversions zoomspecs first)]

      (.setValue ppg-prop ppgX)
      (.setValue upg-prop upgX)
      (.setValue upp-prop upgX)
      (.setValue ppu-prop ppuX)
      (.setValue zoom-level-prop (:zoom-level zoomspecs))
      (.setValue total-zoom-prop 99.9)
      (.setValue originX-prop (first (:origin canvasspecs)))
      (.setValue originY-prop (second (:origin canvasspecs))))))



    (comment
      (.addListener (:ppg-prop grid-properties) (change-listener [ _ nv]  (println "ppg:" nv)))
      (.addListener (:upg-prop grid-properties) (change-listener [ _ nv]  (println "upg:" nv)))
      (.addListener (:upp-prop grid-properties) (change-listener [ _ nv]  (println "upp:" nv)))
      (.addListener (:ppu-prop grid-properties) (change-listener [ _ nv]  (println "ppu:" nv)))
      (.addListener (:zoom-level-prop grid-properties) (change-listener [ _ nv]  (println "zoom-level:" nv)))
      (.addListener (:total-zoom-prop grid-properties) (change-listener [ _ nv]  (println "total-zoom:" nv)))
      (.addListener (:originX-prop grid-properties) (change-listener [ _ nv]  (println "origin-X:" nv)))
      (.addListener (:originY-prop grid-properties) (change-listener [ _ nv]  (println "origin-Y:" nv))))



(declare single?)
(declare solo?)
(declare chain?)
(declare children)
(declare singlefy)
(declare siblings?)

(extend-protocol tv/Tree
  java.lang.Iterable
  (get-items [tree]
    ;; children could `return a single, a solo, a chain, or siblings
    ;; (a b c ... d) -> (b c ... d)
    ;; (a (b c) `
    (let [c (children tree)
          result (cond ;;(single? c) (do (run-later (println "single")) c)
                       (solo? c) c
                       (chain? c) (list (children tree))
                       :siblings (children tree))]
      ;;(run-later (println (str "(get-items " tree ") -> " c " -> " result)))
      result))
  (leaf? [tree] (let [result (or (single? tree) (solo? tree))]
                  ;;(run-later (println "leaf?" tree ":" result))
                  result)) 
  (status-image [tree state] (state {:leaf tv/file-image
                                     :collapsed tv/collapsed-folder-image
                                     :expanded tv/expanded-folder-image}))
  (short-str [tree] (if (chain? tree)
                      (str (first tree))
                      (str (singlefy tree)))))





(defn single?
  "True if just not a list"
  [x]
  (and x (not (sequential? x))))

(declare singlefy)
(defn solo?
  "True if a list, but only has one thing"
  [x]
  (and (sequential? x)
      (single? (singlefy x))))

(defn chain?
  "Checks whether the first item is single and there is a second item.
  This should be complement of siblings? if such a function were to
  exist, but also covers the single case, e.g. (a)."
  [x]
  (boolean (and (sequential? x)
                (single? (first x))
                (second x))))

(defn append
  "Puts x at the end of lst"
  [lst x]
  (reverse (cons x (reverse lst))))

(defn push
  "Add value as child of lst if chain, or children of list. Can't push
  if lst is not a sequential."
  [lst value]
  (cond (nil? value) lst
        (single? value) (append lst value)
        (solo? value) (append lst (first value))
        (chain? value) (append lst value)
        :siblingsa (cond (empty? lst) (append lst value)
                         (or (chain? lst)
                             (solo? lst)) (reduce #(append %1 (singlefy %2)) lst value))))



(defn singlefy
  "Remove extra parens, but leave the first item in a list if it's a
  single item"
  [x]
  (if (sequential? x)
    (if (= (count x) 1)
      (singlefy (first x)) ;; eg (singlefy (a)) when x is ((a))
      (if (and (sequential? (first x))
               (> (count (first x)) 1))
        (map singlefy x) ;; count is > 1, and first is list > 1, eg ((a b)(c))
        (cons (first x) (map singlefy (rest x))))) 
    x)) ;; x is not sequential, eg a

(defn listify
  "Makes first element a list if it isn't already.  Returns nil when x
  is empty list.  Used for "
  [x]
  (if (sequential? x)
    (if (empty? x) nil
      (if (single? (first x))
        (if (seq (rest x))
          (cons (list (first x)) (seq (rest x)))
          (list (first x)))
        x))
    (list x)))

(defn same-heads? [a b]
  (and (chain? a)
       (chain? b)
       (= (first a) (first b))))

(defn siblings?
  "True if x starts with a list"
  [x]
  (boolean (and (sequential? x)
                (sequential? (first x)))))



(defn ncons
  "Conses x onto lst only if x is not nil, otherwise returns lst"
  [x lst]
  (if x (cons x lst)
      lst))

(declare treemerge)

(defn merge-siblings
  "Tries to merge all elements elements in a and b.  Concatenate a and
  b to create list c, then create left as first of c, and right as
  rest of C.  For each element in right, tries to merge with each
  element in left.  If they merge, keep result in same position and
  start process over again with next element in right.  If they don't
  merge, take the second of the result and append it to the end of left."
  [a b]
  (let [c (cond (and (single? a) (single? b)) (listify (list a b))
                (and (single? a) (sequential? b)) (cons a b)
                (and (sequential? a) (single? b)) (append a b)
                :else (listify (append a b)))
        innerfn (fn [lst val] ;; reducer fn
                  (loop [i 0  
                         lst lst
                         done? false]
                    (if done? lst
                        (let [end? (= i (count lst))
                              head (when (not end?) (nth lst i))
                              mr (when (not end?) (treemerge head val))
                              merged? (when (not end?) (not (siblings? mr)))
                              nextresult (if end?
                                           (listify (append lst val))
                                           (if merged?
                                             (replace {head mr} lst)
                                             lst))
                              done? (or merged? end?)]
                          (recur (inc i) nextresult done? )))))]
    (reduce innerfn (first c) (rest c))))


(defn children
  "Retrieves the children of x, if any, without changing hierarchy.
  a -> nil
  (a) -> nil
  ((a) ...) -> nil
  (a b) -> b or (b)
  (a b c) -> ((b) c)
  (a (b c)) -> (b c)"
  [x]
  (if (chain? x) ;; only a chain can have children
    (let [rx (rest x)]
      (if (single? (first rx))
        (listify rx)
        (singlefy rx)))
    nil))


(defn treemerge
  "Joins tree segments, assuming there is at least on element at the
  start which matches"
  [a b]
  (let [a (if (single? a) (listify a) a)
        b (if (single? b) (listify b) b)]
    (cond
      (nil? a) b
      (nil? b) a
      (= a b) a
      ;;(and (nil? a)       b)  b
      ;;(and       a  (nil? b)) a
      (and (solo? a) (solo? b))  (if (= (first a) (first b))
                                   (first a)
                                   (listify (list (first  a) (first b))))
      ;; (and (single? a) (chain? b) (= a (first b))) (push (list a) (children b))
      ;; (and (chain? a) (single? b) (= (first a) b)) (push (list b) (children a))
      (and (solo? a) (chain? b)) (if (= (first a) (first b))
                                   (push a (children b))
                                   (list a b))
      (and (chain? a) (solo? b)) (if (= (first a) (first b))
                                   (push b (children a) )
                                   (list a b))
      (same-heads? a b) (push (listify (first a)) (treemerge (children a) (children b)))
      (or (siblings? a) (siblings? b))  (merge-siblings a b) 
      :else (list a b))))


(defn hierarchify
  "Makes the first element of lst the parent and each subsequent
  element a child, recursively"
  [lst]
  (loop [accum (first lst)
         lst (rest lst)]
    (if (empty? lst) accum
        (recur (list (first lst) accum) (rest lst)))))

(defn merge-loop [& objs]
  (loop [i 0
         result nil]
    (if (= i (count objs)) result
        (let [tright (nth objs i)
              merge-result (treemerge result tright)]
          (recur (inc i) merge-result)))))

(defn merge-hierarchies
  "Joins two objects' hierarchies"
  [ & objs]
  (let [trees (map #(hierarchify (find-hierarchy %)) objs)]
    (apply merge-loop trees)))

(def A '(a b))
(def B '(a (c (d (e)))))
(def C '(a (c (d (f)))))


(def field-options {:color1 {:combo-items toydb.tableviews/color-vals :alt-name "Choose color here"}
                    :color2 {:color-picker true :alt-name "Another color chooser"}
                    :alpha {:combo-items [10 20 30 40 50]} ;; color-vals
                    :beta {:combo-items [10 20 30 40 50 ]}
                    :width1 {:combo-items [1.5 2.5 3.5]}
                    :width2 {:alt-name "Width Two"}
                    :type {:alt-name "Some Fancy Type"}})
