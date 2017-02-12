(ns toydb.treeview
  (:use [jfxutils.core :exclude [-main]]
        [toydb.treemerge :as tm])
  
  (:import [java.nio.file FileSystems Files LinkOption]
           [java.net InetAddress]
           [javafx.scene.control TreeItem TreeView TreeCell ProgressIndicator Button Tooltip]
           [javafx.scene.image ImageView]
           [javafx.scene.control.cell TextFieldTreeCell]
           [javafx.util Callback]
           [javafx.concurrent Task Worker Worker$State]
           [javafx.application Platform]))



(comment
  (def root-image (image "icons/tango-icons-0.8.90/scalable/devices/computer.svg"))
  (def file-image (image "icons/tango-icons-0.8.90/scalable/mimetypes/text-x-generic.svg"))
  (def collapsed-folder-image (image "icons/tango-icons-0.8.90/scalable/places/folder.svg") )
  (def expanded-folder-image  (image "icons/tango-icons-0.8.90/scalable/status/folder-open.svg")))

;; From iconfinder website
(def root-image (image "icons/tango-icons-0.8.90/scalable/devices/computer.svg"))
(def file-image (image "icons/must_have_icon_set/Text Document/Text Document_16x16.png"))
(def collapsed-folder-image (image "icons/must_have_icon_set/Folder/Folder_16x16.png"))
(def expanded-folder-image (image "icons/must_have_icon_set/Folder/Folder_16x16.png"))



(defprotocol TreeProtocol
  "Protocol to manage items going into a TreeView"
  (get-items [item])
  (leaf? [item])
  (status-image [item state])
  (short-str [item]))

(extend-protocol TreeProtocol
  sun.nio.fs.WindowsPath
  (get-items [path] (try (Files/newDirectoryStream path)
                         (catch Exception e ["(Empty drive)"])))
  (leaf? [path] (Files/isRegularFile path (into-array LinkOption []))) ;; takes a long time
  (status-image [path state] (state {:leaf file-image
                                     :collapsed collapsed-folder-image
                                     :expanded expanded-folder-image}))
  (short-str [path] (str (or (.getFileName path) path)))

  java.net.Inet4Address
  (get-items [a] (try (.. FileSystems getDefault getRootDirectories)
                      (catch Exception e (println "My exception caught"))))
  (leaf? [a] (empty? (get-items a)))
  (status-image [a state] (state {:leaf root-image
                                  :collapsed root-image
                                  :expanded root-image}))
  (short-str [a] (.getHostName a))

  java.lang.Iterable
  (get-items [coll] (tm/children coll))
  (leaf? [coll] (empty? (tm/children coll)))
  (status-image [col state] (state {:leaf file-image
                                     :collapsed collapsed-folder-image
                                     :expanded expanded-folder-image}))
  (short-str [coll] (str (first coll)))
  
  Object
  (leaf? [kw] true)
  (status-image [path state] (state {:leaf file-image}))
  (short-str [x] (str x))

  nil
  (leaf? [kw] true)
  (status-image [path state] (state {:leaf file-image}))
  (short-str [x] nil))





(declare tree-item)
(defn build-children-builder
  "Returns a memoized function which does the following potentially
  expensive actions: 
  1. Retrieves items via (get-items), which could be slow
  2. Creates tree-items from items 
  3. Assigns these items to this Treeitem's children via a manual macroexpansion of proxy-super"
  []
  (letfn [(super-get-children [treeitem]
            (proxy-call-with-super
             (fn [] (.getChildren treeitem)) treeitem "getChildren"))]
    (memoize (fn [treeitem] ;; this is the fn that gets returned and called by creator
               (let [value (.getValue treeitem)
                     items (get-items value)]
                 (doto (super-get-children treeitem)
                   (.setAll (map tree-item items))))))))


#_(defn xtree-item
  "Creates a tree item for use in a TreeView.
  item is the underlying item. images is map of states to images"
  [item & [expanded?]]
  (let [leaf-future (future (leaf? item))
        build-children (build-children-builder)
        treeitem (proxy [TreeItem] [item] 
                   (getChildren [] (build-children this))
                   (isLeaf [] (if (future-done? leaf-future)
                                @leaf-future
                                false)))]

    (when expanded?
      (.setExpanded treeitem true))

    ;; Set the swirly wait graphic
    (.setGraphic treeitem (doto (ProgressIndicator.)
                            (.setPrefWidth default-icon-size)
                            (.setPrefHeight default-icon-size)))

    ;; Thread waits for leaf? to finish, then sets graphics and handler in FX thread
    (when-done leaf-future
      #(if %
         (run-later (.setGraphic treeitem (image-view (status-image item :leaf) default-icon-size)))
         (let [^ImageView imageview (image-view (status-image item :collapsed) default-icon-size)]
           (run-later (doto treeitem
                        (.setGraphic imageview)
                        (.addEventHandler (TreeItem/branchExpandedEvent)   
                                          (eventhandler [e] (when (.isExpanded treeitem)
                                                              (.setImage imageview (status-image item :expanded)))))
                        (.addEventHandler (TreeItem/branchCollapsedEvent)
                                          (eventhandler [e] (when (not (.isExpanded treeitem))
                                                              (.setImage imageview (status-image item :collapsed))))))))))
    treeitem))

(defn tree-item
  "Creates a tree item for use in a TreeView.
  item is the underlying item. images is map of states to images"
  [item & [expanded?]]
  (let [leaf-future (future (leaf? item))
        build-children (build-children-builder)
        treeitem (proxy [TreeItem] [item] 
                   (getChildren [] (build-children this))
                   (isLeaf [] (if (future-done? leaf-future)
                                @leaf-future
                                false)))]
    (when expanded?
      (.setExpanded treeitem true))
    treeitem))


(def tree-cell-factory
  (callback [value]
            (proxy [TreeCell] []
              (updateItem [item empty?] ;; item refers to the underlying item, not the treeitem
                (proxy-super updateItem item empty?)
                (if (or empty? (nil? item))
                  (do (.setGraphic this nil)
                      (.setText this nil))
                  (do (.setGraphic this (doto (ProgressIndicator.)
                                          (.setPrefWidth default-icon-size)
                                          (.setPrefHeight default-icon-size)))
                      (.setText this (short-str item))
                      (.setEditable this true)
                      (Tooltip/install this (Tooltip. (str item)))
                      (let [treeitem (.getTreeItem this)]
                        (if (.isLeaf treeitem)
                          (run-later (.setGraphic this (image-view (status-image item :leaf) default-icon-size))) 
                          (let [^ImageView imageview (image-view (status-image item :collapsed) default-icon-size)]
                            (run-later (doto this
                                         (.setGraphic imageview)
                                         (.addEventHandler (TreeItem/branchExpandedEvent)   
                                                           (eventhandler [e] (when (.isExpanded treeitem)
                                                                               (.setImage imageview (status-image item :expanded)))))
                                         (.addEventHandler (TreeItem/branchCollapsedEvent)
                                                           (eventhandler [e] (when (not (.isExpanded treeitem))
                                                                               (.setImage imageview (status-image item :collapsed))))))))))))))))


(defn tree-view
  "Create a TreeView of the given item.  The item must support the Tree protocol"
  [item & [expanded?]]
  (try
    (doto (TreeView. (tree-item item expanded?))
      (.setCellFactory tree-cell-factory))
    (catch Exception e
      (println "Couldn't create treeview" e))))













