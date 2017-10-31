(ns toydb.app.treeview
  (:require [jfxutils.core :refer :all #_[add-event-filter! callback
                                   default-icon-size event-handler
                                   image image-view jfxnew printexp run-now run-later
                                          set-children! set-items! set-on-action! stage]
             :exclude [-main]]
            [toydb.app.treemerge :as tm])
  (:import (java.net InetAddress)
           (java.nio.file Files FileSystems)
           (javafx.geometry Pos)
           (javafx.scene.control ContextMenu MenuItem TextField Tooltip TreeCell
                                 TreeItem TreeView)
           (javafx.scene.control.cell TextFieldTreeCell)
           (javafx.scene.input KeyCode KeyEvent MouseButton MouseEvent)
           (javafx.scene.layout HBox))
  (:gen-class))



;;(set! *warn-on-reflection* true)

(def icopath "icons/tango-icon-theme-0.8.90/scalable/")
(def computer-image (image (str icopath "devices/computer.svg")))
(def root-image (image (str icopath "devices/drive-harddisk.svg")))
(def file-image (image (str icopath "mimetypes/text-x-generic.svg")))
(def collapsed-folder-image (image (str icopath "places/folder.svg")))
(def expanded-folder-image  (image (str icopath "status/folder-open.svg")))
(def error-image (image (str icopath "status/network-error.svg")))

(def NLO (into-array java.nio.file.LinkOption []))

(def status-img-map {:leaf file-image
                     :collapsed collapsed-folder-image
                     :expanded expanded-folder-image})


(defn extension
  "Returns extensions of filename"
  [^java.nio.file.Path file]
  (let [path (str file)]
    (.toLowerCase (.substring path (inc (.lastIndexOf path ".")) (.length path)))))

(defn show-image [^java.nio.file.Path file]
  (-> file image-view stage .show))

(defn show-text [^java.nio.file.Path file]
  (-> file slurp (javafx.scene.control.TextArea.) stage .show))

(defn show-html [^java.nio.file.Path file]
  (-> (jfxnew javafx.scene.web.HTMLEditor :html-text (slurp file))
      stage .show))


(def execute-map {"bmp" show-image
                  "gif" show-image
                  "jpg" show-image
                  "png" show-image
                  "svg" show-image
                  "txt" show-text
                  "html" show-html})


(defprotocol TreeProtocol
  "Protocol to manage items going into a TreeView.  This is for the
  underlying objects, such as File, etc.  So if you have a TreeItem,
  you have to call .getValue first before calling one of these
  methods."
  (get-items [item])
  (leaf? [item])
  (status-image [item state])
  (short-str [item])
  (renamable? [item])
  (delete! [item])
  (rename! [item name])
  (execute! [item]))

(extend-protocol TreeProtocol
  java.net.Inet4Address
  (get-items [a]  (.. FileSystems getDefault getRootDirectories))
  (leaf? [a] (empty? (get-items a)))
  (status-image [a status] computer-image)
  (short-str [a] (.getHostName a))
  (renamable? [item] false)

  java.nio.file.Path
  (get-items [file] (when (Files/isDirectory file NLO)
                      (Files/newDirectoryStream file)))
  (leaf? [file] (not (Files/isDirectory file NLO)))

  (status-image [file status]
    (cond (and (= (.getRoot file) file) (leaf? file)) error-image
          (= (.getRoot file) file) root-image
          :else (status status-img-map)))

  (short-str [file]
    (cond (= (.getRoot file) file) (str file)
          (Files/isDirectory file NLO) (str file)
          :else (str (.getName file (dec (.getNameCount file))))))

  (renamable? [item] true)
  (delete! [file] (println "deleting" file))

  (rename! [file ^String name]
    (let [root (.getRoot file)]
      (cond (= (.getNameCount file) 0) (throw (Exception. "Cannot rename filesystem"))
            (= (.getNameCount file) 1) (.resolve root name)
            :else (let [base (.subpath file 0 (dec (.getNameCount file)))
                        newfile (.resolve (.resolve root base) name)]
                    ;; (Files/move file newfile)
                    newfile))))
  
  (execute! [file]
    ((execute-map (extension file)
                  #(println "Cannot execute" (short-str %))) file))

  
  java.lang.Iterable
  (get-items [it] (tm/children it))
  (leaf? [it] (empty? (tm/children it)))
  (status-image [it status] (status status-img-map))
  (short-str [it] (str (first it)))
  (renamable? [item] false)

  clojure.lang.IPersistentMap
  (get-items [it] (seq it))
  (leaf? [it] (empty? it))
  (status-image [it status] (status status-img-map))
  (short-str [it] (str (keys it)) )
  (renamable? [it] false)

  clojure.lang.MapEntry
  (get-items [it] [(val it)])
  (leaf? [it] false)
  (status-image [it status] (status status-img-map))
  (short-str [it] (name (key it)))
  (renamable? [it] false)
  
  Object
  (leaf? [o] true)
  (status-image [o status] file-image)
  (short-str [o] (str o))
  (renamable? [item] false)

  nil
  (get-items [n] (println "here" ))
  (leaf? [n] true)
  (status-image [n status] (status status-img-map))
  (short-str [n] "nil")
  (renamable? [item] false)
  (execute! [item]))


(def memleaf? (memoize leaf?))

(defn- treeitem-status
  "Returns :leaf, :expanded, or :collapsed from the TreeItem"
  [^TreeItem treeitem]
  (if (.isLeaf treeitem)
    :leaf
    (if (.isExpanded treeitem)
      :expanded
      :collapsed)))

(defn- treeitem-graphic [^TreeItem treeitem]
  "Returns appropriate ImageView for treeitem"
  (image-view
   (status-image (.getValue treeitem) (treeitem-status treeitem))
   default-icon-size))

;; Couldn't get memoization to work for building children and setting
;; list, so just keep track of each item that has gotten itself put
;; into a tree-item.  So the things here have already had their
;; children gotten.
(def memoset (atom #{}))


(defn- tree-item
  "Creates a tree item for use in a TreeView.
  item is the underlying item. images is map of states to images"
  [item]
  (proxy [TreeItem] [item] 
    (getChildren []
      (let [^TreeItem this this]
        (when-not (@memoset item)
          (swap! memoset conj item)
          (.setAll (proxy-super getChildren) (map tree-item (get-items item))))
        (proxy-super getChildren)))
    (isLeaf [] (memleaf? item))))


(def tc-mouse-handler
  (let [last-selected (atom nil)]
    (event-handler [evt]
                   (let [^javafx.scene.input.MouseEvent evt evt]
                     (let [^TreeCell treecell (.getSource evt)
                           treeitem (.getTreeItem treecell)
                           clickcount (.getClickCount evt)]

                       ;; We don't distinguish between leaf and nonleaf selection
                       (when (= (.getButton evt) MouseButton/PRIMARY)
                         ;;(.consume evt)
                         (if (= treecell @last-selected)
                           (do
                             (when (= clickcount 1)
                               (reset! last-selected treecell)
                               (when (not (.isEditing treecell))
                                 (.startEdit treecell)
                                 (.consume evt)))
                             (when (= clickcount 2)
                               (when (.isEditing treecell)
                                 (.cancelEdit treecell))
                               (if (.isLeaf treeitem)
                                 (execute! (.getValue treeitem))
                                 (.setExpanded treeitem true))
                               (.consume evt)))
                           (do ;; different
                             (when (= clickcount 1)
                               (reset! last-selected treecell))
                             (when (= clickcount 2)
                               (println "Won't ever get here")))))
                       
                       ;; Right click popup menu
                       (when (and (= (.getClickCount evt) 1)
                                  (= (.getButton evt) MouseButton/SECONDARY))
                         (let [cm (jfxnew ContextMenu
                                          :items [(jfxnew MenuItem "delete"
                                                          :on-action (println "delete" treeitem ))
                                                  (jfxnew MenuItem "rename"
                                                          :on-action (.startEdit treecell))])]
                           (.setContextMenu treecell cm))))))))



(def tree-cell-factory
  ;; Make a new callback which creates treecells.  Each treecell
  ;; created by the callback must use the same mouse handler
  (callback [treeview]
            (let [tc-initialized (atom false)] ;; holds bool of whether this treecell was used before
              (proxy [TreeCell] []
                (updateItem [item empty?] ;; item refers to the underlying item, not the treeitem
                  (let [^TreeCell this this]
                    (proxy-super updateItem item empty?)
                    (let [^TreeItem treeitem (.getTreeItem this)]
                      (if (or empty?)
                        (doto this
                          (.setText nil)
                          (.setGraphic nil)
                          (.setOnMouseClicked nil))
                        (do
                          (doto this
                            (.setEditable (renamable? item))
                            (.setText (short-str item))
                            (.setGraphic (treeitem-graphic treeitem)))
                          
                          ;; Put a separate mouse handler in each treecell instead of a
                          ;; single mouse handler in the treeview because the single mouse
                          ;; handler in the treeview isn't able to stop the picking at only
                          ;; the treecll; instead it picks the label, the stackpane, and
                          ;; sometimes the cell itself.  Use atom as ugly hack to ensure the
                          ;; handler only gets installed once for this cell.
                          (when-not @tc-initialized
                            (add-event-filter! this MouseEvent/MOUSE_RELEASED  tc-mouse-handler )
                            (reset! tc-initialized true))
                          (Tooltip/install this (Tooltip. (str item))))))))
                (startEdit []
                  (let [^TreeCell this this]
                    (proxy-super startEdit)
                    (when (.isEditing this) ;; Change graphic to graphic + textbox
                      ;; inside on-action, I'm not sure why 'this' refers to the treecell
                      ;; rather than the event handler which is created by the macro
                      (let [tf (jfxnew TextField (.getText this)
                                       :on-action (let [^TextField source (.getSource event)
                                                        new-name (.getText source)
                                                        new-item (rename! (.getItem this) new-name)]
                                                    ;; Need to check for failure before commiting
                                                    (.commitEdit this new-item))
                                       ;; key-released copied from CellUtils.java
                                       ;; seems to be the default cell behavior anyway
                                       :on-key-pressed  (when (= (.getCode ^javafx.scene.input.KeyEvent event)
                                                                 KeyCode/ESCAPE)
                                                          (.cancelEdit this)
                                                          (.consume event)))
                            ng (let [^javafx.scene.image.ImageView oiv (.getGraphic this)
                                     img (.getImage oiv)]
                                 (image-view img default-icon-size))
                            hb (jfxnew HBox :children [ng tf] :alignment Pos/CENTER_LEFT)]
                        (.setText this nil)
                        (.setGraphic this hb)
                        (.requestFocus tf)))))
                (commitEdit [new-value]
                  (let [^TreeCell this this]
                    (proxy-super commitEdit new-value)
                    (println "commit edit" this)
                    (.requestFocus this)))
                (cancelEdit []
                  (let [^TreeCell this this]
                    (proxy-super cancelEdit)
                    (let [item (.getItem this)
                          treeitem (.getTreeItem this)]
                      (.setText this (short-str item))
                      (.setGraphic this (treeitem-graphic treeitem)))))))))

(defn key-handler [treeview]
  (event-handler [evt]
                 (let [^javafx.scene.input.KeyEvent evt evt
                       ^TreeView source (.getSource evt)
                       selection-model (.getSelectionModel source)
                       ^TreeItem treeitem (.getSelectedItem selection-model)]
                   (when (= (.getCode evt) KeyCode/ENTER)
                     (when (and (= (.getSource evt) treeview)
                                (= (.getTarget evt) treeview))
                       (execute! (.getValue treeitem))
                       (.consume evt)))
                   (when (= (.getCode evt) KeyCode/DELETE)
                     (delete! (.getValue treeitem))
                     (.consume evt)))))

(defn treeview
  "Create a TreeView of the given item.  The item must support the Tree protocol"
  ([item]
   (let [tv (jfxnew TreeView (tree-item item)
                    :cell-factory tree-cell-factory
                    :editable true)]
     ;; Filter needs to be on PRESSED because only this happens
     ;; before the default handler which we can't get rid of.  We can
     ;; only intercept and conusme.  The default ENTER puts cell into edit mode
     (add-event-filter! tv KeyEvent/KEY_PRESSED (key-handler tv))
     tv)))


(defn test-treeview [content]
  (let [tv (treeview content)
        st (stage tv)
        p (promise)]
    (.setOnCloseRequest st
                        (event-handler [_]
                                       (deliver p "done!!")))
    (run-now (.show st))
    ;;(printexp @p)
    ;;(shutdown-agents)
    ))


(defn -main []
  (jfxutils.core/app-init)
  ;;(test-treeview [1 2 3 4 5])
  (test-treeview (InetAddress/getLocalHost))
  ;;(test-treeview (bean (javafx.scene.control.Button.)))
  ;;(test-treeview {:a "cat" :b "dog" :c "horse" :d {:aa "meow" :bb "bark!"} :e [1 2 3]})
  (println "done"))

















