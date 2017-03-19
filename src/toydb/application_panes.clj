(ns toydb.application-panes
  (:use [jfxutils.core])

  (:require [toydb
             [tableviews]
             [treeview]
             [canvas :as canvas]
             [editor :as editor]
             [menubars :as mb]])
  (:import ;;[java.nio.file FileSystems Files Paths Path LinkOption ]
   ;;[javafx.collections ObservableList FXCollections]
   ;;[javafx.scene Node]
   ;;[javafx.scene.canvas Canvas GraphicsContext]
   ;;[javafx.scene.control MenuBar MenuItem Menu Button ColorPicker TableView Slider Label TreeItem TreeView]
   ;;[javafx.scene.image ImageView]
   ;;[javafx.scene.input MouseEvent]
   ;;[javafx.stage Stage]
   [java.net InetAddress]
   [javafx.application Application]
   [javafx.geometry Pos]
   [javafx.scene Scene Group]
   [javafx.scene.control Slider Label Button]
   [javafx.scene.layout BorderPane Pane StackPane AnchorPane HBox VBox Region Priority]
   [javafx.scene.paint Color Paint]
   [javafx.scene.shape Circle Ellipse Line]
   [javafx.scene.text Text Font]))



;; populates as you go
(defn explorer-pane
  "Returns tree-view immediately.  Expects
  tree-view to launch background threads to populate pane with
  explorer items as they come in."
  [parent-app & [width height]]
  (toydb.treeview/tree-view (.. InetAddress getLocalHost) false))


(defn db-pane [parent-app & [width height]]
  (toydb.treeview/tree-view @(:database parent-app) true))

#_(defn draw-mm-handler [canvas specatom]
  (event-handler [e]
                (swap! specatom assoc
                       :unit :mm
                       :major-units [(toydb.grid/mm 1) (toydb.grid/mm 1)])
                (toydb.grid/draw-grid! canvas @specatom)))

#_(defn draw-inch-handler [canvas specatom]
  (event-handler [e]
                (swap! specatom assoc
                       :unit :inch
                       :major-units [(toydb.grid/inch 1) (toydb.grid/inch 1)])
                (toydb.grid/draw-grid! canvas @specatom)))


(defn mouse-printer [label-node txt]
  (event-handler [mouse-event]
                (let [mx (int (.getX mouse-event))
                      my (int (.getY mouse-event))]
                  (.setText ^Label label-node (str txt " X:" mx ", Y:" my)))))

(defn application-pane
  "Creates BorderPane with menus at top, and DockPane in center where
  everything attaches.  No DockNodes are created here."
  [& [parent-app [width height]]]
  (let [iv1 (image-view "docknode.png")
        iv2 (image-view "docknode.png")
        iv3 (image-view "docknode.png")
        open-item (mb/menu-item :id "menu-file-open", :text "Open", :icon "icons/tango-icons-0.8.90/scalable/actions/document-open.svg")
        new-item  (mb/menu-item :id "menu-file-new",  :text "New",  :icon "icons/tango-icons-0.8.90/scalable/actions/window-new.svg")        
        save-item (mb/menu-item :id "menu-file-save", :text "Save", :icon "icons/tango-icons-0.8.90/scalable/actions/document-save.svg")
        saveas-item (mb/menu-item :id "menu-file-save-as", :text "Save As...", :icon "icons/tango-icons-0.8.90/scalable/actions/document-save-as.svg")
        submenu-item (mb/menu :name "Submenu", :icon "icons/tango-icons-0.8.90/scalable/actions/list-add.svg"
                              :items [(mb/menu-item :id "menu-file-sub-1", :text "Sub1")
                                      (mb/menu-item :id "menu-file-sub-2", :text "Sub2")])
        separator-item (mb/separator-menu-item)
        close-item (mb/menu-item :id "menu-file-exit", :text "Exit", :icon "icons/tango-icons-0.8.90/scalable/actions/system-log-out.svg")
        app-menu-bar (mb/menu-bar [(mb/menu :text "File"
                                            :items [open-item new-item save-item saveas-item submenu-item separator-item close-item])
                                   (mb/menu :text "Settings"
                                            :items [(mb/menu-item :id "settings-grid"
                                                                  :text "Grid"
                                                                  :icon "icons/tango-icons-0.8.90/scalable/categories/preferences-desktop.svg")])])

        pane-label (Label. "App status bar")
        mouse-label (Label.)
        stage-label (Label.)
        app-status-bar (mb/status-bar [pane-label mouse-label stage-label])

        pane (jfxnew BorderPane ;; center is set in (main-window...)
                     :top app-menu-bar
                     :bottom app-status-bar
                     :background (gradient-background :horizontal (Color/web "#222222") (Color/web "#cccccc")))]

    (when width (.setPrefWidth pane width))
    (when height (.setPrefHeight pane height))
    pane))





       
      
