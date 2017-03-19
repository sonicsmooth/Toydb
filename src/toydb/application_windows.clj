(ns toydb.application-windows
  (:use [jfxutils.core :exclude [main-]]
        [toydb.application-panes :as panes]
        [docks.core :as docks])
  (:import ;;[java.net InetAddres]
           ;;[java.nio.file FileSystems Files Paths Path LinkOption ]
           ;;[javafx.application Application]
           ;;[javafx.scene Node]
           ;;[javafx.scene.canvas Canvas GraphicsContext]
           ;;[javafx.scene.image ImageView]
           ;;[javafx.scene.input MouseEvent]
           ;;[javafx.scene.paint Color Paint]
           ;;[javafx.scene.shape  Circle Ellipse Line]
           ;;[javafx.scene.text Text Font]
           [javafx.collections ObservableList FXCollections]
           [javafx.scene Scene ]
           [javafx.scene.control MenuBar MenuItem Menu Button ColorPicker TableView Slider Label TreeItem TreeView] 
           [javafx.scene.layout BorderPane Pane StackPane VBox]
           [javafx.stage Stage]))

#_(defn debug-window [& [width height]]
  (let [[myoutpw console-scene] (console-scene )]
    ;;(alter-var-root #'*out* (constantly myoutpw))
    (jfxnew Stage
            :scene console-scene
            :width (or width 480) :height (or height 750)
            :x 1200 :y 100)))


(defn inspector-window [db init-accesspath field-options]
  (let [iv (toydb.tableviews/inspector-view db
                                            :accesspath init-accesspath
                                            :field-options field-options)
        scene (jfxnew Scene (jfxnew StackPane :children [iv]))
        default-change-listener (fn [s]  (change-listener
                                           [oldval newval]
                                           (println s "changed to" newval)))]
    ;;(.add (.getStylesheets scene) (.toExternalForm (clojure.java.io/resource "stylesheet.css")))
    (def selectmodel (.getSelectionModel ^TableView iv))
    (def focusmodel (.getFocusModel ^TableView iv))
    (add-listener! selectmodel :selected-index (default-change-listener "selected-index"))
    (add-listener! selectmodel :selected-item (default-change-listener "selected-item"))
    (add-listener! focusmodel :focused-index (default-change-listener "focused-index"))
    (add-listener! focusmodel :focused-cell (default-change-listener "focused-cell"))

    (jfxnew Stage
            :scene scene
            :width 450
            :x 100 :y 100
            :on-close-request (remove-handler [db] [iv]))))

(defn tableview-window [db init-accesspath field-options]
  (let [tv (toydb.tableviews/table-view db
                                        :accesspath init-accesspath
                                        :field-options field-options)
        pane (jfxnew VBox)
        scene (jfxnew Scene pane)
        menu (jfxnew Menu "File")
        menubar (jfxnew MenuBar [menu])]

    
    ;;(.add (.getStylesheets scene) (.toExternalForm (clojure.java.io/resource "stylesheet.css")))
    
    (.addAll ^ObservableList (.getItems menu) (observable
                                               [(jfxnew MenuItem "Open" :on-action (event-handler [e] (println "opening")))
                                                (jfxnew MenuItem "Exit" :on-action (event-handler [e] (javafx.application.Platform/exit)))]))

    (.addAll ^ObservableList (.getChildren pane) (observable [menubar tv])) ;; can convert to add-children
    (jfxnew Stage
            :scene scene
            :width 850
            :x 250 :y 100
            :on-close-request (remove-handler [db] [tv]))))



#_(defn mouse-printer [label-node txt]
  (event-handler [mouse-event]
                (let [mx (int (.getX mouse-event))
                      my (int (.getY mouse-event))]
                  (.setText ^Label label-node (str txt " X:" mx ", Y:" my)))))


#_(defn explorer-window [& [width height]]
  (let [stage (jfxnew Stage
                      :scene (Scene. (explorer-pane width height))
                      :on-close-request (event-handler [_] (close-all-windows)))]
    (when width (.setWidth stage width))
    (when height (.setHeight stage height))
    stage))




















