(ns toydb.core
  ;;(:require [clojure.reflect :as r])
  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure.pprint])
  (:use [clojure.repl :exclude [root-cause]])
  (:require [clojure.java.io :only [resource]])
  (:require [toydb tableviews edn [db :as db]] )
  (:require [toydb editor grid application-panes application-windows])
  ;;(:require [toydb [menubars :as mb]])
  ;;(:require [toydb.treeview :as tv])
  ;;(:require [toydb.classviewer :as cv])
  ;;(:require [clojure.stacktrace])
  ;;(:require [clojure.java.io])
  ;;(:require [clojure.edn :as edn])


  (:import [java.nio.file FileSystems Files Paths Path LinkOption ]
           [java.net InetAddress]
           [javafx.application Application]
           [javafx.collections ObservableList FXCollections]
           [javafx.stage Stage]
           [javafx.scene Scene Group CacheHint]
           [javafx.scene.shape  Circle Ellipse Line]
           [javafx.scene.canvas Canvas GraphicsContext]
           [javafx.scene.input MouseEvent]
           [javafx.scene.layout BorderPane Pane StackPane AnchorPane HBox VBox Region Priority]
           [javafx.scene.control MenuBar MenuItem Menu Button ColorPicker TableView Slider Label TreeItem TreeView] 
           [javafx.scene Node]
           [javafx.scene.image ImageView]
           [javafx.scene.paint Color Paint]
           [javafx.scene.text Text Font]
           [javafx.concurrent Task]
           [org.dockfx DockEvent DockNode DockPane DockPos DockTitleBar]))


;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)



(defprotocol ApplicationProtocol
  (show-windows! [this])
  (close-windows! [this])
  
 )

;; These will all be atoms or ref (in the case of database) so the
;; application can be passed as an argument to the things it owns.
;; That is, create the applicaiton, then create its members with
;; application as an argument, then swap!  the application members
;; with the actual members.

(defrecord MyApplication [database 
                          editor  
                          panes
                          windows]
  ApplicationProtocol
  (show-windows! [app]
    (doseq [[_ win] @(:windows app)] ;; each item in map is a k,v pair
      (.show win)))
  (close-windows! [this] (close-all-windows)))



;; UI can change the type of an item
(def initial-db-map {:components [{:name "My Rectangle"
                                   :type :polygon
                                   :sides 4
                                   :width1 10.0
                                   :width2 1.5e-5
                                   :width3 1.6e-5
                                   :color1 Color/CADETBLUE
                                   :color2 Color/BISQUE
                                   :pos [5 20]
                                   ;;:visible? true
                                   :owner nil
                                   ;;:clicker (jfxnew Button "hi there")
                                   :alpha 10
                                   :beta 10
                                   :gamma 10
                                   }
                                  {:type :circle
                                   :color Color/ORANGE
                                   :radius 20
                                   :pos [10 30]
                                   ;;:active? false
                                   ;;:button (Button. "Hi Butt2")
                                   :email nil}]})


(defn application []
  (let [width 640
        height 480
        ;;database (ref initial-db-map)
        app (->MyApplication (ref nil) (atom nil) (atom nil) (atom nil))
        editor (toydb.editor/editor app [width height])
        panes {:app-pane (toydb.application-panes/application-pane app [width height]) ;; the DockPane
               :editor-pane (toydb.application-panes/editor-pane app [width height]) ;; goes to DockNode
               :exp-pane (toydb.application-panes/explorer-pane app [200 600] ) ;; goes to DockNode
               :db-pane (toydb.application-panes/db-pane app [200 600] ) ;; goes to Stage
               }
        windows {:debug (toydb.application-windows/debug-window 480 750)
                 :main (toydb.application-windows/main-window app
                                                              [width height]
                                                              (:app-pane panes)
                                                              {(:editor-pane panes) DockPos/CENTER
                                                               (:exp-pane panes) DockPos/LEFT
                                                               (:db-pane panes) DockPos/RIGHT
                                                               })
                 
                 ;;:inspector (toydb.application-windows/inspector-window db [] field-options)
                 ;;:tableview (toydb.application-windows/tableview-window db [] field-options)
                 }]

    ;;(.dock (DockNode. (:db-pane panes) (:app-pane panes)) DockPos/LEFT)
    
    
    ;;(.setOnCloseRequest (:main windows) (eventhandler [_] (close-windows! app)))

    ;; Assign content to currently empty app fields
    (dosync (alter (:database app) map-replace initial-db-map))
    (swap! (:editor app) map-replace editor)
    (swap! (:panes app) map-replace panes)
    (swap! (:windows app) map-replace windows)
    app
    )
  )



(defn -start [myoutpw]
  ;;with-redefs [*out* myoutpw] ;; for some reason doesn't bind properly
  ;;(alter-var-root #'*out* (constantly myoutpw))
  ;; To use field options you pass a map.  Each key must match a
  ;; field in the inspector-view, and the value must be appropriate
  ;; to the field type.  For now only :combo-items is supported, but
  ;; eventually other things such as sliders, validators, etc., can
  ;; be used.
  (let [app (application)]
    (show-windows! app)
    ;;(.show (toydb.bubble/bubble-window 800 600 100))
    ;;(.show (explorer-window 400 400))
    ;;;(.show (cv/class-window 400 400))



))





(defn main []
  (run-now (-start *out*)))

(defn -main []
  ;; This gets called from lein run. The setImplicitExit was
  ;; previously set to false when loading the jfxutils module, which
  ;; allows the JFX thread to keep running while at the REPL.  However
  ;; this is not what we want when running from the command line
  ;; standalone, where we do want it to exit, so we set it back to
  ;; true for JavaFX to be done when the last window closes
  (javafx.application.Platform/setImplicitExit true)
  (main))








