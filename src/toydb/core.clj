(ns toydb.core
  (:use [clojure
         [pprint]
         [repl :exclude [root-cause]]
         ;;edn
         ])
  (:use [jfxutils.core :exclude [-main]])

  (:require [clojure.java.io :as io])
  (:require [docks.core :as docks])
  (:require [toydb
             ;;[db :as db]
             ;;[treeview]q
             ;;[tableviews]
             ;;[classviewer :as cv]
             [editor :as editor]
             [grid :as grid]
             [application-panes :as panes]
             [application-windows :as windows]
             [menubars :as mb]])

  ;;(:require 
  ;;(:require [toydb.treeview :as tv])
  ;;(:require [toydb.classviewer :as cv])
  ;;(:require [clojure.edn :as edn])

  (:import [java.net InetAddress]
           [javafx.application Application]
           [javafx.collections ObservableList FXCollections]
           [javafx.concurrent Task]
           [javafx.scene Node Scene Group CacheHint]
           [javafx.scene.control MenuBar MenuItem Menu Button ColorPicker TableView Slider Label TreeItem TreeView TextArea] 
           [javafx.scene.image ImageView]
           [javafx.scene.input MouseEvent]
           [javafx.scene.layout BorderPane Pane StackPane AnchorPane HBox VBox Region Priority]
           [javafx.scene.paint Color Paint]
           [javafx.scene.shape Circle Ellipse Line]
           [javafx.scene.text Text Font]
           [javafx.stage Stage]
           [java.nio.file FileSystems Files Paths Path ]))


;;(set! *warn-on-reflection* false)
;;(set! *unchecked-math* false)



(defprotocol ApplicationProtocol
  (show-windows! [this])
  (close-windows! [this]))

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
  (close-windows! [this] (jfxutils.core/close-all-stages))) ;; not sure why I have to qualify



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

(defn main-stage
  "Place center-node in center of border-pane, then return new window
  with border-pane as scene."
  [border-pane center-node & [[width height]]]
  (.setCenter border-pane center-node)
  (if (and width height)
    (stage border-pane [width height])
    (stage border-pane)))

(defn application [out]
  (docks/set-docking-system! :DockFX)
  (let [width 1280
        height 800
        app (->MyApplication (ref nil) (atom nil) (atom nil) (atom nil))
        editor (editor/editor app [width height])
        ;; panes managed by top level application; does not include editor base
        panes {:app-pane (panes/application-pane app) ;;[width height] ;; just a border pane with menubar, etc.
               :exp-pane (panes/explorer-pane app )
               :db-pane (panes/db-pane app )}
        
        windows {;;:debug (stage (console-scene *out*) [480 750])
                 :main (main-stage (:app-pane panes)
                                   (docks/base :center (doto (docks/node (:top-pane editor) nil)
                                                         (.setPrefSize (- width 200) height))
                                               :left (doto (docks/node (:exp-pane panes) "Explorer pane")
                                                       (.setPrefWidth 100))
                                               :right (doto (docks/node (:db-pane panes) "database")
                                                        (.setPrefWidth 100))))}]

        
    (.setOnCloseRequest (:main windows) (event-handler [_] (close-windows! app)))

    ;; Assign content to currently empty app fields
    (dosync (alter (:database app) map-replace initial-db-map))
    (swap! (:editor app) map-replace editor)
    (swap! (:panes app) map-replace panes)
    (swap! (:windows app) map-replace windows)
    app))


(defn -start [print-writer]
  ;; Already in FX thread
  (let [app (application print-writer)]
    (show-windows! app)
    ;;(.show (toydb.bubble/bubble-window 800 600 100))
    ;;(.show (explorer-window 400 400))
    ;;(.show (cv/class-window 400 400))
    ;;:inspector (toydb.application-windows/inspector-window db [] field-options)
    ;;:tableview (toydb.application-windows/tableview-window db [] field-options)

    (docks/init-style)
    ;;app
    ))





(defn main
  "Gets called from REPL"
  []
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






