(ns toydb.core
  (:gen-class)
  (:require [jfxutils.core :refer :all :exclude [-main]])
  #_  (:use  [clojure
              [pprint]
              [repl :exclude [root-cause]]
              ;;edn
              ])

  #_(:require [clojure.java.io :as io])
  (:require [docks.core :as docks]
            [toydb.editor.editor :as editor]
            [toydb.app.application-panes :as panes])
  #_(:require [toydb
               ;;[db :as db]
               ;;[treeview]
               ;;[tableviews]
               ;;[classviewer :as cv]
               ;;[editor :as editor]
               [grid :as grid]
               ;;[application-panes :as panes]
               ;;[application-windows :as windows]
               ;;[menubars :as mb]
               ;;[docksdemo]
               ])

  ;;(:require 
  ;;(:require [toydb.treeview :as tv])
  ;;(:require [toydb.classviewer :as cv])
  ;;(:require [clojure.edn :as edn])

  (:import ;;[java.net InetAddress]
   ;;[javafx.application Application]
   ;;[javafx.collections ObservableList FXCollections]
   ;;[javafx.concurrent Task]
   ;;[javafx.scene Node Scene Group CacheHint]
   [javafx.scene.control ;;MenuBar MenuItem Menu
    Button
;;    #_ColorPicker #_TableView #_Slider #_Label #_TreeItem TreeView TextArea
    ] 
   ;;[javafx.scene.image ImageView]
   ;;[javafx.scene.input MouseEvent]
   [javafx.scene.layout
    BorderPane
    ;;Pane StackPane AnchorPane HBox VBox Region Priority
    ]
   [javafx.scene.paint Color Paint]
   ;;[javafx.scene.shape Circle Ellipse Line]
   ;;[javafx.scene.text Text Font]
   ;;[javafx.stage Stage]
   ;;[java.nio.file FileSystems Files Paths Path ]
   ))


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
#_(def initial-db-map {:components [{:name "My Rectangle"
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
  (let [width 1280
        height 800
        app (map->MyApplication {:database (ref nil),
                                 :editor (atom nil),
                                 :panes (atom nil),
                                 :windows (atom nil)})
        editor (editor/editor app )
        ;; panes managed by top level application; does not include editor base
        panes { ;;:app-pane (panes/application-pane app) ;; just a border pane with menubar, etc.
               :exp-pane (panes/explorer-pane app )
               ;;:db-pane (panes/db-pane app)
               }
        windows { ;;:debug (stage (console-scene *out*) [480 750])
                 :main (main-stage (BorderPane.) ;;(:app-pane panes)
                                   (docks/base
                                    :center (doto (docks/node (:top-pane editor) nil)
                                              (.setPrefSize (- width 200) height))
                                    :left (doto (docks/node (:exp-pane panes) "Explorer pane")
                                            (.setPrefWidth 100))
                                    #_:right #_(doto (docks/node (:db-pane panes) "database")
                                                 (.setPrefWidth 100))))}]

        
    (.setOnCloseRequest (:main windows)
                        (event-handler [_]
                                       (close-windows! app)
                                       (shutdown-agents)))

    ;; Assign content to currently empty app fields
    ;;(dosync (alter (:database app) (fn [a b] b) initial-db-map))
    ;;(reset! (:editor app) editor)
    ;;(reset! (:panes app) panes)
    ;;(reset! (:windows app) windows)
    ;;app
    ))


(defn -start [print-writer]
  (jfxutils.core/app-init)
  (let [app (application print-writer)

        ]
    ;;(show-windows! app)
    ;;(.show (toydb.bubble/bubble-window 800 600 100))
    ;;(.show (explorer-window 400 400))
    ;;(.show (cv/class-window 400 400))
    ;;:inspector (toydb.application-windows/inspector-window db [] field-options)
    ;;:tableview (toydb.application-windows/tableview-window db [] field-options)

    (run-now (docks/init-style))
    ;;app
    ))





(defn main
  "Gets called from lein repl and cider"
  []
  (jfxutils.core/app-init)
  (-start *out*))

(defn -main
  "Gets called from lein run and uberjar"
  []
  (jfxutils.core/app-init)
  (jfxutils.core/set-exit true)
  (-start *out*))
















