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
            [toydb.app.application-panes :as panes]
   )
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


(defn application [out]
  (let [width 1280
        height 800
        app (map->MyApplication {:database (ref nil),
                                 :editor (atom nil),
                                 :panes (atom nil),
                                 :windows (atom nil)})
        editor (editor/editor app)
        editor-pane (:top-pane editor)
        editor-behaviors (:behaviors editor)
        app-pane (apply panes/application-pane
                        app
                        editor-behaviors) ;; just a border pane with menubar, etc.
        exp-pane (panes/explorer-pane app )
        db-pane  (panes/db-pane app)
        main-pane (BorderPane. app-pane)
        main-stage (stage main-pane [width height])
        center-docknode (doto (docks/node editor-pane nil)  (.setPrefWidth (- width 200)))
        left-docknode (doto (docks/node exp-pane "Explorer pane") (.setPrefWidth 100))
        right-docknode (doto (docks/node db-pane  "database") (.setPrefWidth 100))
        main-dock (docks/base
                   :center center-docknode
                   :left left-docknode 
                   :right right-docknode)]
    (run-now (.setCenter app-pane main-dock))


    (.setOnCloseRequest main-stage (event-handler [_] (close-windows! app)
                                                  ;;(shutdown-agents)
                                                  ))

    ;; Assign content to currently empty app fields
    ;;(dosync (alter (:database app) (fn [a b] b) initial-db-map))
    ;;(reset! (:editor app) editor)
    ;;(reset! (:panes app) panes)
    ;;(reset! (:windows app) windows)
    app
    ))


(defn -start [print-writer]
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
  (jfxutils.core/set-exit false)
  (-start *out*))

(defn -main
  "Gets called from lein run and uberjar"
  []
  (jfxutils.core/app-init)
  (jfxutils.core/set-exit true)
  (-start *out*))
