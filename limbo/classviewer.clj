(ns toydb.classviewer
  ;;(:require [clojure.reflect :as r])
  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure.pprint])
  (:use [clojure.repl :exclude [root-cause]])
  (:use [clojure.set :as set])
  (:require [clojure.java.io :only [resource]])
  ;;(:require [toydb.tableviews])
  ;;(:require [toydb.bubble])
  (:require [toydb.grid])
  ;;(:require [toydb.edn])
 ;; (:require [toydb.db :as db])
  (:require [toydb.menubars :as mb])
  ;;(:require [toydb.treeview :as tv])
  (:require [toydb.treemerge :as tm])
  (:require [clojure.stacktrace])
  (:require [clojure.java.io])
  (:require [clojure.reflect :as r])
  (:require [clojure.edn :as edn])



  (:import [java.net InetAddress]
           [javafx.animation AnimationTimer FadeTransition FillTransition Interpolator KeyFrame KeyValue
            ParallelTransition PathTransition PauseTransition RotateTransition ScaleTransition SequentialTransition StrokeTransition Timeline Transition TranslateTransition Animation$Status PathTransition$OrientationType ]
           [javafx.application Application HostServices Platform Preloader Preloader$ErrorNotification Preloader$ProgressNotification Preloader$StateChangeNotification ConditionalFeature Preloader$StateChangeNotification$Type]
           [javafx.beans InvalidationListener Observable WeakListener WeakInvalidationListener DefaultProperty NamedArg]
           [javafx.collections ObservableList FXCollections]
           [javafx.stage Stage]
           [javafx.scene Scene Group CacheHint]
           [javafx.scene.shape Circle Ellipse Line]
           [javafx.scene.canvas Canvas GraphicsContext]
           [javafx.scene.input MouseEvent]
           [javafx.scene.layout BorderPane Pane StackPane AnchorPane HBox VBox Region Priority]
           [javafx.scene.control MenuBar MenuItem Menu Button ColorPicker TableView Slider Label TreeItem TreeView Cell] 
           [javafx.scene Node]
           [javafx.scene.paint Color]
           [javafx.scene.image ImageView]
           [javafx.scene.text Text Font]
           [javafx.concurrent Task]
           [org.dockfx DockEvent DockNode DockPane DockPos DockTitleBar]))

(def ALL-CLASSES (map second (ns-imports *ns*)))

#_(def ALL-CLASSES [ ;; javafx.animation
                  AnimationTimer FadeTransition FillTransition Interpolator KeyFrame KeyValue
                  ParallelTransition PathTransition PauseTransition RotateTransition ScaleTransition
                  SequentialTransition StrokeTransition Timeline Transition TranslateTransition
                  Animation$Status PathTransition$OrientationType

                  ;; javafx.application
                  Application HostServices Platform Preloader Preloader$ErrorNotification
                  Preloader$ProgressNotification Preloader$StateChangeNotification

                  ;; javafx.beans
                  WeakInvalidationListener

                  ;; javafx.bean.binding
                  Bindings BooleanBinding BooleanExpression

                  
                  ])



(defn make-tree-from-obj [obj]
  (tm/make-tree-from-chain (fxparents obj)))

(defn init-trees
"Returns a list of independent trees, one from each obj"
  [& objs]
  (map make-tree-from-obj objs))

(defn merge-hierarchies
  "Joins two objects' hierarchies"
  [ & objs]
  (let [trees (apply init-trees objs)]
    (reduce tm/tree-merge trees)))

(def ALL-CLASS-HIERARCHY (apply merge-hierarchies ALL-CLASSES))


 
(defn tree-pane
  "Make generic stackpane with a new tree-item with root"
  [root & [width height]]
  (let [pane (jfxnew StackPane :children [root])]
    (when width (.setPrefWidth pane width))
    (when height (.setPrefHeight pane height))
    pane))


#_(defn class-pane
  "Creates stackpane with tree-view and returns immediately.  Expects
  tree-view to Launch background threads to populate pane with
  explorer items as they come in."
  [& [width height]]
  (tree-pane (tv/tree-view ALL-CLASS-HIERARCHY false)
             width height))

#_(defn class-window [& [width height]]
  (let [stage (jfxnew Stage
                      :scene (Scene. (class-pane width height))
                      :on-close-request (event-handler [_] (close-all-stages)))]
    (when width (.setWidth stage width))
    (when height (.setHeight stage height))
    stage))

(defn application-pane
  "Creates BorderPane with main grid, menu bar, toolbar, left and right buttons, and status bar"
  [width height]
  (let [app-tool-bar (mb/tool-bar [(jfxnew Button "Save to PDF"  :on-action (run-later (println "Save to PDF")))])
        pane-label (Label.)
        mouse-label (Label.)
        stage-label (Label.)
        app-status-bar (mb/status-bar [pane-label mouse-label stage-label ])
        ;;background (gradient-background height (Color/web "#232526") (Color/web "#414345"))
        pane (jfxnew BorderPane
                     ;;:center grid-canvas
                     :top (jfxnew VBox :children [app-tool-bar])
                     :bottom app-status-bar
                     ;;:background background
                     )]
    pane))

(defn application-window [width height]
  (let [pane (application-pane width height)
        window (jfxnew Stage 
                       :scene (Scene. pane)
                       :width width
                       :height height
                       :on-close-request (event-handler [_] (close-all-stages)))]
    window))

(defn -start [myoutpw]
  (let [dockpane (DockPane.)
        app-pane  (application-pane 600 400)
        iv1 (image-view "icons/tango-icon-theme-0.8.90/scalable/status/weather-clear.svg")
        iv2 (image-view "icons/tango-icon-theme-0.8.90/scalable/status/weather-clear.svg")
        iv3 (image-view "icons/tango-icon-theme-0.8.90/scalable/status/weather-clear.svg")
        app-dock (DockNode. app-pane "Grid" iv1)
        #_exp-dock #_(DockNode. (class-pane 100 100) "Explorer" iv2)]
    ;;(.setDockTitleBar app-dock nil)
    (.dock app-dock dockpane DockPos/TOP)
    ;;(.dock exp-dock dockpane DockPos/LEFT)
    (.show (jfxnew Stage
                   :scene (Scene. dockpane 800 600)))))





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








