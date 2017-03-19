(ns toydb.menubars
  (:use [jfxutils.core :exclude [-main]])
  (:require [clojure.java.io :as io])
  (:import [java.nio.file Path]
           [javafx.stage Stage FileChooser DirectoryChooser]
           [javafx.geometry Pos]
           [javafx.scene Group]
           [javafx.scene.control Menu MenuBar MenuItem SeparatorMenuItem ToolBar Button]
           [javafx.scene.layout HBox VBox Background BackgroundFill ]))





(defn menu-item [& {:keys [id text icon action]}]
  (let [item (MenuItem.)]
    (when id (.setId item id))
    (when text (.setText item text))
    (when icon (.setGraphic item (image-view icon)))
    (when action (.setOnAction item (event-handler [_] (action))))
    item))

(defn separator-menu-item []
  (SeparatorMenuItem.))

(defn menu [& {:keys [id text icon items]}]
  (let [m (Menu.)]
    (when id (.setId m id))
    (when text (.setText m text))
    (when icon (.setGraphic m (image-view icon)))
    (when items (add-items! m items))
    m))

(defn menu-bar [menus]
  (let [mb (MenuBar.)]
    (when menus (add-menus! mb  menus))
    mb))

(defn status-bar [items]
  (let [sb (jfxnew HBox :pref-height 30, :spacing 10,
                   :alignment Pos/CENTER_LEFT
                   :style "-fx-background-color: gainsboro")]
    (when items (set-children! sb items))
    sb))

(defn open-file-dialog []
  (println "here opening")
  (let [fc (FileChooser.)]
    (.showOpenDialog fc (Stage.))))

(defn tool-bar [nodes]
  (let [tb (jfxnew ToolBar)]
    (when nodes (add-items! tb nodes))
    tb))










