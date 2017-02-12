(ns toydb.menubars
  (:use [jfxutils.core :exclude [-main]])
  (:require [clojure.java.io :as io])
  (:import [java.nio.file Path]
           [javafx.stage Stage FileChooser DirectoryChooser]
           [javafx.geometry Pos]
           [javafx.scene Group]
           [javafx.scene.control Menu MenuBar MenuItem SeparatorMenuItem ToolBar Button]
           [javafx.scene.layout HBox VBox Background BackgroundFill ]))





(defn menu-item [& {:keys [name icon action]}]
  (jfxnew MenuItem
          :text name
          :graphic (image-view icon)
          :on-action (eventhandler [_] (action))))

(defn separator-menu-item []
  (SeparatorMenuItem.))

(defn menu [& {:keys [name icon items]}]
  (jfxnew Menu
          :text name
          :graphic (image-view icon)
          :items items))

(defn menu-bar [menus]
  (jfxnew MenuBar :menus menus))

(defn status-bar [items]
  (jfxnew HBox
          :spacing 10
          :children items
          :alignment Pos/CENTER_LEFT
          :style "-fx-background-color: gainsboro"))

(defn open-file-dialog []
  (println "here opening")
  (let [fc (FileChooser.)]
    (.showOpenDialog fc (Stage.))))

(defn tool-bar [nodes]
  (jfxnew ToolBar :items nodes))
