(ns toydb.docksdemo
  (:require [clojure.java.io :as io]
            [docks.core :as docks]
            [jfxutils.core :refer [add-children! event-handler jfxnew
                                   run-now run-later set-items! set-list!
                                   set-menus! stage]])
  (:import (javafx.application Application)
           (javafx.scene Scene)
           (javafx.scene.control Label Menu MenuBar MenuItem Tab
                                 TabPane TableColumn TableView TextArea
                                 TreeItem TreeView)
           (javafx.scene.image Image)
           (javafx.scene.layout BorderPane)
           (javafx.stage Stage))
  (:gen-class))

(defn generate-random-tree []
  (let [root (TreeItem.)
        tree-view (TreeView. root)]
    (.setShowRoot tree-view false)
    (doseq [i (range (+ 4 (rand-int 8)))]
      (let [tree-item (TreeItem. (str "Item " i))]
        (add-children! root [tree-item])
        (doseq [j (range (+ 2 (rand-int 4)))]
          (add-children! tree-item [(TreeItem. (str "Child " j)) ]))))
    tree-view))


(defn -start []
  (let [tabs (TabPane.)
        html-editor (run-now (javafx.scene.web.HTMLEditor.))
        table-view (TableView.)
        dock-image (Image. (.toExternalForm (io/resource "docknode.png")))
        new-text-node (fn [num] (docks/node (TextArea. (slurp (io/resource "loremipsum.txt")))
                                            (str "Text " num)))
        edit-base (docks/base :center (map new-text-node (range 3)))
        tn1 (docks/node (generate-random-tree) "Tree Node1" dock-image)
        tn2 (docks/node (generate-random-tree) "Tree Node2" dock-image)
        tv (docks/node table-view "Table node")
        center-base (docks/node edit-base)
        root-dock-pane (docks/base :center center-base
                                   :left tn2
                                   :right tn1
                                   :bottom tv)]
    (.setPrefSize center-base 300 600)
    (.setPrefSize tv 300 100)


    (set-list! tabs :tabs [(Tab. "Tab1" html-editor) (Tab. "Tab2") (Tab. "Tab3")])
    (set-list! table-view :columns (map #(TableColumn. %) ["A" "B" "C"]))

    
    (let [st (stage (jfxnew BorderPane
                            :center root-dock-pane
                            :bottom (Label. "Bottom")
                            :top (jfxnew MenuBar
                                         :menus [(jfxnew Menu "File"
                                                         :items [(jfxnew MenuItem "New Tab"
                                                                         :on-action (event-handler [e] (docks/dock (new-text-node 0) edit-base :center)))])
                                                 (jfxnew Menu "Edit")] )) )]
      (run-now (.setTitle st (name :DockFX))
               (try (.setHtmlText html-editor (slurp (io/resource "readme.html")))
                    (catch java.io.IOException e
                      (.printStackTrace e)))
               (Application/setUserAgentStylesheet Application/STYLESHEET_MODENA)
               (docks/init-style)))))




(defn main []
  (jfxutils.core/app-init)
  (-start))

(defn -main []
  (jfxutils.core/app-init)
  (-start))



