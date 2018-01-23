(ns toydb.ui.SettingsPane
  (:require [jfxutils.core :as jfxc :exclude [-main]]
            [toydb.ui.GridSettingsPane :as gsp :exclude [-main]])
  (:import [javafx.event EventHandler]
           [javafx.fxml FXMLLoader]
           [javafx.scene.control Tab]))


;; The way the (show) function works basically makes this a Singleton
;; A SettingsPane is owned/operated by the top level application, but
;; the contents of the pane are owned by each individual module.
;; Therefore, each module is responsible for providing its panes up to
;; the top layer as a return value when it is created

(defn settings-pane [uid]
  "Load up tabbed settings pane.  Returns map with both the node and
  the states."
  (let [root (jfxc/load-fxml-root "SettingsPane.fxml")]
    #_(jfxc/set-tabs! (jfxc/lookup root "settings-tab-pane")
                    [(Tab. "Schematic Grid" sch-root)
                     (Tab. "Layout Grid" lay-root)])
    #_{:root root
     :schematic-state sch-state
     :layout-state lay-state}
    root))

;; Could not find a way to get "this" namespace
;; other than finding a known-existing variable,
;; such as the show function, and getting its namespace
;; So just hard code this namespace here
(defn show
  "Show the settings pane given in arg, or the one in this
  namespace. Create a var called settings-stage if it doesn't exist
  already, then call show.  Calling (show pane) a second time will
  show the previous stage, so if you want to change the pane, call
  reset-pane! first."
  #_([]
   (if-let [the-var (resolve `settings-stage)]
     (.show (var-get the-var))
     (let [st (jfxc/stage (settings-pane nil))]
       (intern 'toydb.ui.SettingsPane 'settings-stage st )
       (.show st))))
  ([pane]
   (if-let [the-var (resolve `settings-stage)]
     (.show (var-get the-var))
     (let [st (jfxc/stage pane)]
       (intern 'toydb.ui.SettingsPane 'settings-stage st )
       (.show st)))))

;; Probably not needed because settings pane should reflect
;; values in main settings atom.
(defn reset-pane! []
  (ns-unmap 'toydb.ui.SettingsPane 'settings-stage))


#_(defn main []
  (jfxutils.core/app-init)
  (jfxc/stage (:root (settings-pane nil))))

#_(defn -main []
  (jfxutils.core/app-init)
  (jfxc/stage (:root (settings-pane nil))))














