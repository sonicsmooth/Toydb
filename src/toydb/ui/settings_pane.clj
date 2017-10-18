(ns toydb.ui.settings-pane
  (:use [jfxutils.core :exclude [-main]]
        [toydb.ui.grid-settings-pane :as gsp :exclude [-main]])
  (:import [javafx.event EventHandler]
           [javafx.fxml FXMLLoader]
           [javafx.scene.control Tab]))


(defn settings-pane [uid]
  (let [root (load-fxml-root "SettingsPane.fxml")
        schematic-gsp (gsp/grid-settings-pane (join-hyph uid "schematic"))
        layout-gsp (gsp/grid-settings-pane (join-hyph uid "layout"))]
    (set-tabs! (lookup root "settings-tab-pane")
               [(Tab. "Schematic Grid" schematic-gsp)
                (Tab. "Layout Grid" layout-gsp)])
    root))

;; Could not find a way to get "this" namespace
;; other than finding a known-existing variable,
;; such as the show function, and getting its namespace
;; So just hard code this namespace here
(defn show []
  (if-let [the-var (resolve `settings-stage)]
    (.show (var-get the-var))
    (let [st (stage (settings-pane nil))]
      (intern 'toydb.ui.settings-pane 'settings-stage st )
      (.show st))))

;; Probably not needed because settings pane should reflect
;; values in main settings atom.
(defn reset-pane! []
  (ns-unmap 'toydb.ui.settings-pane 'settings-stage))


(defn main []
  (jfxutils.core/app-init false)
  (stage (settings-pane nil)))

(defn -main []
  (jfxutils.core/app-init false)
  (stage (settings-pane nil)))














