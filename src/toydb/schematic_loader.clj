(ns toydb.schematic-loader)

(defprotocol IJFXConstruct
  (jfx [this]))

(extend-protocol IJFXConstruct
  toydb.edn.SchGroup
  (jfx [this]
    (javafx.scene.Group. (into-array (map jfx (:children this)))))

  toydb.edn.SchLine
  (jfx [this]
    (javafx.scene.shape.Line. (:x0 this) (:y0 this) (:x1 this) (:y1 this)))

  toydb.edn.SchCircle
  (jfx [this]
    (javafx.scene.shape.Circle. (:x this) (:y this) (:r this)))

  toydb.edn.SchText
  (jfx [this]
    (javafx.scene.text.Text. (:x this) (:y this) (:txt this))))



