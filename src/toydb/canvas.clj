(ns ^{:doc "Creates a resizable canvas"}
    toydb.canvas
  (require [jfxutils.core :only [showstack]]))

(defn resizable-canvas
  "Returns a proxy of Canvas which is resizable.  The function drawfn,
  if supplied, is called whenever the canvas is resized.  The canvas
  must be placed inside a layout container which resizes its children,
  such as Pane, StackPane, etc."
  [& [drawfn]]
  (proxy [javafx.scene.canvas.Canvas] []
    (isResizable [] true)
    (minWidth [h] 100)
    (minHeight [w] 100)
    (maxWidth [h] Double/MAX_VALUE)
    (maxHeight [w] Double/MAX_VALUE)
    (prefWidth [h] (.minWidth this h))
    (prefHeight [w] (.minHeight this w))
    (resize [w h]
      (when drawfn
        (let [old-size (javafx.geometry.Point2D. (.getWidth this) (.getHeight this))]
          (.setWidth this w)
          (.setHeight this h)
          (drawfn this old-size))))))

