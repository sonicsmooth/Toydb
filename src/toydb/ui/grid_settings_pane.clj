(ns toydb.ui.grid-settings-pane
  (:use [jfxutils.core :exclude [-main]])
  )


(defn update-sliders!
  "Set up ticks, snap"
  [root name]
  (let [minor-gpm (lookup root (join-hyph name "sl-minor-gpm"))
        zoom-ppu (lookup root (join-hyph name "sl-zoom-ppu"))
        major-glw (lookup root (join-hyph name "sl-major-grid-line-width"))
        major-gdw (lookup root (join-hyph name  "sl-major-grid-dots-width"))
        minor-glw (lookup root (join-hyph name  "sl-minor-grid-line-width"))
        minor-gdw (lookup root (join-hyph name "sl-minor-grid-dots-width"))]
    (doto minor-gpm
      (.setMin 0)
      (.setMax 20)
      (.setMajorTickUnit 5)
      (.setMinorTickCount 4)
      (.setShowTickMarks true)
      (.setShowTickLabels true))
    (doto zoom-ppu
      (.setMin 5)
      (.setMax 200)
      (.setMajorTickUnit 50)
      (.setMinorTickCount 10)
      (.setShowTickMarks true)
      (.setShowTickLabels true))
    (doto major-glw
      (.setMin 0)
      (.setMax 5)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto major-gdw
      (.setMin 0)
      (.setMax 5)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto minor-glw
      (.setMin 0)
      (.setMax 5)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto minor-gdw
      (.setMin 0)
      (.setMax 5)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    
    root))

(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (let [ids ["cb-enable-minor-grid"
             "cb-enable-major-grid"
             "tf-major-grid-spacing-mm"
             "tf-major-grid-spacing-mils"
             "sl-minor-gpm"
             "tf-minor-gpm"
             "sl-zoom-ppu"
             "tf-zoom-ppu"
             "tf-zoom-range-min"
             "tf-zoom-range-max"
             "cb-major-grid-snap-to"
             "cb-major-grid-lines-visible"
             "sl-major-grid-line-width"
             "tf-major-grid-line-width"
             "cs-major-grid-line-color"
             "cb-major-grid-dots-visible"
             "sl-major-grid-dots-width"
             "tf-major-grid-dots-width"
             "cs-major-grid-dots-color"
             "cb-minor-grid-snap-to"
             "cb-minor-grid-lines-visible"
             "sl-minor-grid-line-width"
             "tf-minor-grid-line-width"
             "cs-minor-grid-line-color"
             "cb-minor-grid-dots-visible"
             "sl-minor-grid-dots-width"
             "tf-minor-grid-dots-width"
             "cs-minor-grid-dots-color"]]
    (doseq [id ids]
      (when-let [node (lookup root id)]
        (.setId node (join-hyph name id)))))
  root)



(defn grid-settings-pane [name]
  (let [gsp (load-fxml-root "GridSettingsPane.fxml")]
    (update-names! gsp name)
    (update-sliders! gsp name)))














