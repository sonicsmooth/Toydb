(ns toydb.ui.GridSettingsPane
  (:require [jfxutils.core :refer [add-listener! app-init change-listener get-property*
                                   index-of join-hyph load-fxml-root
                                   lookup printexp replace-item set-exit split-hyph stage]]
            [toydb.bind :refer [bind! invalidate!]]
            [toydb.units :refer [um mm cm km inch mil nearest]]))



(def ids ["cb-enable-major-grid"
          "cb-enable-minor-grid"
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
          "cs-minor-grid-dots-color"])

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
      (.setMax 20)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto major-gdw
      (.setMin 0)
      (.setMax 20)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto minor-glw
      (.setMin 0)
      (.setMax 20)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))
    (doto minor-gdw
      (.setMin 0)
      (.setMax 20)
      (.setMajorTickUnit 1)
      (.setMinorTickCount 20)
      (.setShowTickLabels true))))

(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (doseq [id ids]
    (when-let [node (lookup root id)]
      (.setId node (join-hyph name id))))
  root)


(defn- strquote [s]
  (str "\"" s "\""))

(defn bind-properties!
  "Bind control properties to vars"
  [root name vars]
  (let [lu #(lookup root (join-hyph name %))
        tgt-mm (lu "tf-major-grid-spacing-mm")
        tgt-mil (lu "tf-major-grid-spacing-mils")]

    ;; Mechanically go through each one for GridSettingsPane
    ;; The init values override the settings in the fxml
    #_(bind! :var vars, :init false, :keyvec [:enable-major-grid]
           :targets [(lu "cb-enable-major-grid")]         
           :property :selected)
    #_(bind! :var vars, :init false, :keyvec [:enable-minor-grid]
           :targets [(lu "cb-enable-minor-grid")]
           :property :selected)
    
    (bind! :var vars, :init (um (mm 10)), :keyvec [:major-grid-spacing-um]
           :property :text
           :no-action nil
           :targets {tgt-mm {:prop-to-var-fn #(let [result (nearest (um (mm %)) 0.01)]
                                                ;;(println "mmp2v:" (strquote  %) "->" result)
                                                result)
                             :var-to-prop-fn #(let [result (format "%5.5g" (.value (nearest (mm %) 0.000001)))]
                                                ;;(println "mmv2p:" % "->" (strquote result))
                                                (if (.isFocused tgt-mm) nil result))}
                     tgt-mil {:prop-to-var-fn #(let [result (nearest (um (mil %)) 0.1)]
                                                 ;;(println "milp2v:" (strquote %) "->" result)
                                                 result)
                              :var-to-prop-fn #(let [result (format "%5.5g" (.value (mil %)))]
                                                 ;;(println "milv2p:" % "->" (strquote result))
                                                 (if (.isFocused tgt-mil) nil result))}}))
  root)



(defn GridSettingsPane [name]
  (let [vars (atom {})]
    (def v vars) ;; for use in repl
    (let [root (doto (load-fxml-root "GridSettingsPane.fxml")
                 (update-names! name)
                 (update-sliders! name)
                 (bind-properties! name vars))
          lu #(lookup root (join-hyph name %))
          tgt-mm (lu "tf-major-grid-spacing-mm")
          tgt-mil (lu "tf-major-grid-spacing-mils")
          listener (change-listener [oldval newval]
                                    (when (false? newval)
                                      (invalidate! vars)))]
      
      ;; Set up focus listener so values are updated when focus is lost
      (add-listener! tgt-mm :focused listener)
      (add-listener! tgt-mil :focused listener)
      root)))



(defn main []
  (set-exit false)
  (stage (GridSettingsPane "demo") [800 600]))

(defn -main []
  (app-init)
  (stage (GridSettingsPane "demo") [800 600]))













