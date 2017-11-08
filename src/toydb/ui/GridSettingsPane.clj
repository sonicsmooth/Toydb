(ns toydb.ui.GridSettingsPane
  (:require [jfxutils.core :refer [add-listener! app-init change-listener get-property*
                                   get-property index-of invalidation-listener
                                   get-prop-val jfxnew join-hyph load-fxml-root
                                   lookup printexp replace-item set-exit set-prop-val!
                                   split-hyph stage]]
            [toydb.bind :refer [bind! dirtify!]]
            [toydb.units :refer [um mm cm m km inch mil incr decr nearest distance
                                 distance-string-converter distance-text-formatter
                                 add sub]]))


(def ERROR-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "error"))


(def ids ["cb-enable-major-grid"
          "cb-enable-minor-grid"
          "sp-major-grid-spacing-mm"
          "sp-major-grid-spacing-mils"
          "sp-minor-gpm"
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
  (let [;;minor-gpm (lookup root (join-hyph name "sl-minor-gpm"))
        zoom-ppu (lookup root (join-hyph name "sl-zoom-ppu"))
        major-glw (lookup root (join-hyph name "sl-major-grid-line-width"))
        major-gdw (lookup root (join-hyph name  "sl-major-grid-dots-width"))
        minor-glw (lookup root (join-hyph name  "sl-minor-grid-line-width"))
        minor-gdw (lookup root (join-hyph name "sl-minor-grid-dots-width"))]
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

#_(defn update-textfields!
  "Set up text converters"
  [root name]
  (let [tgt-mm (lookup root (join-hyph name "tf-major-grid-spacing-mm"))
        tgt-mil (lookup root (join-hyph name "tf-major-grid-spacing-mils"))
        err-listener #(change-listener [oldval newval]
                                       (.pseudoClassStateChanged
                                        % ERROR-PSEUDO-CLASS (not (distance newval))))
        field-fns {tgt-mm mm, tgt-mil mil}]
    ;; Set up text formatter on each text field
    ;; Set up error listener so text fields format red on error
    (doseq [txtfield [tgt-mm tgt-mil]]
      (set-prop-val! txtfield :text-formatter (distance-text-formatter (field-fns txtfield)))
      (add-listener! txtfield :text (err-listener txtfield))
      (.add (.getStyleClass txtfield) "error"))))

(defn spinner-distance-value-factory
  "Returns proxy of SpinnerValueFactory.  Unit is the unit shown, one
  of um, mm, cm, m, km, mil, or inch.  init is the initial underlying
  value.  The type of this value is maintained.  For example, if unit
  is mm and init is (inch 1), then each increment wil be 1 inch, but
  display will be in mm."
  ([unit]
   (doto (proxy [javafx.scene.control.SpinnerValueFactory] []
           (decrement [steps] (.setValue this (decr (.getValue this))))
           (increment [steps] (.setValue this (incr (.getValue this)))))
     (.setConverter (distance-string-converter unit))))
  ([unit init]
   (doto (spinner-distance-value-factory unit)
     (.setValue (unit init)))))

(defn update-spinners!
  "Set up spinners"
  [root name]
  (let [sp-mm (lookup root (join-hyph name "sp-major-grid-spacing-mm"))
        sp-mil (lookup root (join-hyph name "sp-major-grid-spacing-mils"))
        sp-gpm (lookup root (join-hyph name "sp-minor-gpm"))

        ;; Define listeners directly, not with make-listener style function.
        ;; The listener has all it needs to determine the Nodes, properties, etc.
        invalid-listener (invalidation-listener
                          ;; Force local text to update regardless of whether the underlying
                          ;; value is the same as before
                          (let [newvalue (.getValue observable)
                                spinner (.getBean observable)
                                converter (.. spinner getValueFactory getConverter)
                                newstring (.toString converter newvalue)]
                            (.setText (.getEditor spinner) newstring)))

        ;; call TextField's on-action, same as user pressing enter
        focus-listener (change-listener 
                        (when-not newval
                          (.. observable getBean getEditor
                              getOnAction (handle (javafx.event.ActionEvent.)))))

        err-listener (change-listener [oldval newval]
                                      (.pseudoClassStateChanged
                                       (.. observable getBean)
                                       ERROR-PSEUDO-CLASS (not (distance newval))))
        field-fns {sp-mm mm, sp-mil mil}]

    (doseq [spinner [sp-mm sp-mil]]
      (.setValueFactory spinner (spinner-distance-value-factory (field-fns spinner)))
      (add-listener! spinner :focused focus-listener)
      (add-listener! spinner :value invalid-listener)
      (.setEditable spinner true)
      ;;(.. spintxt getStyleClass (add "error")) ;; not necessary?
      (let [spintxt (.getEditor spinner)]
        (add-listener! spintxt :text err-listener )))

    (doseq [spinner [sp-gpm]]
      (.setValueFactory
       spinner
       (javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory. 2 10))))) 

(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (doseq [id ids]
    (when-let [node (lookup root id)]
      (.setId node (join-hyph name id))))
  root)


(defn bind-properties!
  "Bind control properties to clojure state.  Root is some parent Node
  of the hierarchy.  name is the common name given to all the nodes of
  a given branch to distinguish them from an otherwise identical
  branch under the root.  state is the clojure atom holding the
  state."
  [root name state]
  (let [lu #(lookup root (join-hyph name %))
        tgt-enmajg (lu "cb-enable-major-grid")
        tgt-enming (lu "cb-enable-minor-grid")
        tgt-spmm (lu "sp-major-grid-spacing-mm")
        tgt-spmmvf (.getValueFactory tgt-spmm)
        tgt-spmil (lu "sp-major-grid-spacing-mils")
        tgt-spmilvf (.getValueFactory tgt-spmil)
        tgt-spgpm (lu "sp-minor-gpm")
        tgt-spgpmvf (.getValueFactory tgt-spgpm )]

    ;; Mechanically go through each one for GridSettingsPane
    ;; The init values override the settings in the fxml

    (bind! :var state, :init false, :keyvec [:enable-major-grid]
           :var-fn! #(if %
                       (swap! state assoc :enable-major-grid true)
                       (swap! state assoc :enable-major-grid false, :enable-minor-grid false))
           :targets {tgt-enmajg {:property :selected}
                     tgt-spmm {:property :disable, :var-to-prop-fn not}
                     tgt-spmil {:property :disable, :var-to-prop-fn not}})
    (bind! :var state, :init false, :keyvec [:enable-minor-grid]
           :var-fn! #(if % ;; enable both, avoid assoc-in for now
                       (swap! state assoc :enable-minor-grid true, :enable-major-grid true)
                       (swap! state assoc :enable-minor-grid false))
           :targets {tgt-enming{:property :selected}
                     tgt-spgpm {:property :disable,
                                :var-to-prop-fn #(not (and (:enable-major-grid @state) %))}})
    (bind! :var state, :init (um (mm 10)), :keyvec [:major-grid-spacing-um]
           ;; var-fn could set state to nil if no-action-val is left
           ;; unspecified.  By specifying no-action-val, we let the
           ;; binding system "do nothing" to change the state, but
           ;; still trigger an update on the target nodes, which
           ;; allows targets to be filled with correct model value.
           :no-action-val nil
           :property :value
           :prop-to-var-fn #(nearest (um %) 0.1)
           :targets {tgt-spmmvf {:var-to-prop-fn mm}
                     tgt-spmilvf {:var-to-prop-fn mil}})
    (bind! :var state, :init (int 2,) :keyvec [:minor-gpm]
             :property :value
             :targets [tgt-spgpmvf]))
  
  root)



(defn GridSettingsPane [name]
  (let [state (atom {})]
    (def v state) ;; for use in repl
    (let [root (doto (load-fxml-root "GridSettingsPane.fxml")
                 (update-names! name)
                 (update-sliders! name)
                 ;;(update-textfields! name)
                 (update-spinners! name)
                 (bind-properties! name state))]
      
      root)))



(defn main []
  (set-exit false)
  (stage (GridSettingsPane "demo") [800 600]))

(defn -main []
  (app-init)
  (stage (GridSettingsPane "demo") [800 600]))


(defn test-spinner []
  (let [svf (spinner-distance-value-factory mm)
        sp (javafx.scene.control.Spinner. svf)]
    (def svf svf)
    (def sp sp)
    (.setEditable sp true)
    (.setFont (.getEditor sp) (javafx.scene.text.Font. 48))
    (.setValue svf (mm 100))
    (add-listener! sp :value (invalidation-listener
                              ;; Force local text to update regardless of whether the underlying
                              ;; value is the same as before
                              (let [newvalue (.getValue observable)
                                    spinner (.getBean observable)
                                    converter (.. spinner getValueFactory getConverter)
                                    newstring (.toString converter newvalue)]
                                (.setText (.getEditor spinner) newstring))))
    #_(add-listener! svf :value (invalidation-listener
                               (println "factory invalidated" (.getValue observable))))
    #_(add-listener! sp :value (change-listener
                              (println "spinner changed" (.getValue observable))))
    #_(add-listener! svf :value (change-listener
                               (println "factory changed" (.getValue observable))))
    #_(add-listener! (.getEditor sp) :text (change-listener
                                          (println "text changed")))
    (stage sp)))











