(ns toydb.ui.GridSettingsPane
  (:require [jfxutils.core :as jfxc :refer [printexp lookup get-property]]
            [jfxutils.ui :as jfxui]
            [jfxutils.bind :as jfxb]
            [toydb.units :refer [um mm cm m km inch mil incr decr nearest distance
                                 distance-string-converter distance-text-formatter
                                 add sub]]))




"Todo: describe strategy for managing textfields, etc."
"GridSettingsPane should return state for (so far) three atoms:
1. Editor specs, currently background colors
2. Grid specs, such as line and dots width and color
3. Viewdef, such as minors-per-major, and grid spacing

However currently it puts grid specs and a few viewdef together in one
atom, and doesn't have editor background color or axis toggle.  Also,
the UI has line/dot color selection, but these are not currently
connected to the state.

Todo: 
* Reduce axis to some origin marker -- cross or circle
* Snap-to for large and small
* Disables for large and small
Rectangular grid
Position/scale overlay
keys for pan/zoom
Enable/disable dynamic scale
Zoom ratio
Save/load values
* :derivative key for background and snap-allowed

"



(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (doseq [id (jfxc/recurse-named-nodes root)]
    (.setId (jfxc/lookup root id) (jfxc/join-hyph name id)))
  root)


(defn distance-range-check
  "Extracts the value from d, mn, mx and runs (number-range-check)
  with any extra args.  Returns a number rather than a distance type."
  ([d mn mx action]
   (when d
     (jfxui/number-range-check (.value d) (.value mn) (.value mx) action)))
  ([d mn mx]
   (distance-range-check d mn mx nil)))

(defn distance-spinner-value-factory
  "Returns proxy of SpinnerValueFactory.  Unit is the unit shown, one
  of um, mm, cm, m, km, mil, or inch.  init is the initial underlying
  value.  The type of this value is maintained.  For example, if unit
  is mm and init is (inch 1), then each increment will be 1 inch, but
  display will be in mm. converter-or-unit is either a StringConverter
  or unit."
  ([converter-or-unit]
   (let [factory (proxy [javafx.scene.control.SpinnerValueFactory] []
                   (decrement [steps] (.setValue this (decr (.getValue this))))
                   (increment [steps] (.setValue this (incr (.getValue this)))))]
     (if (instance? javafx.util.StringConverter converter-or-unit)
       (doto factory (.setConverter converter-or-unit))
       (doto factory (.setConverter (distance-string-converter converter-or-unit))))))
  ([unit init]
     (doto (distance-spinner-value-factory unit)
       (.setValue (unit init))))
  ([unit min max]
   (distance-spinner-value-factory
    (distance-string-converter unit min max))))


(defn setup-grid-enable-checkboxes! [state lu]
  (let [ ;; TitledPanes
        tpmaj (lu "tp-major-grid")
        tpmin (lu "tp-minor-grid")
        tpaxes (lu "tp-axes")
        tporig (lu "tp-origin")

        ;; GridPanes
        gpmaj (lu "gp-major-grid-elements")
        gpmin (lu "gp-minor-grid-elements")
        gpaxes (lu "gp-axes-elements")
        gporig (lu "gp-orig-elements")

        ;; Corner checkboxes
        ;; Not using lu because jfxutils.core/subnodes does not support multiple gets of content
        enmaj (.getGraphic tpmaj) ;; (lu "cb-major-grid-enable")
        enmin (.getGraphic tpmin) ;; (lu "cb-minor-grid-enable")
        enaxes (.getGraphic tpaxes) ;; (lu "cb-axes-visible")
        enorig (.getGraphic tporig) ;; (lu "cb-origin-visible")
        ]

    (jfxb/bind! :var state, :init true, :keyvec [:major-grid-enable]
                :targets {enmaj {:property :selected}
                          gpmaj {:property :disable, :var-to-prop-fn not}
                          tpmin {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state, :init true, :keyvec [:minor-grid-enable]
                :targets {enmin {:property :selected}
                          gpmin {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state, :init true, :keyvec [:axes-visible]
                :targets {enaxes {:property :selected}
                          gpaxes {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state, :init true, :keyvec [:origin-visible]
                :targets {enorig {:property :selected}
                          gporig {:property :disable, :var-to-prop-fn not}})

    ;; Adjust checkbox positions to the far right, adding 20px to avoid ellipsis
    (doseq [tgt [tpmaj tpmin tpaxes tporig]]
      (.bind (jfxc/get-property tgt :graphic-text-gap)
             (.subtract (get-property tgt :width)
                        (.add (get-property (.getGraphic tgt) :width)
                              (+ (.. (doto (javafx.scene.text.Text. (.getText tgt)) .applyCss)
                                     getLayoutBounds getWidth) 20)))))))

(defn setup-major-grid-spacing-spinners! [state lu]
  (let [lower (um 100)
        upper (um (mm 1000))
        prop-to-var-fn #(nearest (um %) 0.1)
        drc #(distance-range-check (prop-to-var-fn %) lower upper)
        drc-clip #(distance-range-check (prop-to-var-fn %) lower upper :clip)

        ;; Listener gets called when var changes. Force local text to
        ;; update regardless of whether the underlying value is the
        ;; same as before, eg 10mm becomes 10000um.  Assume underlying
        ;; value is always valid.
        invalid-listener (jfxc/invalidation-listener
                          (let [newvalue (.getValue observable)
                                spinner (.getBean observable)
                                converter (.. spinner getValueFactory getConverter)]
                            (.setText (.getEditor spinner) (.toString converter newvalue))))

        ;; ValueFactories return either converted value or nil
        ;; Valid range is not checked in value factory
        tgt-spmmvf (distance-spinner-value-factory mm)
        tgt-spmilvf (distance-spinner-value-factory mil)
        tgt-spmm (lu "sp-major-grid-spacing-mm")
        tgt-spmil (lu "sp-major-grid-spacing-mils")]

    (.setValueFactory tgt-spmm tgt-spmmvf)
    (.setValueFactory tgt-spmil tgt-spmilvf)
    (doseq [tgt [tgt-spmm tgt-spmil]]
      (.setEditable tgt true)
      (jfxc/add-listener! tgt :value invalid-listener)
      (jfxui/setup-number-textfield! (.getEditor tgt) (.. tgt getValueFactory getConverter) drc))

    (jfxb/bind! :var state, :init (um (mm 10)), :keyvec [:major-spacing-um]
           :no-action-val nil
           :property :value
           :range-fn #(um (drc-clip %))
           :prop-to-var-fn prop-to-var-fn
           :targets {tgt-spmmvf {:var-to-prop-fn mm}
                     tgt-spmilvf {:var-to-prop-fn mil}})))

(defn setup-minor-grid-per-major-grid-spinner! [state lu]
  (let [lower 2
        upper 10
        tgt-spgpmvf (jfxui/integer-spinner-value-factory (int lower) (int upper)) ;; use int because there is no LongSpinnerValueFactory
        tgt-spgpm (doto (lu "sp-minor-grid-ratio") (.setValueFactory tgt-spgpmvf))]
    (.setEditable tgt-spgpm true)
    (jfxui/setup-number-textfield! (.getEditor tgt-spgpm) (.getConverter tgt-spgpmvf) lower upper)
    (jfxb/bind! :var state, :init 8, :keyvec [:minor-grid-ratio]
           :property :value
           :no-action-val nil
           :range-fn #(jfxui/number-range-check % lower upper :clip)
           :var-to-prop-fn int   ;; valueconverter is integer 
           :prop-to-var-fn long  ;; valueconverter is integer
           :targets [tgt-spgpmvf])  ))

(defn setup-sliders! [state lu]
  (jfxui/setup-generic-slider-and-text
   state lu
   {:slider "sl-axis-line-width"
    :textfield "tf-axis-line-width"
    :keyvec [:axis-line-width-px]
    :type Double
    :range [0.01 5.0]
    :init 1.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-origin-line-width"
    :textfield "tf-origin-line-width"
    :keyvec [:origin-line-width-px]
    :type Double
    :range [0.01 5.0]
    :init 3.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-zoom-ppmm"
    :textfield "tf-zoom-ppmm"
    :keyvec [:zoom-ppmm]
    :type Long
    :range [1 20] 
    :init 10
    :major-tick-unit 50
    :minor-tick-count 9
    :show-tick-marks true
    :show-tick-labels true}
   {:slider "sl-major-grid-line-width"
    :textfield "tf-major-grid-line-width"
    :keyvec [:major-line-width-px]
    :type Double
    :range [0.01 8.0]
    :init 0.25
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-major-grid-dot-width"
    :textfield "tf-major-grid-dot-width"
    :keyvec [:major-dot-width-px]
    :type Double
    :range [0.01 10.0]
    :init 2.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-minor-grid-line-width"
    :textfield "tf-minor-grid-line-width"
    :keyvec [:minor-line-width-px]
    :type Double
    :range [0.01 8.0]
    :init 0.025
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-minor-grid-dot-width"
    :textfield "tf-minor-grid-dot-width"
    :keyvec [:minor-dot-width-px]
    :type Double
    :range [0.01 10.0]
    :init 0.2
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}))

(defn setup-snap-to-checkboxes!
  "Bind major and minor snap-to checkboxes, and also create a
  derivative var entry which is the and of a bunch of stuff, similar
  to how the background colors works."
  [state lu]
  (jfxui/setup-generic-checkbox
   state lu
   {:checkbox "cb-major-grid-snap-to"
    :keyvec [:major-snap]
    :init true}
   {:checkbox "cb-minor-grid-snap-to"
    :keyvec [:minor-snap]
    :init true})

  
  (let [major-snap-qual #(and (:major-grid-enable %)
                              (:major-snap %))
        minor-snap-qual #(and (:major-grid-enable %)
                              (:minor-grid-enable %)
                              (:minor-snap %))
        swap-snap! #(let [majsq (major-snap-qual %)
                          minsq (minor-snap-qual %)
                          anysq (or majsq minsq)]
                      (swap! state jfxc/multi-assoc-in
                             [:calculated :major-snap-allowed] majsq
                             [:calculated :minor-snap-allowed] minsq
                             [:calculated :any-snap-allowed] anysq))]
    
    (add-watch state :snap-enabler
               (fn [key ref old new]
                 (when (or (not= (minor-snap-qual old)
                                 (minor-snap-qual new))
                           (not= (major-snap-qual old)
                                 (major-snap-qual new)))
                   (swap-snap! new))))
    (swap-snap! @state)))

(defn setup-visibility-checkboxes!
"Set up checkboxes which both enable a feature, such as axes-visible,
  and enable or disable related UI elements so they look grayed out
  when the feature is not available."
  [state lu]
  (jfxb/bind! :var state, :init true, :keyvec [:major-lines-visible]
              :property :disable
              :var-to-prop-fn not
              :targets {(lu "cb-major-grid-lines-visible") {:property :selected, :var-to-prop-fn identity}
                        (lu "sl-major-grid-line-width") {}
                        (lu "tf-major-grid-line-width") {}
                        (lu "col-major-grid-line-color") {}})
  
  (jfxb/bind! :var state, :init true, :keyvec [:major-dots-visible]
              :property :disable
              :var-to-prop-fn not
              :targets {(lu "cb-major-grid-dots-visible") {:property :selected, :var-to-prop-fn identity}
                        (lu "sl-major-grid-dot-width") {}
                        (lu "tf-major-grid-dot-width") {}
                        (lu "col-major-grid-dot-color") {}})

  (jfxb/bind! :var state, :init true, :keyvec [:minor-lines-visible]
              :property :disable
              :var-to-prop-fn not
              :targets {(lu "cb-minor-grid-lines-visible") {:property :selected, :var-to-prop-fn identity}
                        (lu "sl-minor-grid-line-width") {}
                        (lu "tf-minor-grid-line-width") {}
                        (lu "col-minor-grid-line-color") {}})

  (jfxb/bind! :var state, :init true, :keyvec [:minor-dots-visible]
              :property :disable
              :var-to-prop-fn not
              :targets {(lu "cb-minor-grid-dots-visible") {:property :selected, :var-to-prop-fn identity}
                        (lu "sl-minor-grid-dot-width") {}
                        (lu "tf-minor-grid-dot-width") {}
                        (lu "col-minor-grid-dot-color") {}}))

(defn setup-other-checkboxes!
  "Sets up generic checkboxes, which only connect to var state, not
  visibility or other logic."
  [state lu]
  (jfxb/bind! :var state, :init true :keyvec [:dynamic-grid-enable]
              :property :selected
              :targets [(lu "cb-dynamic-grid")])
  (jfxb/bind! :var state, :init true :keyvec [:scale-visible]
              :property :selected
              :targets [(lu "cb-scale-visible")]))

(defn setup-origin-marker-selection!
  "Sets up origin marker selection.  Similar to
  setup-visibility-checkboxes! but uses a ComboBox (aka drop-downlist)
  instead.  Sets up the ComboBox."
  [state lu]

  (let [combo (lu "dd-origin-marker")
        callb (jfxc/callback [value]
                             (proxy [javafx.scene.control.ListCell] []
                               (updateItem [item empty]
                                 (proxy-super updateItem item empty)
                                 (when item
                                   (.setText this (.toString (.getConverter combo) item))))))
        conv (proxy [javafx.util.StringConverter] []
               (fromString [s] (keyword s))
               (toString [k] (clojure.string/capitalize (name k))))]
    (jfxc/set-items! combo [:crosshair :diag-crosshair :circle])
    (doto combo
      (.setCellFactory callb)
      (.setConverter conv))

    (jfxb/bind! :var state, :init :diag-crosshair, :keyvec [:origin-marker]
                :property :value
                :targets [(lu "dd-origin-marker")])))

(defn setup-generic-color-selector! [state lu & specs]
  "Args is list of maps with :color, :keyvec, init"
  (doseq [spec specs]
    (jfxb/bind! :var state, :init (:init spec), :keyvec (:keyvec spec)
                :property :value
                :no-action-val nil
                :targets [(lu (:picker spec))])))

(defn setup-background-color-selectors! [state lu]
  "Because [:calculated :background] is the final property used, we must create it
  separately and swap it in for initialization."
  (let [top-init (javafx.scene.paint.Color/web "F9FCFF")
        bot-init (javafx.scene.paint.Color/web "E1F2FF")
        swap-background! (fn [top bot]
                           (swap! state assoc-in [:calculated :background]
                                  (jfxc/background top bot)))]
    (setup-generic-color-selector!
     state lu
     {:picker "col-bg-top"
      :keyvec [:background-color-top]
      :init top-init}
     {:picker "col-bg-bot"
      :keyvec [:background-color-bot]
      :init bot-init})

    ;; Create a new background when relevant color changes occur
    (add-watch state :background-changer
               (fn [key ref old new]
                 (when (jfxc/keydiff old new [:background-color-top
                                              :background-color-bot])
                   (swap-background!
                    (:background-color-top new)
                    (:background-color-bot new)))))
    (swap-background! top-init bot-init)))

(defn setup-other-color-selectors! [state lu]
  (let [idpairs ["col-axis-line-color"       [:axis-line-color ]
                 "col-origin-line-color"     [:origin-line-color]
                 "col-major-grid-line-color" [:major-line-color]
                 "col-major-grid-dot-color"  [:major-dot-color ]
                 "col-minor-grid-line-color" [:minor-line-color]
                 "col-minor-grid-dot-color"  [:minor-dot-color ]]]
    (apply setup-generic-color-selector! state lu
           (map #(hash-map :picker (first %)
                           :keyvec (second %)
                           :init javafx.scene.paint.Color/BLACK)
                (partition 2 idpairs)))))



(defn GridSettingsPane [name]
  "Load up GridSettingsPane.  Returns a map with both the node and the
  state. "
  (let [grid-settings (atom {})
        editor-settings (atom {})]
    (let [root (doto (jfxc/load-fxml-root "GridSettingsPane2.fxml"))
          lu (fn [id] (if-let [result (jfxc/lookup root id)]
                        result
                        (throw (Exception. (str "Could not find " id)))))]
      (def root root)
      (def lu lu)
      (def gs grid-settings)
      (setup-grid-enable-checkboxes! grid-settings lu)
      (setup-major-grid-spacing-spinners! grid-settings lu)
      (setup-minor-grid-per-major-grid-spinner! grid-settings lu)
      (setup-sliders! grid-settings lu)
      (setup-snap-to-checkboxes! grid-settings lu)
      (setup-visibility-checkboxes! grid-settings lu)
      (setup-origin-marker-selection! grid-settings lu)
      (setup-background-color-selectors! editor-settings lu)
      (setup-other-color-selectors! grid-settings lu)
      (setup-other-checkboxes! grid-settings lu)
      {:root root
       :grid-settings grid-settings
       :editor-settings editor-settings
       })))

(defn main []
  (jfxc/set-exit false)
  (let [{:keys [root grid-settings editor-settings]} (GridSettingsPane "demo")]
     (jfxc/stage root [800 600])))

(defn -main []
  (jfxc/app-init)
  (jfxc/stage (GridSettingsPane "demo") [800 600]))




