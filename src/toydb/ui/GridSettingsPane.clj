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
Reduce axis to some origin marker
Snap-to for large and small
Disables for large and small
Rectangular grid
Position/scale overlay
keys for pan/zoom


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

        ;; GridPanes
        gpmaj (lu "gp-major-grid-elements")
        gpmin (lu "gp-minor-grid-elements")

        ;; Corner checkboxes
        enmaj (.getGraphic tpmaj)
        enmin (.getGraphic tpmin)

        major-grid-enable-bindings
        (jfxb/bind! :var state, :init true, :keyvec [:major-grid-enable]
                    :targets {enmaj {:property :selected}
                              gpmaj {:property :disable, :var-to-prop-fn not}
                              tpmin {:property :disable, :var-to-prop-fn not}
                              })

        minor-grid-enable-bindings
        (jfxb/bind! :var state, :init true, :keyvec [:minor-grid-enable]
                      :targets {enmin {:property :selected}
                                gpmin {:property :disable, :var-to-prop-fn not}})]

    ;; Adjust checkbox positions to the far right, adding 20px to avoid ellipsis
    (doseq [tgt [tpmaj tpmin]]
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
  (let [lower (int 2) ;; use int because there is no LongSpinnerValueFactory
        upper (int 10)
        tgt-spgpmvf (jfxui/integer-spinner-value-factory lower upper)
        tgt-spgpm (doto (lu "sp-minor-grid-ratio") (.setValueFactory tgt-spgpmvf))]
    (.setEditable tgt-spgpm true)
    (jfxui/setup-number-textfield! (.getEditor tgt-spgpm) (.getConverter tgt-spgpmvf) lower upper)
    (jfxb/bind! :var state, :init lower, :keyvec [:minor-gpm]
           :property :value
           :no-action-val nil
           :range-fn #(jfxui/number-range-check % lower upper :clip)
           :targets [tgt-spgpmvf])  ))

(defn setup-zoom-level-range-text! [state lu]
  (jfxui/setup-generic-text
   state lu
   {:textfield "tf-zoom-range-min"
    :type Long
    :keyvec [:zoom-range-min]
    :range [-400 0]
    :init -200}
   {:textfield "tf-zoom-range-max"
    :type Long
    :keyvec [:zoom-range-max]
    :range [0 400]
    :init 200}))

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
   {:slider "sl-zoom-ppu"
    :textfield "tf-zoom-ppu"
    :keyvec [:zoom-ppu]
    :type Long
    :range [5 200] 
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

(defn setup-snap-to-checkboxes! [state lu]
  (jfxui/setup-generic-checkbox
   state lu
   {:checkbox "cb-major-grid-snap-to"
    :keyvec [:major-snap]
    :init false}
   {:checkbox "cb-minor-grid-snap-to"
    :keyvec [:minor-snap]
    :init false}))

(defn setup-visibility-checkboxes! [state lu]
  (jfxb/bind! :var state, :init true, :keyvec [:axes-visible]
              :property :disable
              :var-to-prop-fn not
              :targets {(lu "cb-axes-visible") {:property :selected, :var-to-prop-fn identity}
                        (lu "sl-axis-line-width") {}
                        (lu "tf-axis-line-width") {}
                        (lu "col-axis-line-color") {}})
  
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

(defn setup-generic-color-selector! [state lu & specs]
  "Args is list of maps with :color, :keyvec, init"
  (doseq [spec specs]
    (jfxb/bind! :var state, :init (:init spec), :keyvec (:keyvec spec)
                :property :value
                :no-action-val nil
                :targets [(lu (:picker spec))])))

(defn setup-background-color-selectors! [state lu]
  "Because :background is the final property used, we must create it
  separately and swap it in for initialization."
  (let [top-init (javafx.scene.paint.Color/web "F9FCFF")
        bot-init (javafx.scene.paint.Color/web "E1F2FF")
        swap-background! (fn [top bot]
                           (swap! state assoc :background
                                  (jfxc/background top bot)))]
    (setup-generic-color-selector
     state lu
     {:picker "col-bg-top"
      :keyvec [:background-color-top]
      :init top-init}
     {:picker "col-bg-bot"
      :keyvec [:background-color-bot]
      :init bot-init})

    ;; Create a new background when relevant color changes occur
    (add-watch state :background-changer
               (fn [k r o n]
                 (when (or (not= (:background-color-top o)
                                 (:background-color-top n))
                           (not= (:background-color-bot o)
                                 (:background-color-bot n)))
                   (swap-background!
                    (:background-color-top n)
                    (:background-color-bot n)))))
    (swap-background! top-init bot-init)))

(defn setup-other-color-selectors! [state lu]
  (let [idpairs ["col-axis-line-color"       [:axis-line-color ]
                 "col-major-grid-line-color" [:major-line-color]
                 "col-major-grid-dot-color"  [:major-dot-color ]
                 "col-minor-grid-line-color" [:minor-line-color]
                 "col-minor-grid-dot-color"  [:minor-dot-color ]]]
    (apply setup-generic-color-selector state lu
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
                        (throw (Exception. (str "Could not find " id)))))
          #_(fn [id] (let [fqn (jfxc/join-hyph name id)]
                     (if-let [result (jfxc/lookup root fqn)]
                       result
                       (throw (Exception. (str "Could not find " fqn))))))]
      ;;(update-names! root "")
      (def root root)
      (def lu lu)
      (setup-grid-enable-checkboxes! grid-settings lu)
      (setup-major-grid-spacing-spinners! grid-settings lu)
      (setup-minor-grid-per-major-grid-spinner! grid-settings lu)
      (setup-zoom-level-range-text! grid-settings lu)
      (setup-sliders! grid-settings lu)
      (setup-snap-to-checkboxes! grid-settings lu)
      (setup-visibility-checkboxes! grid-settings lu)
      (setup-background-color-selectors! editor-settings lu)
      (setup-other-color-selectors! grid-settings lu)
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






  






















