(ns toydb.ui.GridSettingsPane
  (:require [jfxutils.core :as jfxc]
            [jfxutils.ui :as jfxui]
            [jfxutils.bind :as jfxb]
            ;;[toydb.bind :refer [bind! dirtify!]]
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


"

(defn idvalid? [id-string]
  "Returns true if id is a proper name"
  (and (not (empty? id-string)) ;; captures both nil and ""
       (not (.startsWith id-string "arrow"))))

(defn recurse-named-nodes [node]
  "Return flattened list of all named subnodes of node"
  (letfn [(inner-dump [n]
            (let [id (.getId n)
                  idvalid (idvalid? id)
                  children (jfxc/subnodes n)
                  get-child-ids #(remove nil? (map inner-dump children))]
              (cond
                (and (not idvalid) (nil? children)) nil
                (and (not idvalid) children) (get-child-ids)
                (and idvalid (nil? children)) id
                (and idvalid children) (cons id (get-child-ids)))))]
    (flatten (inner-dump node))))


(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (let [ids (recurse-named-nodes root)]
    (doseq [id ids]
      (.setId (jfxc/lookup root id) (jfxc/join-hyph name id))))
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
  (let [tgt-enmajg (lu "cb-enable-major-grid")
        tgt-spmm (lu "sp-major-grid-spacing-mm")
        tgt-spmil (lu "sp-major-grid-spacing-mils")
        major-grid-enable-bindings
        (jfxb/bind! :var state, :init true, :keyvec [:major-lines-visible]
               :var-fn! #(if % ;; enable one or disable both
                           (swap! state assoc-in [:major-lines-visible] true)
                           (swap! state jfxc/multi-assoc-in [:major-lines-visible] false, [:minor-lines-visible] false))
               :targets {tgt-enmajg {:property :selected}
                         tgt-spmm {:property :disable, :var-to-prop-fn not}
                         tgt-spmil {:property :disable, :var-to-prop-fn not}})

        tgt-enming (lu "cb-enable-minor-grid")
        tgt-spgpm (lu "sp-minor-gpm")
        minor-grid-enable-bindings
        (jfxb/bind! :var state, :init true, :keyvec [:minor-lines-visible]
               :var-fn! #(if % ;; enable both or disable the one
                           (swap! state jfxc/multi-assoc-in [:minor-lines-visible] true, [:major-lines-visible] true)
                           (swap! state assoc-in [:minor-lines-visible] false))
               :targets {tgt-enming{:property :selected}
                         tgt-spgpm {:property :disable,
                                    :var-to-prop-fn #(not (and (:major-lines-visible @state) %))}})]))

(defn setup-snap-to-checkboxes [state lu]
  (jfxui/setup-generic-checkbox
   state lu
   {:checkbox "cb-major-grid-snap-to"
    :keyvec [:major-snap]
    :init false}
   {:checkbox "cb-minor-grid-snap-to"
    :keyvec [:minor-snap]
    :init false}))

(defn setup-visibility-checkboxes [state lu]
  (jfxb/bind! :var state, :init true, :keyvec [:major-lines-visible]
         :property :disable
         :var-to-prop-fn not
         :targets {(lu "cb-major-grid-lines-visible") {:property :selected, :var-to-prop-fn identity}
                   (lu "sl-major-grid-line-width") {}
                   (lu "tf-major-grid-line-width") {}
                   (lu "cs-major-grid-line-color") {}})
  
  (jfxb/bind! :var state, :init true, :keyvec [:major-dots-visible]
         :property :disable
         :var-to-prop-fn not
         :targets {(lu "cb-major-grid-dots-visible") {:property :selected, :var-to-prop-fn identity}
                   (lu "sl-major-grid-dot-width") {}
                   (lu "tf-major-grid-dot-width") {}
                   (lu "cs-major-grid-dot-color") {}})

  (jfxb/bind! :var state, :init true, :keyvec [:minor-lines-visible]
         :property :disable
         :var-to-prop-fn not
         :targets {(lu "cb-minor-grid-lines-visible") {:property :selected, :var-to-prop-fn identity}
                   (lu "sl-minor-grid-line-width") {}
                   (lu "tf-minor-grid-line-width") {}
                   (lu "cs-minor-grid-line-color") {}})

  (jfxb/bind! :var state, :init true, :keyvec [:minor-dots-visible]
         :property :disable
         :var-to-prop-fn not
         :targets {(lu "cb-minor-grid-dots-visible") {:property :selected, :var-to-prop-fn identity}
                   (lu "sl-minor-grid-dot-width") {}
                   (lu "tf-minor-grid-dot-width") {}
                   (lu "cs-minor-grid-dot-color") {}}))


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
        tgt-spgpm (doto (lu "sp-minor-gpm") (.setValueFactory tgt-spgpmvf))]
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

(defn setup-sliders [state lu]
  (jfxui/setup-generic-slider-and-text
   state lu
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
    :range [0.1 20.0]
    :init 1.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.25}
   {:slider "sl-major-grid-dot-width"
    :textfield "tf-major-grid-dot-width"
    :keyvec [:major-dot-width-px]
    :type Double
    :range [0.1 20.0]
    :init 1.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.25}
   {:slider "sl-minor-grid-line-width"
    :textfield "tf-minor-grid-line-width"
    :keyvec [:minor-line-width-px]
    :type Double
    :range [0.1 20.0]
    :init 1.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.25}
   {:slider "sl-minor-grid-dot-width"
    :textfield "tf-minor-grid-dot-width"
    :keyvec [:minor-dot-width-px]
    :type Double
    :range [0.1 20.0]
    :init 1.0
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.25}))



(defn GridSettingsPane [name]
  "Load up GridSettingsPane.  Returns a map with both the node and the
  state. "
  (let [state (atom {})]
    (let [root (doto (jfxc/load-fxml-root "GridSettingsPane.fxml"))
          lu (fn [id] (jfxc/lookup root (jfxc/join-hyph name id)))]
      (update-names! root name)
      (setup-grid-enable-checkboxes! state lu)
      (setup-major-grid-spacing-spinners! state lu)
      (setup-minor-grid-per-major-grid-spinner! state lu)
      (setup-zoom-level-range-text! state lu)
      (setup-sliders state lu)
      (setup-snap-to-checkboxes state lu)
      (setup-visibility-checkboxes state lu)
      {:root root :settings state})))

(defn main []
  (jfxc/set-exit false)
  (let [{:keys [root state]} (GridSettingsPane "demo") 
        st (jfxc/stage root [800 600])
        
        #_root #_(stage (jfxnew javafx.scene.layout.HBox
                            :id "hb"
                            :stylesheets [(str (clojure.java.io/resource "style.css"))]
                            :children [(jfxnew javafx.scene.control.Slider :id "other-slider")
                                       (jfxnew javafx.scene.control.TextField :id "other-textfield")]
                            :alignment javafx.geometry.Pos/CENTER) [400 100])
        ;;lu (fn [id] (lookup root id))
        ;;sc (.getScene root)
        ]

    #_(jfxutils.core/run-later
     (setup-generic-slider-and-text
      v lu
      {:slider "other-slider"
       :textfield "other-textfield"
       :keyvec [:zoom-ppu]
       :type Long
       :range [5 200] 
       :init 30
       :major-tick-unit 50
       :minor-tick-count 9
       :show-tick-marks true
       :show-tick-labels true}))
    ))

(defn -main []
  (jfxc/app-init)
  (jfxc/stage (GridSettingsPane "demo") [800 600]))


(defn test-spinner []
  (let [svf (distance-spinner-value-factory mm)
        sp (javafx.scene.control.Spinner. svf)]
    (def svf svf)
    (def sp sp)
    (.setEditable sp true)
    (.setFont (.getEditor sp) (javafx.scene.text.Font. 48))
    (.setValue svf (mm 100))
    (jfxc/add-listener! sp :value (jfxc/invalidation-listener
                                   ;; Force local text to update regardless of whether the underlying
                                   ;; value is the same as before
                                   (let [newvalue (.getValue observable)
                                         spinner (.getBean observable)
                                         converter (.. spinner getValueFactory getConverter)
                                         newstring (.toString converter newvalue)]
                                     (.setText (.getEditor spinner) newstring))))
    (jfxc/stage sp)))



  






















