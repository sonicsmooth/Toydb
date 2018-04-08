(ns toydb.ui.GridSettingsPane
  (:require [clojure.pprint :as pp]
            [jfxutils.core :as jfxc :refer [printexp lookup get-property]]
            [jfxutils.ui :as jfxui]
            [jfxutils.bind :as jfxb]
            [toydb.units :refer [um mm cm m km inch mil incr decr nearest distance
                                 distance-string-converter distance-text-formatter
                                 add sub]]
            [toydb.edn.converters :as conv]))




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
Rectangular grid
keys for pan/zoom
Save/load values


"

;; This may go into a separate file someday
(def possible-init-settings
  {:major-grid/enable true
   :major-grid/spacing (um (mm 10))
   :major-grid/lines-visible true
   :major-grid/line-width-px 0.25
   :major-grid/line-color javafx.scene.paint.Color/BLACK
   :major-grid/dots-visible true
   :major-grid/dot-width-px 2.0
   :major-grid/dot-color javafx.scene.paint.Color/BLACK
   :major-grid/snap-to true

   :minor-grid/enable true
   :minor-grid/ratio 8
   :minor-grid/lines-visible true
   :minor-grid/line-width-px 0.025
   :minor-grid/line-color javafx.scene.paint.Color/BLACK
   :minor-grid/dots-visible true
   :minor-grid/dot-width-px 0.5
   :minor-grid/dot-color javafx.scene.paint.Color/BLACK
   :minor-grid/snap-to true

   :axes/visible true
   :axes/line-width-px 1.0
   :axes/line-color javafx.scene.paint.Color/BLACK

   :origin/visible true
   :origin/line-width-px 3.0
   :origin/line-color javafx.scene.paint.Color/GREEN
   :origin/marker :diag-crosshair
   :origin/size-px 10

   :zoom/ppmm 10.0
   :zoom/dynamic-grid-enable true
   :zoom/scale-visible true

   ;; Background is actually part of the editor not GridSettings Pane
   :background/gradient-top (javafx.scene.paint.Color/web "F9F2FF")
   :background/gradient-bottom (javafx.scene.paint.Color/web "E1F2FFac")})

;; This gets dynamically bound with the values from the init-file
;;(def ^:dynamic *init-vals*)

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

    (jfxb/bind! :var state
                ;;:init (:major-grid/spacing *init-vals*)
                :keyvec [:major-grid/spacing]
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
    (jfxb/bind! :var state
                ;;:init (:minor-grid/ratio *init-vals*)
                :keyvec [:minor-grid/ratio]
                :property :value
                :no-action-val nil
                :range-fn #(jfxui/number-range-check % lower upper :clip)
                :var-to-prop-fn int ;; valueconverter is integer 
                :prop-to-var-fn long ;; valueconverter is integer
                :targets [tgt-spgpmvf])  ))

(defn setup-sliders! [state lu]
  (jfxui/setup-generic-slider-and-text
   state lu
   {:slider "sl-axis-line-width"
    :textfield "tf-axis-line-width"
    :keyvec [:axes/line-width-px]
    :type Double
    :range [0.01 5.0]
    ;;:init (:axes/line-width-px *init-vals*)
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-origin-line-width"
    :textfield "tf-origin-line-width"
    :keyvec [:origin/line-width-px]
    :type Double
    :range [0.01 5.0]
    ;;:init (:origin/line-width-px *init-vals*)
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-origin-size"
    :textfield "tf-origin-size"
    :keyvec [:origin/size-px]
    :type Double
    :range [1.0 50.0]
    ;;:init (:origin/size-px *init-vals*)
    :major-tick-unit 10
    :minor-tick-count 4
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 5
    :snap-to 0.5}
   {:slider "sl-zoom-ppmm"
    :textfield "tf-zoom-ppmm"
    :keyvec [:zoom/ppmm]
    :type Double
    :range [0.25 20.0 ]
    ;;:init (:zoom/ppmm *init-vals*)
    ;;:major-tick-unit 5
    ;;:minor-tick-count 5
    ;;:show-tick-marks false
    ;;:show-tick-labels false
    ;;:block-increment 1.0
    :snap-to 0.1}
   {:slider "sl-major-grid-line-width"
    :textfield "tf-major-grid-line-width"
    :keyvec [:major-grid/line-width-px]
    :type Double
    :range [0.01 8.0]
    ;;:init (:major-grid/line-width-px *init-vals*)
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-major-grid-dot-width"
    :textfield "tf-major-grid-dot-width"
    :keyvec [:major-grid/dot-width-px]
    :type Double
    :range [0.01 10.0]
    ;;:init (:major-grid/dot-width-px *init-vals*)
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-minor-grid-line-width"
    :textfield "tf-minor-grid-line-width"
    :keyvec [:minor-grid/line-width-px]
    :type Double
    :range [0.01 8.0]
    ;;:init (:minor-grid/line-width-px *init-vals*)
    :major-tick-unit 5
    :minor-tick-count 3
    :show-tick-marks true
    :show-tick-labels true
    :block-increment 1.0
    :snap-to 0.01}
   {:slider "sl-minor-grid-dot-width"
    :textfield "tf-minor-grid-dot-width"
    :keyvec [:minor-grid/dot-width-px]
    :type Double
    :range [0.01 10.0]
    ;;:init (:minor-grid/dot-width-px *init-vals*)
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
  (jfxui/setup-generic-checkbox!
   state lu
   {:checkbox "cb-major-grid-snap-to"
    :keyvec [:major-grid/snap-to]
    ;;:init (:major-grid/snap-to *init-vals*)
    }
   {:checkbox "cb-minor-grid-snap-to"
    :keyvec [:minor-grid/snap-to]
    ;;:init (:minor-grid/snap-to *init-vals*)
    })

  
  (let [major-snap-qual #(and (:major-grid/enable %)
                              (:major-grid/snap-to %))
        minor-snap-qual #(and (:major-grid/enable %)
                              (:minor-grid/enable %)
                              (:minor-grid/snap-to %))
        swap-snap! #(let [majsq (major-snap-qual %)
                          minsq (minor-snap-qual %)
                          anysq (or majsq minsq)]
                      (swap! state jfxc/multi-assoc-in
                             [:major-grid/calculated/snap-allowed] majsq
                             [:minor-grid/calculated/snap-allowed] minsq
                             [:any-grid/calculated/snap-allowed] anysq))]
    
    (add-watch state :snap-enabler
               (fn [key ref old new]
                 (when (or (not= (minor-snap-qual old)
                                 (minor-snap-qual new))
                           (not= (major-snap-qual old)
                                 (major-snap-qual new)))
                   (swap-snap! new))))
    ;;(swap-snap! @state)
    ))

(defn setup-visibility-checkboxes!
  "Set up checkboxes which both enable a feature, such as axes-visible,
  and enable or disable related UI elements so they look grayed out
  when the feature is not available. "
  [state lu]
  ;; Need to do getGraphic directly from grid-panes because
  ;; jfxutils.core/subnodes does not support multiple gets out of
  ;; content
  (let [ ;; Outer TitledPanes
        tp-axes (lu "tp-axes")
        tp-origin (lu "tp-origin")
        tp-major-grid (lu "tp-major-grid")
        tp-minor-grid (lu "tp-minor-grid")

        ;; Inner TitledPanes
        tp-major-lines (lu "tp-major-lines")
        tp-major-dots (lu "tp-major-dots")
        tp-major-spacing (lu "tp-major-spacing")

        tp-minor-lines (lu "tp-minor-lines")
        tp-minor-dots (lu "tp-minor-dots")
        tp-minor-properties (lu "tp-minor-properties")

        ;; Checkboxes
        cb-axes-visible (.getGraphic tp-axes)
        cb-origin-visible (.getGraphic tp-origin)
        cb-major-grid-enable (.getGraphic tp-major-grid)
        cb-major-grid-snap-to (lu "cb-major-grid-snap-to")
        cb-major-grid-lines-visible (.getGraphic tp-major-lines)
        cb-major-grid-dots-visible (.getGraphic tp-major-dots)
        cb-minor-grid-enable (.getGraphic tp-minor-grid)
        cb-minor-grid-snap-to (lu "cb-minor-grid-snap-to")
        cb-minor-grid-lines-visible (.getGraphic tp-minor-lines)
        cb-minor-grid-dots-visible (.getGraphic tp-minor-dots)

        ;; GridPanes
        gp-axes-elements (lu "gp-axes-elements")
        gp-origin-elements (lu "gp-origin-elements")
        gp-major-lines-elements (lu "gp-major-lines-elements")
        gp-major-dots-elements (lu "gp-major-dots-elements")
        gp-minor-lines-elements (lu "gp-minor-lines-elements")
        gp-minor-dots-elements (lu "gp-minor-dots-elements")]

    (jfxb/bind! :var state
                ;; :init (:axes/visible *init-vals*),
                :keyvec [:axes/visble]
                :targets {cb-axes-visible {:property :selected}
                          gp-axes-elements {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state, #_:init #_(:origin/visible *init-vals*), :keyvec [:origin/visible]
                :targets {cb-origin-visible {:property :selected}
                          gp-origin-elements {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state
                ;;:init (:major-grid/enable *init-vals*)
                :keyvec [:major-grid/enable]
                :property :disable
                :var-to-prop-fn not
                :targets {cb-major-grid-enable {:property :selected, :var-to-prop-fn identity}
                          tp-minor-grid {}
                          cb-major-grid-snap-to {}
                          tp-major-lines {}
                          tp-major-dots {}
                          tp-major-spacing {}})
    
    (jfxb/bind! :var state
                ;;:init (:major-grid/lines-visible *init-vals*)
                :keyvec [:major-grid/lines-visible]
                :targets {cb-major-grid-lines-visible {:property :selected}
                          gp-major-lines-elements {:property :disable, :var-to-prop-fn not}})
    
    (jfxb/bind! :var state,
                ;;:init (:major-grid/dots-visible *init-vals*)
                :keyvec [:major-grid/dots-visible]
                :targets {cb-major-grid-dots-visible {:property :selected}
                          gp-major-dots-elements {:property :disable, :var-to-prop-fn not}})

    (jfxb/bind! :var state,
                ;;:init (:minor-grid/enable *init-vals*)
                :keyvec [:minor-grid/enable]
                :property :disable
                :var-to-prop-fn not
                :targets {cb-minor-grid-enable {:property :selected, :var-to-prop-fn identity}
                          cb-minor-grid-snap-to {}
                          tp-minor-lines {}
                          tp-minor-dots {}
                          tp-minor-properties {}})
    
    (jfxb/bind! :var state
                ;;:init (:minor-grid/lines-visible *init-vals*)
                :keyvec [:minor-grid/lines-visible]
                :targets {cb-minor-grid-lines-visible {:property :selected}
                          gp-minor-lines-elements {:property :disable, :var-to-prop-fn not}})
    
    (jfxb/bind! :var state
                ;;:init (:minor-grid/dots-visible *init-vals*)
                :keyvec [:minor-grid/dots-visible]
                :targets {cb-minor-grid-dots-visible {:property :selected}
                          gp-minor-dots-elements {:property :disable, :var-to-prop-fn not}})

    ;; Adjust checkbox positions to the far right, adding 20px to avoid ellipsis
    ;; There is also the option someplace to not show the ellipsis
    (doseq [tgt [tp-axes tp-origin
                 tp-major-grid tp-major-lines tp-major-dots
                 tp-minor-grid tp-minor-lines tp-minor-dots]]
      (.bind (jfxc/get-property tgt :graphic-text-gap)
             (.subtract (get-property tgt :width)
                        (.add (get-property (.getGraphic tgt) :width)
                              (+ (.. (doto (javafx.scene.text.Text. (.getText tgt)) .applyCss)
                                     getLayoutBounds getWidth) 20)))))))

(defn setup-other-checkboxes!
  "Sets up generic checkboxes, which only connect to var state, not
  visibility or other logic."
  [state lu]
  (jfxb/bind! :var state
              ;;:init (:zoom/dynamic-grid-enable *init-vals*)
              :keyvec [:zoom/dynamic-grid-enable]
              :property :selected
              :targets [(lu "cb-dynamic-grid")])
  (jfxb/bind! :var state
              ;;:init (:zoom/scale-visible *init-vals*)
              :keyvec [:zoom/scale-visible]
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
    (jfxc/set-items! combo [:diag-crosshair :crosshair :circle])
    (doto combo
      (.setCellFactory callb)
      (.setConverter conv))

    (jfxb/bind! :var state
                ;;:init (:origin/marker *init-vals*)
                :keyvec [:origin/marker]
                :property :value
                :targets [(lu "dd-origin-marker")])))



(defn setup-background-color-selectors! [state lu]
  "Because [:calculated :background] is the final property used, we must create it
  separately and swap it in for initialization."
  (let [swap-background! (fn [top bot]
                           (swap! state assoc-in [:background/calculated]
                                  (jfxc/background top bot)))]
    (jfxui/setup-generic-color-selector!
     state lu
     {:picker "col-bg-top"
      :keyvec [:background/gradient-top]
      ;;:init (:background/gradient-top *init-vals*)
      }
     {:picker "col-bg-bot"
      :keyvec [:background/gradient-bottom]
      ;;:init (:background/gradient-bottom *init-vals*)
      })

    ;; Create a new background when relevant color changes occur
    (add-watch state :background-changer
               (fn [key ref old new]
                 (when (jfxc/keydiff old new [:background/gradient-top
                                              :background/gradient-bottom])
                   (swap-background!
                    (:background/gradient-top new)
                    (:background/gradient-bottom new)))))
    #_(swap-background! (:background/gradient-top *init-vals*)
                      (:background/gradient-bottom *init-vals*))))

(defn setup-other-color-selectors! [state lu]
  (let [idtriples ["col-axis-line-color"       #_:axes/line-color       [:axes/line-color  ]
                   "col-origin-line-color"     #_:origin/line-color     [:origin/line-color]
                   "col-major-grid-line-color" #_:major-grid/line-color [:major-grid/line-color ]
                   "col-major-grid-dot-color"  #_:major-grid/dot-color  [:major-grid/dot-color  ]
                   "col-minor-grid-line-color" #_:minor-grid/line-color [:minor-grid/line-color ]
                   "col-minor-grid-dot-color"  #_:minor-grid/dot-color  [:minor-grid/dot-color  ]]]
    (apply jfxui/setup-generic-color-selector! state lu
           (map #(hash-map :picker (first %)
                           ;;:init ((second %) *init-vals*)
                           :keyvec (second %)) ;; third (was nth 2)
                (partition 2 idtriples)))))

(defn- home-path
  "Returns java.io.File of user's home path plus .toydb
  subdirectory plus additional argument path.  Appears to accept both
  / and \\ separators in addpath."
  ([]
   (home-path ""))
  ([addpath]
   (.toFile (java.nio.file.Paths/get
             (System/getProperty "user.home")
             (into-array [".toydb" addpath])))))

(defn setup-theme-panel! [load! save! revert! lu]
  (let [btn-revert (lu "btn-revert")
        btn-load (lu "btn-load")
        btn-save (lu "btn-save")
        dd-theme (lu "dd-theme")
        make-fullname #(str "settings/" (.getValue %) ".edn")
        set-list! (fn []
                    (let [dirlist (->> (file-seq (home-path "settings"))
                                       (filter #(.isFile %))
                                       (map #(.getName %))
                                       (filter #(.endsWith % ".edn"))
                                       (map #(clojure.string/replace % ".edn" "" )))]
                      (jfxc/set-items! dd-theme dirlist)))]

    ;; Set up the buttons
    (jfxc/set-on-action! btn-revert (revert!))
    (jfxc/set-on-action! btn-load (load! (home-path (make-fullname dd-theme))))
    (jfxc/set-on-action! btn-save (do
                                    (save! (home-path (make-fullname dd-theme)))
                                    (set-list!))) ;; update list when item is saved
    (set-list!)

    ;; Set up the list to auto-load when new item is selected
    ;; Not sure why this is triggered twice when defocus
    (jfxc/add-listener! (.getSelectionModel dd-theme) :selected-item
                        (fn [oldval newval]
                          (load! (home-path (make-fullname dd-theme)))))

    ;; Set up the load and save buttons to disable when text box is blank
    (.bind (jfxc/get-property btn-save :disable)
           (javafx.beans.binding.Bindings/equal
            (jfxc/get-property (.getEditor dd-theme) :text) ""))
    (.bind (jfxc/get-property btn-load :disable)
           (javafx.beans.binding.Bindings/equal
            (jfxc/get-property (.getEditor dd-theme) :text) ""))))




(defn load-settings
  "Load single map from src and given keys."
  [src keyz]
  (let [srcmap (condp instance? src
                 clojure.lang.PersistentArrayMap src
                 clojure.lang.PersistentHashMap src
                 ;;java.lang.String (load-settings (clojure.java.io/reader src) keyz)
                 ;;java.io.File (load-settings (clojure.java.io/reader src) keyz)
                 ;;java.io.Reader
                 ;;(with-open [f src])
                 (conv/read-string (slurp src))) ;; can take a String, File, or Reader
        srcvals (map #(find srcmap %) keyz)]
    (into {} srcvals)))

(defn- merge-keymaps
  "Returns a merged keymap from keymaps, used for save.  Keymap is of
  the form {map1 [keys-to-get] map2 [keys-to-get]} and returns the
  form {key1 val-from-map1,...}"
  [keymaps]
  (let [srcmaps (keys keymaps)
        srckeys (vals keymaps)
        get-pair (fn [m k] (when-let [v (m k)] [k v]))
        collect-mapvals (fn [ma keyz]
                          (->> (mapcat get-pair (repeat @ma) keyz)
                               (apply hash-map)))]
    (reduce merge (map collect-mapvals srcmaps srckeys))))

(defn GridSettingsPane [name]
  "Load up GridSettingsPane.  Returns a map with both the node and the
  state. "
  (let [grid-settings (atom {})
        editor-settings (atom {})
        ;;init-file (clojure.java.io/as-file "settings/GridSettings.edn")
        root (doto (jfxc/load-fxml-root "GridSettingsPane4.fxml"))
        lu (fn [id] (if-let [result (jfxc/lookup root id)]
                      result
                      (throw (Exception. (str "Could not find " id)))))]

    (setup-visibility-checkboxes! grid-settings lu)
    (setup-major-grid-spacing-spinners! grid-settings lu)
    (setup-minor-grid-per-major-grid-spinner! grid-settings lu)
    (setup-sliders! grid-settings lu)
    (setup-snap-to-checkboxes! grid-settings lu)
    (setup-origin-marker-selection! grid-settings lu)
    (setup-background-color-selectors! editor-settings lu) ;; <<-- editor-settings !!
    (setup-other-color-selectors! grid-settings lu)
    (setup-other-checkboxes! grid-settings lu)

    ;; A quicker way might be to dissoc the :background keys
    ;; from possible-init-settings
    (let [keymaps {grid-settings [:axes/line-color
                                  :axes/line-width-px 
                                  :axes/visible true,
                                  :major-grid/dot-color 
                                  :major-grid/dot-width-px
                                  :major-grid/dots-visible
                                  :major-grid/enable 
                                  :major-grid/line-color 
                                  :major-grid/line-width-px 
                                  :major-grid/lines-visible
                                  :major-grid/snap-to 
                                  :major-grid/spacing 
                                  :minor-grid/dot-color
                                  :minor-grid/dot-width-px 
                                  :minor-grid/dots-visible 
                                  :minor-grid/enable
                                  :minor-grid/line-color 
                                  :minor-grid/line-width-px 
                                  :minor-grid/lines-visible 
                                  :minor-grid/ratio 
                                  :minor-grid/snap-to 
                                  :origin/line-color 
                                  :origin/line-width-px 
                                  :origin/marker 
                                  :origin/size-px 
                                  :origin/visible 
                                  :zoom/dynamic-grid-enable 
                                  :zoom/ppmm
                                  :zoom/scale-visible]
                   editor-settings [:background/gradient-bottom
                                    :background/gradient-top]}
          last-init-source (atom nil)
          load! (fn [file] ;; a String or File
                  ;; Ensure file is a java.io.File
                  (let [file (condp instance? file
                               java.io.File file
                               java.lang.String (java.io.File. file))]
                    (println "Loading " (.getPath file))
                    (doseq [[setting keyz] keymaps]
                      ;; Swap in default-settings first, then current settings, then file settings.
                      ;; This ensures everything has at least some value, but does not overwrite
                      ;; an existing value with a default value if the new file doesn't specify.
                      ;; IOW, just change if the value is specified.
                      (let [file-settings (try (load-settings file keyz)
                                               (catch java.io.FileNotFoundException e
                                                 (println (format "File %s not found" (.getPath file)))))]
                        (swap! setting merge
                               (load-settings possible-init-settings keyz)
                               file-settings)
                        (reset! last-init-source file)))))

          save! (fn [file]
                  ;; Ensure file is a java.io.File
                  (let [file (condp instance? file
                               java.io.File file
                               java.lang.String (java.io.File. file))
                        map-to-save (merge-keymaps keymaps)]
                    (println "Saving to " (.getPath file))
                    (binding [*print-dup* true 
                              pp/*print-right-margin* 80] ;; don't break lines too early  
                      (with-open [f (clojure.java.io/writer file)]
                        (pp/pprint (into (sorted-map) map-to-save) f))
                      (reset! last-init-source file))))

          revert! (fn [] (load! @last-init-source))]
      
      (setup-theme-panel! load! save! revert! lu)
      (load! (home-path "settings/GridSettings.edn"))

      )

    (def gs grid-settings)
    (def es editor-settings)
    (def root root)
      
    
    {:root root
     :grid-settings grid-settings
     :editor-settings editor-settings
     }))

(defn main []
  (jfxc/set-exit false)
  (let [{:keys [root grid-settings editor-settings]} (GridSettingsPane "demo")]
     (jfxc/stage root ;;[800 600]
                 )))

(defn -main []
  (jfxc/app-init)
  (jfxc/stage (GridSettingsPane "demo") [800 600]))




