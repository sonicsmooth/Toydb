(ns toydb.ui.GridSettingsPane
  (:require [jfxutils.core :refer [add-listener! app-init change-listener get-property* clip event-handler
                                   get-property index-of invalidation-listener
                                   get-prop-val jfxnew join-hyph load-fxml-root
                                   lookup multi-assoc-in printexp replace-item set-exit set-prop-val!
                                   set-on-key-pressed! set-on-key-typed! set-on-key-released! split-hyph stage]]
            [toydb.bind :refer [bind! dirtify!]]
            [toydb.units :refer [um mm cm m km inch mil incr decr nearest distance
                                 distance-string-converter distance-text-formatter
                                 add sub]]))


(def ERROR-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "error"))
(def OUT-OF-RANGE-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "out-of-range"))
(def EDITING-PSEUDO-CLASS (javafx.css.PseudoClass/getPseudoClass "editing"))


"Current strategy: 
 For UI groups with text, the text field has a converter which returns
 either the converted and clipped number, or nil if not convertible.
 As you type, the listener runs the converter and if nil, colors the
 text red.  When user presses enter, the converter is run
 automatically again by JFX.  If nil is returned, the binding stuff
 ignores it, and if not nil, the binding stuff changes the value of
 the underlying variable, which may have been clipped by the
 converter.  The problem with this version is that the system does not
 indicate as the user is typing whether the number will be clipped.
 Also, there is no obvious or consistent mechanism for clipping or
 error detection when trying to swap! the variable from outside the
 UI.  For now, all the binding stuff happens in one place, including
 qualifying and clipping the var, and all the listeners are set up in
 another place.

New strategy: 

Set up binding and listeners together, for each group of controls.
This allows for less redundancy when selecting items from the graph.
Somehow (TBD) distinguish between a valid-but-clipped value typed in
by the user, and an invalid value which should not be accepted at all.
Allow valid-but-clipped values to be colored differently from
non-convertible values.  Unify functions to qualify/clip the value
both in the UI and when swap!ing the var.  I don't think it's easy to
entirely work around the StringConverter concept, such as with
spinners, so those converters will either convert unclipped, or send
nil with invalid argument.  The listener on the text field will have
to distinguish among invalid, clipped, or not clipped for coloring.
The validation function for the var will be the same function."


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
      (.setShowTickLabels true)
      jfxutils.core/integer-slider)
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


;; call TextField's on-action, same as user pressing enter
(def txt-defocus-listener
  (change-listener
   (let [bean (.getBean observable)]
     (.. bean getOnAction (handle (javafx.event.ActionEvent.)))
     (when (false? newval)
       (.pseudoClassStateChanged bean EDITING-PSEUDO-CLASS false)))))

(defn number-range-check
  "Checks if x is between mn and mx, inclusive.  If yes, then returns x.
  If x is outside the range, then the following occurs based on the
  value of action:
  :throw         Throws a NumberFormatException
  :clip          Limits the number to mn or mx
  :clip-and-tell Limits the number to mn or mx, and wraps it in a vector
  false otherwise"
  ([x mn mx]
   (if (nil? x) nil
     (and (>= x mn) (<= x mx) x)))
  ([x mn mx action]
   (if (nil? x) nil
     (if (or (> x mx) (< x mn))
       (condp = action
         :throw (throw (NumberFormatException.))
         :clip (clip x mn mx)
         :clip-and-tell (clip x mn mx :tellme))
       x))))

(defn distance-range-check [d mn mx & args]
  "Extracts the value from d, mn, mx and runs (number-range-check)
  with any extra args."
  (if d
    (apply number-range-check (.value d) (.value mn) (.value mx) args)
    d))

(defmacro nullable-string-converter
  "Returns a proxy of StringConverter of the type given by numtype, eg
  Integer, Double, etc.  The presumption is that numtype is some
  numeric type, such that min and max can be used.  If action
  is :clip, then the string is converted to be within min and max.  If
  action is :throw, then nil is returned.  If min, max, and action are
  all omitted, then a 'simple' version is returned which uses the
  numtype's built-in MIN_VALUE and MAX_VALUE, and which returns nil
  with out-of-range or unparsable inputs.

  TODO: Return an indicator that the number has been clipped
"
  ([numtype min max action]
   (let [cname (str "javafx.util.converter." (name numtype) "StringConverter")
         csym (symbol cname)
         ssfn (fn [o]
                [(str "nullable-" (name numtype) "-string-converter Exception in .toString from")
                 o ", of type " `(class ~o)])]
     `(proxy [~csym] []
        (~'toString
         ([] ~cname) ;; need zero-arity fn for repl display
         ([obj#]
          (let [~'locobj obj#] ;; need to keep obj constant for passing to ssfn
            (try (proxy-super ~'toString ~'locobj)
                 (catch Exception e# ;; not sure what exception might be thrown
                   (println ~(ssfn 'locobj)))))))
        (~'fromString [s#] (try (number-range-check (proxy-super ~'fromString s#) ~min ~max ~action)
                                (catch NumberFormatException e#
                                  nil))))))
  ([numtype]
   (let [num-min (fn [numtype] (symbol (str numtype "/MIN_VALUE")))
         num-max (fn [numtype] (symbol (str numtype "/MAX_VALUE")))]
     `(nullable-string-converter ~numtype ~(num-min numtype) ~(num-max numtype) :throw))))


(defn spinner-distance-value-factory
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
     (doto (spinner-distance-value-factory unit)
       (.setValue (unit init))))
  ([unit min max]
   (spinner-distance-value-factory
    (distance-string-converter unit min max))))

(defn spinner-integer-value-factory
  "Returns an instance of SpinnerValueFactory.IntegerSpinnerValueFactory 
  which returns nil for an improper entry instead of throwing an exception."
  [min max #_action]
  (let [mn Integer/MIN_VALUE
        mx Integer/MAX_VALUE]
    (doto (javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory. min max)
      (.setConverter (nullable-string-converter Integer mn mx :throw)))))

(defn setup-grid-enable-checkboxes! [state lu]
  (let [tgt-enmajg (lu "cb-enable-major-grid")
        tgt-spmm (lu "sp-major-grid-spacing-mm")
        tgt-spmil (lu "sp-major-grid-spacing-mils")
        major-grid-enable-bindings
        (bind! :var state, :init true, :keyvec [:enable-major-grid]
               :var-fn! #(if % ;; enable one or disable both
                           (swap! state assoc-in [:enable-major-grid] true)
                           (swap! state multi-assoc-in [:enable-major-grid] false, [:enable-minor-grid] false))
               :targets {tgt-enmajg {:property :selected}
                         tgt-spmm {:property :disable, :var-to-prop-fn not}
                         tgt-spmil {:property :disable, :var-to-prop-fn not}})

        tgt-enming (lu "cb-enable-minor-grid")
        tgt-spgpm (lu "sp-minor-gpm")
        minor-grid-enable-bindings
        (bind! :var state, :init true, :keyvec [:enable-minor-grid]
               :var-fn! #(if % ;; enable both or disable the one
                           (swap! state multi-assoc-in [:enable-minor-grid] true, [:enable-major-grid] true)
                           (swap! state assoc-in [:enable-minor-grid] false))
               :targets {tgt-enming{:property :selected}
                         tgt-spgpm {:property :disable,
                                    :var-to-prop-fn #(not (and (:enable-major-grid @state) %))}})]))

(defn err-range-fn! [textfield newval in-range?]
  "Sets the pseudoclass states of textfield for error and in-range.
   For error, it checks wether newval is nil.  For in-range, it
   evaluates newval with in-range?"
  (.pseudoClassStateChanged textfield ERROR-PSEUDO-CLASS (nil? newval))
  (.pseudoClassStateChanged textfield OUT-OF-RANGE-PSEUDO-CLASS (not (in-range? newval))))

(defn err-range-listener [converter in-range?]
  "Returns change listener which converts the 'newval' string and runs
  it through the in-range? fn. The in-range? fn returns true if the
  converted value is within range."
  (change-listener ;; newval is a string, converted to nil or good, no Exception
   (let [newval-from-string (.fromString converter newval)
         textfield (.getBean observable)]
     (err-range-fn! textfield newval-from-string in-range?))))

(defn enable-editing-pseudostate [textfield]
  (event-handler [event]
                 (let [code (.getCode event)]
                   (when (or (.isDigitKey code)
                             (.isKeypadKey code)
                             (.isLetterKey code)
                             (.isWhitespaceKey code)
                             (= javafx.scene.input.KeyCode/BACK_SPACE code)
                             (= javafx.scene.input.KeyCode/DELETE code))
                     (.pseudoClassStateChanged textfield EDITING-PSEUDO-CLASS true)))))

(defn setup-major-grid-spacing-spinners! [state lu]
  (let [lower-um (um 100)
        upper-um (um (mm 1000))
        prop-to-var-fn #(nearest (um %) 0.1)
        drc #(distance-range-check (prop-to-var-fn %) lower-um upper-um)
        drc-clip #(distance-range-check (prop-to-var-fn %) lower-um upper-um :clip)

        ;; Listener gets called when var changes. Force local
        ;; text to update regardless of whether the underlying value
        ;; is the same as before.  Assume underlying value is always
        ;; valid.
        invalid-listener (invalidation-listener
                          (let [newvalue (.getValue observable)
                                spinner (.getBean observable)
                                converter (.. spinner getValueFactory getConverter)]
                            (.setText (.getEditor spinner) (.toString converter newvalue))))

        ;; ValueFactories return either converted value or nil
        ;; Valid range is not checked in value factory
        tgt-spmmvf (spinner-distance-value-factory mm)
        tgt-spmilvf (spinner-distance-value-factory mil)

        tgt-spmm (doto (lu "sp-major-grid-spacing-mm") (.setValueFactory tgt-spmmvf))
        tgt-spmil (doto (lu "sp-major-grid-spacing-mils") (.setValueFactory tgt-spmilvf))
        major-grid-spacing-bindings
        (bind! :var state, :init (um (mm 10)), :keyvec [:major-grid-spacing-um]
               :no-action-val nil
               :property :value
               :range-fn #(um (drc-clip %))
               :prop-to-var-fn prop-to-var-fn
               :targets {tgt-spmmvf {:var-to-prop-fn mm}
                         tgt-spmilvf {:var-to-prop-fn mil}})]

    (doseq [tgt-spx [tgt-spmm tgt-spmil]]
      (.setEditable tgt-spx true)
      (let [textfield (.getEditor tgt-spx)]
        (add-listener! textfield :focused txt-defocus-listener) ;; disables editing pseudostate
        (add-listener! textfield :text (err-range-listener (.. tgt-spx getValueFactory getConverter) drc)) ;; colors red or yellow
        (.setOnKeyPressed textfield (enable-editing-pseudostate textfield)))))) ;; enables editing pseudostate (green if not red or yellow)

(defn setup-minor-grid-per-major-grid-spinner! [state lu]
  (let [lower (int 2)  ;; use int because there is no LongSpinnerValueFactory
        upper (int 10)
        drc #(number-range-check % lower upper)
        drc-clip #(number-range-check % lower upper :clip)
        tgt-spgpmvf (spinner-integer-value-factory lower upper)
        tgt-spgpm (doto (lu "sp-minor-gpm") (.setValueFactory tgt-spgpmvf))

        minor-gpm-bindings
        (bind! :var state, :init lower, :keyvec [:minor-gpm]
               :no-action-val nil
               :property :value
               :range-fn drc-clip
               :targets [tgt-spgpmvf])]

    (.setEditable tgt-spgpm true)
    (let [textfield (.getEditor tgt-spgpm)]
      (add-listener! textfield :focused txt-defocus-listener)
      (add-listener! textfield :text (err-range-listener (.. tgt-spgpm getValueFactory getConverter) drc))
      (.setOnKeyPressed textfield (enable-editing-pseudostate textfield)))))


(defn setup-zoom-level-range-text! [state lu])
(defn setup-overall-zoom-slider-and-text! [state lu])
(defn setup-major-grid-line-width-slider-and-text! [state lu])
(defn setup-major-grid-dot-width-slider-and-text! [state lu])
(defn setup-minor-grid-line-width-slider-and-text! [state lu])
(defn setup-minor-grid-dot-width-slider-and-text! [state lu])

(defn update-textfields!
  "Set up text converters"
  [root name]
  (let [tgt-zppu (lookup root (join-hyph name "tf-zoom-ppu"))
        tgt-maxz (lookup root (join-hyph name "tf-zoom-range-max"))
        tgt-minz (lookup root (join-hyph name "tf-zoom-range-min"))
        nlsc (fn [mn mx] (nullable-string-converter Long mn mx :clip))
        err-listener (fn [textfield]
                       (change-listener 
                        (let [newval-from-string (.. textfield getTextFormatter getValueConverter (fromString newval))]
                          (printexp newval-from-string)
                          (.pseudoClassStateChanged
                           textfield
                           ERROR-PSEUDO-CLASS
                           (nil? newval-from-string)))))]
 
    ;; Set up error listener so text fields format red on error
    ;; TextFormatter can deal with exceptions from
    ;; IntegerStringConverter when user presses enter, but
    ;; err-listener uses the StringConverter directly, which does
    ;; throw Exceptions, so we use the nullable-long-string-converter
    ;; (nlsc) If nlsc gets a :clip argument, then it clips an
    ;; out-of-range value and the text box does not become red while
    ;; typing.  If nlsc get a :throw argument, then it returns a nil
    ;; when given an out-of-range string.  In any case the nlsc
    ;; returns a nil when given an unparsable string.

    
    (set-prop-val! tgt-zppu :text-formatter (javafx.scene.control.TextFormatter. (nlsc    5 200)))
    (set-prop-val! tgt-minz :text-formatter (javafx.scene.control.TextFormatter. (nlsc -400 400)))
    (set-prop-val! tgt-maxz :text-formatter (javafx.scene.control.TextFormatter. (nlsc -400 400)))

    (add-listener! tgt-zppu :text (err-listener tgt-zppu))
    (add-listener! tgt-minz :text (err-listener tgt-minz))
    (add-listener! tgt-maxz :text (err-listener tgt-maxz))))

(defn update-spinners!
  "Set up spinners"
  [root name]
  (let [;;sp-mm (lookup root (join-hyph name "sp-major-grid-spacing-mm"))
        ;;sp-mil (lookup root (join-hyph name "sp-major-grid-spacing-mils"))
        sp-gpm (lookup root (join-hyph name "sp-minor-gpm"))

        ;; The listener has all it needs to determine the Nodes, properties, etc.
        ;; Gets called when var changes
        invalid-listener (invalidation-listener
                          ;; Force local text to update regardless of whether the underlying
                          ;; value is the same as before.  Assume underlying value is always valid.
                          (let [newvalue (.getValue observable)
                                spinner (.getBean observable)
                                converter (.. spinner getValueFactory getConverter)]
                            (.setText (.getEditor spinner)
                                      (.toString converter newvalue))))

        ;; Realtime error checking
        err-listener (fn [spinner valid?]
                       (change-listener ;; newval is a string, converted to nil or good, no Exception
                        (let [newval-from-string (.. spinner getValueFactory getConverter (fromString newval))]
                          (.pseudoClassStateChanged
                           (.getBean observable)
                           ERROR-PSEUDO-CLASS
                           (nil? newval-from-string)))))]

    #_(doseq [spinner [sp-mm sp-mil]]
      (.setEditable spinner true)
      (add-listener! spinner :focused txt-focus-listener)
      (add-listener! spinner :value invalid-listener)
      (add-listener! (.getEditor spinner) :text (err-listener spinner some? #_#(and % (pos? (.value %))))))


    (doseq [spinner [sp-gpm]]
      (.setEditable spinner true)
      (add-listener! spinner :focused txt-focus-listener)
      (add-listener! spinner :value invalid-listener)
      (add-listener! (.getEditor spinner) :text (err-listener spinner some?)))

    ;;(.setValueFactory sp-mm (spinner-distance-value-factory mm 0 1000))
    ;;(.setValueFactory sp-mil (spinner-distance-value-factory mil 0 (.value (mil (mm 1000)))))
    (.setValueFactory sp-gpm (spinner-integer-value-factory (int 5) (int 10) :clip))))

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
  (let [lu (fn [id] (lookup root (join-hyph name id)))





        ;; Zoom scale controls
        ;; Use range-fn instead of validator
        tgt-slzppu (lu "sl-zoom-ppu")
        tgt-tfzppu (.getTextFormatter (lu "tf-zoom-ppu"))
        zoom-ppu-bindings
        (bind! :var state, :init 10, :keyvec [:zoom-ppu]
               :property :value
               :no-action-val nil
               :targets {tgt-tfzppu {} ;; no prop-to-var-fn because converter returns proper value or nil
                         tgt-slzppu {:prop-to-var-fn long}})  ;; slider returns doubles, so need to convert

        
        tgt-minz (.getTextFormatter (lu "tf-zoom-range-min"))
        zoom-range-min-bindings
        (bind! :var state, :init -200, :keyvec [:zoom-range-min]
               :property :value
               :no-action-val nil
               :targets [tgt-minz])

        tgt-maxz (.getTextFormatter (lu "tf-zoom-range-max"))
        zoom-range-max-bindings
        (bind! :var state, :init 200, :keyvec [:zoom-range-max]
               :property :value
               :no-action-val nil
               :targets [tgt-maxz])


        ]

    (def zppu tgt-slzppu))
  
  root)



(defn GridSettingsPane [name]
  (let [state (atom {})]
    (def v state) ;; for use in repl
    (let [root (doto (load-fxml-root "GridSettingsPane.fxml"))
          lu (fn [id] (lookup root (join-hyph name id)))]
      (update-names! root name)
      ;;(update-sliders! name)

      (setup-grid-enable-checkboxes! state lu)
      (setup-major-grid-spacing-spinners! state lu)
      (setup-minor-grid-per-major-grid-spinner! state lu)

      ;;(setup-zoom-level-range-text! name state lu)
      ;;(setup-overall-zoom-slider-and-text! name state lu)

      ;;(setup-major-grid-line-width-slider-and-text! name state lu )
      ;;(setup-major-grid-dot-width-slider-and-text! name state lu)
      ;;(setup-minor-grid-line-width-slider-and-text! name state lu)
      ;;(setup-minor-grid-dot-width-slider-and-text! name state lu)

                 
      ;;(update-textfields! name)
      ;;(update-spinners! name)
      ;;(bind-properties! name state)
      (def root root)
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

#_(do 
  (def sp (javafx.scene.control.Spinner. 10 100 50))
  (def tf (.getEditor sp))
  (def st (stage sp))
  (def sc (.getScene st))
  (.setUserAgentStylesheet sc "style.css")
  (printexp (.getStyleClass sp))
  (printexp (.getStyleClass tf))
  ;;(.pseudoClassStateChanged tf OUT-OF-RANGE-PSEUDO-CLASS true)  
  ;;(.pseudoClassStateChanged tf OUT-OF-RANGE-PSEUDO-CLASS false)
  ;;(.pseudoClassStateChanged tf ERROR-PSEUDO-CLASS true)
  ;;(.pseudoClassStateChanged tf ERROR-PSEUDO-CLASS false)
  (.setEditable sp true))

  






















