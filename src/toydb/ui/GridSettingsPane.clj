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
          "sl-major-grid-dot-width"
          "tf-major-grid-dot-width"
          "cs-major-grid-dot-color"
          "cb-minor-grid-snap-to"
          "cb-minor-grid-lines-visible"
          "sl-minor-grid-line-width"
          "tf-minor-grid-line-width"
          "cs-minor-grid-line-color"
          "cb-minor-grid-dots-visible"
          "sl-minor-grid-dot-width"
          "tf-minor-grid-dot-width"
          "cs-minor-grid-dot-color"])

(defn update-names!
  "Append name to all ids in settings-pane"
  [root name]
  (doseq [id ids]
    (when-let [node (lookup root id)]
      (.setId node (join-hyph name id))))
  root)


(defn update-sliders!
  "Set up ticks, snap"
  [root name]
  (let [;;minor-gpm (lookup root (join-hyph name "sl-minor-gpm"))
        ;;zoom-ppu (lookup root (join-hyph name "sl-zoom-ppu"))
        ;;major-glw (lookup root (join-hyph name "sl-major-grid-line-width"))
        ;;major-gdw (lookup root (join-hyph name  "sl-major-grid-dots-width"))
        minor-glw (lookup root (join-hyph name  "sl-minor-grid-line-width"))
        minor-gdw (lookup root (join-hyph name "sl-minor-grid-dots-width"))]

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
     (when (false? newval)
       (when-let [action (.getOnAction bean)]
         (.handle action (javafx.event.ActionEvent.)))
       (.pseudoClassStateChanged bean EDITING-PSEUDO-CLASS false)))))

(defn number-range-check
  "Checks if x is between mn and mx, inclusive.  If yes, then returns x.
  If x is outside the range, then the following occurs based on the
  value of action:
  nil            Returns nil
  :noaction      Returns x
  :throw         Throws a NumberFormatException
  :clip          Limits the number to mn or mx
  :clip-and-tell Limits the number to mn or mx, and wraps it in a vector"
  ([x mn mx]
   (if (nil? x) nil
     (and (>= x mn) (<= x mx) x)))
  ([x mn mx action]
   (if (nil? x) nil
       (if (or (> x mx) (< x mn))
         (condp = action
           nil nil
           :noaction x
           :throw (throw (NumberFormatException.))
           :clip (clip x mn mx)
           :clip-and-tell (clip x mn mx :tellme))
       x))))

(defn distance-range-check
  "Extracts the value from d, mn, mx and runs (number-range-check)
  with any extra args.  Returns a number rather than a distance type."
  ([d mn mx action]
   (when d
     (number-range-check (.value d) (.value mn) (.value mx) action)))
  ([d mn mx]
   (distance-range-check d mn mx nil)))

(defmacro nullable-string-converter
  "Returns a proxy of StringConverter of the type given by numtype, eg
  Integer, Double, etc.  The presumption is that numtype is some
  numeric type, such that min and max can be used.  If the string is
  convertible to numtype and is within range, then that value is
  returned.  If it is not convertible, then nil is returned.  If it is
  convertible but out of range, then action is taken.  In this case,
  if action is :clip, then the value is limited to min and max.  If
  action is :noaction, then the value is returned as is.  If action
  is :nil, then nil is returned.  If min, max, and action are all
  omitted, then a 'simple' version is returned which uses the
  numtype's built-in MIN_VALUE and MAX_VALUE, and which returns nil
  with out-of-range or unparsable inputs."
  ([numtype min max action]
   (let [cname (str "javafx.util.converter." (name numtype) "StringConverter")
         csym (symbol cname)
         ssfn (fn [o]
                [(str "nullable-" (name numtype) "-string-converter Exception in .toString from \"")
                 o "\", of type " `(class ~o)])
         newaction ({:noaction :noaction
                     :nil :throw ;; :nil returns nil when out of range, but the number fn requires :throw
                     :clip :clip
                     ;; deliberately exclude clip-and-tell because a vector
                     ;; cannot be converted to the numtype required by the StringConverter
                     } action)]
     `(proxy [~csym] []
        (~'toString
         ([] ~cname) ;; need zero-arity fn for repl display
         ([obj#]
          (let [~'locobj obj#] ;; need to keep obj constant for passing to ssfn
            (try (proxy-super ~'toString ~'locobj)
                 (catch Exception e# ;; not sure what exception might be thrown
                   (println ~(ssfn 'locobj)))))))
        (~'fromString [s#] (try (number-range-check (proxy-super ~'fromString s#) ~min ~max ~newaction)
                                (catch NumberFormatException e#
                                  nil))))))
  ([numtype]
   (let [num-min (fn [numtype] (symbol (str numtype "/MIN_VALUE")))
         num-max (fn [numtype] (symbol (str numtype "/MAX_VALUE")))]
     `(nullable-string-converter ~numtype ~(num-min numtype) ~(num-max numtype) :nil))))


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

(defn integer-spinner-value-factory
  "Returns an instance of
  SpinnerValueFactory.IntegerSpinnerValueFactory.  The spinner will
  stay within min and max, but the number converter will return any
  proper integer, or nil if the string is unreadable."
  [min max]
  (let [mn Integer/MIN_VALUE
        mx Integer/MAX_VALUE]
    (doto (javafx.scene.control.SpinnerValueFactory$IntegerSpinnerValueFactory. min max)
      (.setConverter (nullable-string-converter Integer mn mx :nil)))))

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

(defn setup-number-textfield! 
  "Sets up properties and listeners on textfield so it shows green
  while editing proper numerical values, yellow while editing
  out-of-range values, and red while editing nonparsable text.
  textfield is self-described.  Lower and upper set the valid range.
  convspec is either an instance of a StringConverter, or a numeric
  Type such as Long.  Rng is the range check function. Returns
  textfield."
  ([textfield cnvspec rngfn]
   (let [numtype? (not (instance? javafx.util.StringConverter cnvspec))
         converter (condp = cnvspec
                     Integer (nullable-string-converter Integer) ;; because it's a macro...
                     Long (nullable-string-converter Long)
                     Double (nullable-string-converter Double)
                     cnvspec)
         formatter (and numtype? (javafx.scene.control.TextFormatter. converter))]
     (when numtype?
       (set-prop-val! textfield :text-formatter formatter))
     (add-listener! textfield :focused txt-defocus-listener)
     (add-listener! textfield :text (err-range-listener converter rngfn))
     (.setOnKeyPressed textfield (enable-editing-pseudostate textfield))
     textfield))
  ([textfield cnvspec lower upper]
   (let [rngfn #(number-range-check % lower upper)]
     (setup-number-textfield! textfield cnvspec rngfn))))


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
        invalid-listener (invalidation-listener
                          (let [newvalue (.getValue observable)
                                spinner (.getBean observable)
                                converter (.. spinner getValueFactory getConverter)]
                            (.setText (.getEditor spinner) (.toString converter newvalue))))

        ;; ValueFactories return either converted value or nil
        ;; Valid range is not checked in value factory
        tgt-spmmvf (spinner-distance-value-factory mm)
        tgt-spmilvf (spinner-distance-value-factory mil)
        tgt-spmm (lu "sp-major-grid-spacing-mm")
        tgt-spmil (lu "sp-major-grid-spacing-mils")]

    (.setValueFactory tgt-spmm tgt-spmmvf)
    (.setValueFactory tgt-spmil tgt-spmilvf)
    (doseq [tgt [tgt-spmm tgt-spmil]]
      (.setEditable tgt true)
      (add-listener! tgt :value invalid-listener)
      (setup-number-textfield! (.getEditor tgt) (.. tgt getValueFactory getConverter) drc))

    (bind! :var state, :init (um (mm 10)), :keyvec [:major-grid-spacing-um]
           :no-action-val nil
           :property :value
           :range-fn #(um (drc-clip %))
           :prop-to-var-fn prop-to-var-fn
           :targets {tgt-spmmvf {:var-to-prop-fn mm}
                     tgt-spmilvf {:var-to-prop-fn mil}})))

(defn setup-minor-grid-per-major-grid-spinner! [state lu]
  (let [lower (int 2) ;; use int because there is no LongSpinnerValueFactory
        upper (int 10)
        tgt-spgpmvf (integer-spinner-value-factory lower upper)
        tgt-spgpm (doto (lu "sp-minor-gpm") (.setValueFactory tgt-spgpmvf))]
    (.setEditable tgt-spgpm true)
    (setup-number-textfield! (.getEditor tgt-spgpm) (.getConverter tgt-spgpmvf) lower upper)
    (bind! :var state, :init lower, :keyvec [:minor-gpm]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets [tgt-spgpmvf])  ))

(defn setup-overall-zoom-slider-and-text! [state lu]
  (let [lower 5
        upper 200
        slider (lu "sl-zoom-ppu")
        textfield (lu "tf-zoom-ppu")]
    (setup-number-textfield! textfield Long lower upper)
    (doto slider
      (.setMin lower)
      (.setMax upper)
      (.setMajorTickUnit 50)
      (.setMinorTickCount 9)
      (.setShowTickMarks true)
      (.setShowTickLabels true)
      jfxutils.core/integer-slider)

    ;; We can't rely on the slider's clipping/limiting functionality
    ;; since the slider doesn't let us know the setValue has been clipped
    (bind! :var state, :init 10, :keyvec [:zoom-ppu]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets {(get-prop-val textfield :text-formatter) {} ;; no prop-to-var-fn because converter returns proper value or nil
                     slider {:prop-to-var-fn long}}))) ;; slider returns doubles, so need to convert

(defn setup-zoom-level-range-text! [state lu]
  (let [minlower -400
        mininit -200
        minupper 0
        maxlower 0
        maxinit 200
        maxupper 400
        max-textfield (lu "tf-zoom-range-max")
        min-textfield (lu "tf-zoom-range-min")]

    (setup-number-textfield! min-textfield Long minlower minupper)
    (setup-number-textfield! max-textfield Long maxlower maxupper)

    (bind! :var state, :init mininit, :keyvec [:zoom-range-min]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % minlower minupper :clip)
           :targets [(get-prop-val min-textfield :text-formatter)])
    
    (bind! :var state, :init maxinit, :keyvec [:zoom-range-max]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % maxlower maxupper :clip)
           :targets [(get-prop-val max-textfield :text-formatter)])))

(defn setup-major-grid-line-width-slider-and-text! [state lu]
  (let [lower 0.1
        upper 20.0
        slider (lu "sl-major-grid-line-width")
        textfield (lu "tf-major-grid-line-width")]
    (setup-number-textfield! textfield Double lower upper)
    (doto slider
      (.setMin lower)
      (.setMax upper)
      (.setMajorTickUnit 5)
      (.setMinorTickCount 3)
      (.setShowTickMarks true)
      (.setShowTickLabels true)
      (.setBlockIncrement 1.0)
      (jfxutils.core/decimal-slider 0.25))

    ;; We can't rely on the slider's clipping/limiting functionality
    ;; since the slider doesn't let us know the setValue has been clipped
    (bind! :var state, :init 1.00, :keyvec [:major-glw]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets [(get-prop-val textfield :text-formatter)
                     slider])))

(defn setup-major-grid-dot-width-slider-and-text! [state lu]
  (let [lower 0.1
        upper 20.0
        slider (lu "sl-major-grid-dot-width")
        textfield (lu "tf-major-grid-dot-width")]
    (setup-number-textfield! textfield Double lower upper)
    (doto slider
      (.setMin lower)
      (.setMax upper)
      (.setMajorTickUnit 5)
      (.setMinorTickCount 3)
      (.setShowTickMarks true)
      (.setShowTickLabels true)
      (.setBlockIncrement 1.0)
      (jfxutils.core/decimal-slider 0.25))

    ;; We can't rely on the slider's clipping/limiting functionality
    ;; since the slider doesn't let us know the setValue has been clipped
    (bind! :var state, :init 1.00, :keyvec [:major-gdw]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets [(get-prop-val textfield :text-formatter)
                     slider])))

(defn setup-minor-grid-line-width-slider-and-text! [state lu]
  (let [lower 0.1
        upper 20.0
        slider (lu "sl-minor-grid-line-width")
        textfield (lu "tf-minor-grid-line-width")]
    (setup-number-textfield! textfield Double lower upper)
    (doto slider
      (.setMin lower)
      (.setMax upper)
      (.setMajorTickUnit 5)
      (.setMinorTickCount 3)
      (.setShowTickMarks true)
      (.setShowTickLabels true)
      (.setBlockIncrement 1.0)
      (jfxutils.core/decimal-slider 0.25))

    ;; We can't rely on the slider's clipping/limiting functionality
    ;; since the slider doesn't let us know the setValue has been clipped
    (bind! :var state, :init 1.00, :keyvec [:minor-glw]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets [(get-prop-val textfield :text-formatter)
                     slider])))

(defn setup-minor-grid-dot-width-slider-and-text! [state lu]
  (let [lower 0.1
        upper 20.0
        slider (lu "sl-minor-grid-dot-width")
        textfield (lu "tf-minor-grid-dot-width")]
    (setup-number-textfield! textfield Double lower upper)
    (doto slider
      (.setMin lower)
      (.setMax upper)
      (.setMajorTickUnit 5)
      (.setMinorTickCount 3)
      (.setShowTickMarks true)
      (.setShowTickLabels true)
      (.setBlockIncrement 1.0)
      (jfxutils.core/decimal-slider 0.25))

    ;; We can't rely on the slider's clipping/limiting functionality
    ;; since the slider doesn't let us know the setValue has been clipped
    (bind! :var state, :init 1.00, :keyvec [:minor-gdw]
           :property :value
           :no-action-val nil
           :range-fn #(number-range-check % lower upper :clip)
           :targets [(get-prop-val textfield :text-formatter)
                     slider])))


(defn GridSettingsPane [name]
  (let [state (atom {})]
    (def v state) ;; for use in repl
    (let [root (doto (load-fxml-root "GridSettingsPane.fxml"))
          lu (fn [id] (lookup root (join-hyph name id)))]
      (update-names! root name)

      (setup-grid-enable-checkboxes! state lu)
      (setup-major-grid-spacing-spinners! state lu)
      (setup-minor-grid-per-major-grid-spinner! state lu)
      (setup-overall-zoom-slider-and-text! state lu)
      (setup-zoom-level-range-text! state lu)
      
      (setup-major-grid-line-width-slider-and-text! state lu )
      (setup-major-grid-dot-width-slider-and-text! state lu)
      (setup-minor-grid-line-width-slider-and-text! state lu)
      (setup-minor-grid-dot-width-slider-and-text! state lu)

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

  






















