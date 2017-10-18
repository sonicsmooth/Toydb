;; Implement binding fns
(ns toydb.bind.bind
  (:use [jfxutils.core :exclude [-main] ]
        [clojure.pprint :as pp])
  (:import [javafx.scene Group Scene Node]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button Slider Label]
           [javafx.geometry Point2D Insets]
           [javafx.scene.input MouseEvent MouseButton ScrollEvent KeyEvent]
           [javafx.scene.layout BorderPane Region HBox VBox Priority StackPane Pane
            Background BackgroundFill CornerRadii
            Border BorderStroke BorderStrokeStyle BorderWidths ]
           [javafx.scene.shape Rectangle Line Circle]
           [javafx.scene.paint Color]
           [javafx.scene.text Font]
           [javafx.scene.transform Transform Affine Rotate Scale Translate]
           [javafx.stage Stage]
           [javafx.beans.binding Bindings]))

(defn- map-cleaner
  "Returns maps cleaned of its nil values"
  [m]
  (apply hash-map
         (apply concat
                (filter #(not (nil? (val %))) m))))


(defn- make-target
  "Takes a target, a property, and key-value pairs.  Returns a map
  with target as key, and value an inner map with a :property value
  and the remaining key-value pairs as entries in the inner map.  The
  inner map will contain no nil entries."
  [tgt property & kvps]
  (let [val (map-cleaner (apply hash-map kvps))
        val (merge {:property property} val)]
    {tgt val}))

(defn- clean
  "Targets is a map where each val is an inner map.  Removes nil
  entries from inner maps."
  [targets]
  (let [clean-vals (map map-cleaner (vals targets))
        kv (interleave (keys targets) clean-vals)]
    (apply hash-map kv)))

(defn- update-targets! [tgts-map newval]
  (doseq [[tgt props] tgts-map]
    (if-let [v2p (:var-to-prop-fn props)]
      (set-prop-val! tgt (:property props) (v2p newval))
      (set-prop-val! tgt (:property props) newval))))


(defn bind!
  "Bind a clojure var with one or more javafx properties.  If you want
  to bind JavaFX properties only, with no Clojure var, then do not use
  this function; use JavaFX Bindings instead.  var is the
  var (currently only atoms are supported). keyvec is the vector of
  keys which access the value inside the var, same as get-in or
  assoc-in.  targets is seq of javafx Nodes.  property is the property
  common to all the nodes.  terminal is a bool indicating whether the
  property should only accept changes and cannot generate changes.
  var-to-prop-fn is the function that is called when the var changes,
  prior to being assigned to the property's value.  prop-to-var-fn is
  the function that is called when the target's value changes, prior
  to being assigned to the var's value.  If nil or omitted, these fns
  are not called.  If there are different specs required for the
  properties, then targets must be a clojure map with :target, :property, 
  :terminal (optional), :var-to-prop-fn (optional), and :prop-to-var-fn
  (optional) fields.  A :terminal field set to true will cause this
  function NOT to install a listener on the node.  Atom is normally
  set using swap!, but if a different fn is desired, then pass :var-fn
  arg.  Atom is assoc'd to init using keys after everything is set up.
  Additional arguments apply as pairs in key-value map."
  [& kvps]
  (let [opts (apply hash-map kvps)
        {:keys [init var var-fn keyvec targets property]} opts
        extra-opts (dissoc opts :init :var :var-fn :keyvec :targets :property)
        ;; Update so common things like :property can be specified
        ;; outside individual target maps
        targets-map (if (map? targets)
                      (clean targets) ;; individually specified params

                      ;; Otherwise, generate a map with given targets as
                      ;; keys and value being the same inner map, and any
                      ;; other key-value pairs given as args.
                      (apply merge (map #(apply make-target %1 property %2) targets
                                        (repeat (apply concat extra-opts)))))]

    ;; Add watch to atom to update all the objects. jfx properties
    ;; won't call change listener if it's the same value.  Clojure
    ;; atom will call watch even if it's the same value. targets-map is
    ;; the unique key for the watch; using something simple like :id
    ;; didn't work because later watches would overwrite earlier ones
    (add-watch var targets-map 
               (fn [_key _ref oldmap newmap]
                 (let [oldval (get-in oldmap keyvec)
                       newval (get-in newmap keyvec)]
                   (when (not= oldval newval)
                     (update-targets! targets-map newval)))))

    ;; For each target in tuple, add listener to property of target
    ;; Skip if :terminal == false
    (doseq [[target properties] targets-map]
      (when-not (:terminal properties)
        (add-listener! target (:property properties)
                       (change-listener [oldval newval] ;; oldval and newval are from property
                                        (when (and (not= oldval newval)
                                                   ;;(not= newval (get-in @var keyvec)) ;; If uncommenting, compare to converted value
                                                   ) ;; don't call if var already set

                                          ;; Update newval with fn if provided
                                          (let [newval (if-let [p2v (:prop-to-var-fn properties)]
                                                         (p2v newval)
                                                         newval)]

                                            ;; Call swap! directly, or use fn if provided
                                            (if-let [var-fn (:var-fn opts)]
                                              (var-fn newval)
                                              (swap! var assoc-in keyvec newval))))))))



    ;; Set var to init val and force targets anyway, so things may get triggered twice
    (swap! var assoc-in keyvec init)
    (update-targets! targets-map init)))



;; Do something to validate values when they are set externally via swap
;; Do something to deal with snapping to nearest value, as in zoom slider
;; var-to-prop-fn and prop-to-var-fn return nil if they can't convert

(defn- make-slider [id]
  (jfxnew Slider -20 20 0.0
          :block-increment 5
          :show-tick-marks true
          :show-tick-labels true
          :major-tick-unit 5
          :pref-width 350
          :id id))

(defn bind-test []
  (let [slider1 (make-slider "slider1")
        slider2 (make-slider "slider2")
        slider3 (make-slider "slider3")
        sliders [slider1 slider2 slider3]
        tf1 (javafx.scene.control.TextFormatter. (javafx.util.converter.DoubleStringConverter.))
        tf2 (javafx.scene.control.TextFormatter. (javafx.util.converter.DoubleStringConverter.))
        tf3 (javafx.scene.control.TextFormatter. (javafx.util.converter.DoubleStringConverter.))
        tfs [tf1 tf2 tf3]
        txt1 (javafx.scene.control.TextField.)
        txt2 (javafx.scene.control.TextField.)
        txt3 (javafx.scene.control.TextField.)
        ;;txt1 (jfxnew javafx.scene.control.TextField :text-formatter tf1)
        ;;txt2 (jfxnew javafx.scene.control.TextField :text-formatter tf2)
        ;;txt3 (jfxnew javafx.scene.control.TextField :text-formatter tf3)
        txts [txt1 txt2 txt3]
        nodes (concat sliders txts)
        targets (concat sliders tfs)
        state (atom nil)
        vb (jfxnew VBox :children nodes)

        ;; Anything with nil can be omitted
        mb (bind! 
                  :init 0.0            ;; starting value
                  :var state           ;; atom
                  :var-fn nil          ;; use this fn with new value instead of swap! to change var
                  :keyvec [:field1]    ;; how to get-in the value from the var

                  ;; Specify multiple targets and a common property
                  :property :value
                  :targets targets
                  :var-to-prop-fn nil ;; Convert var value before setting property
                  :prop-to-var-fn nil ;; Convert property value before setting val
                  
                  ;; Or specify multiple targets, properties, and fns
                  #_:targets #_{slider11 {:property :value, :var-to-prop-fn nil, :prop-to-var-fn nil}
                                slider2  {:property :value}
                                slider3  {:property :value}
                                tfs1 {:property :text, etc etc}
                                tfs2 {:property :text, etc etc}
                                tfs3 {:property :text, etc etc}}



                  )]

    ;; Need to change it so instead of :targets [{:target target,
    ;; :property prop}], it's :targets {target {:property prop, :var2prop v2p}}, etc.
    
    (stage vb [640 400])
    state))




#_(defn -main [& args]
  (def state (bind-test)))












