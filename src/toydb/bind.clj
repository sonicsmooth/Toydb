;; Implement binding fns
(ns toydb.bind
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


(defn- update-targets!
  "Sends newval to all targets in tgts-map, passing the value through
  var-to-prop-fn when supplied in tgts-map.  If :no-action is also
  specified in tgts-map, and var-to-prop-fn returns such a value, then
  the value is not propagated to the targets, ie 'nothing happens'."

  [tgts-map newval]
  (doseq [[tgt properties] tgts-map]
    (if-let [v2p (:var-to-prop-fn properties)]

      ;; var-to-prop is specified and not nil
      (let [v2pval (v2p newval)
            take-action? (nil? (find properties :no-action))]
        (if take-action?
          (set-prop-val! tgt (:property properties) v2pval)
          (let [noaction-value (:no-action properties)]
            (when-not (= v2pval noaction-value)
              (set-prop-val! tgt (:property properties) v2pval)))))

      ;; var-to-prop is nil or not specified, so just set property to
      ;; newval directly
      (set-prop-val! tgt (:property properties) newval))))

(defn invalidate! [v]
  (swap! v assoc :dirty true))

(defn validate! [v]
  (swap! v assoc :dirty false))

(defn bind!
  "Bind a clojure var with one or more javafx properties.  If you want
  to bind JavaFX properties only, with no Clojure var, then do not use
  this function; use JavaFX Bindings instead.  Arguments are
  keyword-value pairs.  The required keywords
  are :var, :keyvec, :targets, and :property.  Everything else is
  optional.  By default, each property is both a sink and a source of
  change events, but can be changed to sink only, aka :terminal.  This
  function does not return a value.

  :var is the var (currently only atoms are supported). 

  :keyvec is the vector of keys which access the value inside the var,
  same as get-in or assoc-in.

  :property is the property of the javafx Nodes to bind to.

  :targets is either a seq of javafx Nodes or a map with keys which
  are javafx Nodes.  If a seq, then each target Node shares the same
  binding parmeters.  If a map, then each target can have its own
  binding parmaeters.  If a :keyword is specified generally and also
  inside the :targets map, then the one in the targets map takes
  precedence.

  :terminal is a bool indicating whether the property should only
  accept changes and cannot generate changes.  A ChangeListener will
  not be installed on this Node.  An example would be a read-only text
  field or label which reports the value of the var.  If you
  programmatically set this value outside the binding mechanism, the
  change will not propagate.

  :var-to-prop-fn is a function taking one arg and returning one
  value, which is called when the var changes, prior to being assigned
  to the property's value.  If nil or omitted, this fn is not called
  and the value of the var is written directly to the property.  This
  function may return the value of :no-action if it determines no
  state should change.

  :prop-to-var-fn is a function taking one arge and returning one
  value, which is called when the target's value changes, prior to
  being assigned to the var's value.  If nil or omitted, this fn is
  not called and the value of the property is written directly to the
  var.  This function may return the value of :noaction if it
  determines no state should change.

  :var-fn! is a function taking one arg, which is used to write the new
  value to the var, instead of the default swap!.  Its return value is
  ignored.  If prop-to-var-fn is specified, then the value passes
  through that function first before being passed to var-fn.

  :no-action is the value which will be returned from prop-to-var-fn
  or var-to-prop-fn which results in no changes being propagated. For
  example if this is set to :bluberries, then prop-to-var-fn and
  var-to-prop-fn must return :blueberries as an indictor to the
  binding system *not* to propagate the change.  This value can be
  specified as nil of course, in which case the fns must return nil
  for nothing to happen.  If :no-action is left unspecified, then
  :blueberries or nil or whatever will be the value propagated to
  the var and other targets.

  :init is the value which is swap!'ed or var-fn'd into var after
  everything is set up.  This is intended to initialize the targets
  with a known value.  The propagation of this value ignores the
  no-action value.  If :init is not specified, then the var remains at
  its previous value (including possibly not even existing in the
  atom), and the Node properties are left alone."

  [& kvps]
  (let [opts (apply hash-map kvps)
        ;; These are the required keys
        {:keys [var keyvec targets property]} opts

        ;; This is a map with the remaining keys
        optionals (dissoc opts :var :keyvec :targets :property)

        ;; Create a common map which occurs in all the values of the
        ;; targets-map.  The keys of the targets-map are either the
        ;; keys of targets if targets is a map, or the elements of
        ;; targets if targets is a seq.  The values of targets-map is
        ;; either the merge of each original value in targets with the
        ;; common map if targets is a map, or the common map if
        ;; targets is a seq.
        common-map (merge {:property property} optionals)
        target-keys (if (map? targets) (keys targets) targets)
        target-vals (if (map? targets)
                      (map merge (repeat common-map) (vals targets))
                      (repeat common-map))
        targets-map (let [buncha-maps (map hash-map target-keys target-vals)]
                      (into {} buncha-maps)) ;; returns single map
        swapper! (or (:var-fn! opts)
                     #(swap! var assoc-in keyvec %))]

    ;; Add watch to atom to update all the objects. jfx properties
    ;; won't call change listener if the value is programmatically set
    ;; to what is already there.  Clojure atom will call watch even if
    ;; it's the same value. targets-map is the unique key for the
    ;; watch; using something simple like :id didn't work because
    ;; later watches would overwrite earlier ones.

    ;; This whole thing assumes the value in the var is the "real"
    ;; value, whatever that is.  The properties in the Nodes are
    ;; somehow "representations" of this value.  For example the var
    ;; can be 1000um, but the property can be "1.0mm", "1.mm", "1mm",
    ;; "1e-3m", "1e-6km", etc.  Of course the var and the property can
    ;; be the same thing, such as a Color object, etc.
    (add-watch var targets-map 
               (fn [_key _ref oldmap newmap]
                 (let [oldval (get-in oldmap keyvec)
                       newval (get-in newmap keyvec)
                       dirty? (true? (:dirty newmap))]
                   (when (or (not (= oldval newval))
                             dirty?)
                     (update-targets! targets-map newval)
                     (validate! var)))))

    ;; For each target in tuple, add listener to property of targety
    (doseq [[target properties] targets-map]
      (when-not (:terminal properties) ;; Only if :terminal if falsey
        (add-listener! target (:property properties)
                       (change-listener [oldval newval] ;; oldval and newval are from property
                                        
                                        ;; The comparison should happen in the var space, ie "real" units.
                                        (let [p2v (or (:prop-to-var-fn properties) identity)
                                              oldreal (p2v oldval)
                                              newreal (p2v newval)
                                              take-action? (nil? (find properties :no-action))]
                                          (if take-action?
                                            (swapper! newreal)
                                            (let [noaction-value (:no-action properties)]
                                              (when-not (= newreal noaction-value)
                                                (swapper! newreal)))))))))

    ;; Set var to init val and force targets anyway, so things may get triggered twice
    (when-let [init (:init optionals)]
      (swapper! init))))



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
        ;;txt1 (javafx.scene.control.TextField.)
        ;;txt2 (javafx.scene.control.TextField.)
        ;;txt3 (javafx.scene.control.TextField.)
        txt1 (jfxnew javafx.scene.control.TextField :text-formatter tf1)
        txt2 (jfxnew javafx.scene.control.TextField :text-formatter tf2)
        txt3 (jfxnew javafx.scene.control.TextField :text-formatter tf3)
        txts [txt1 txt2 txt3]
        nodes (concat sliders txts)
        targets (concat sliders tfs)
        state (atom nil)
        vb (jfxnew VBox :children nodes)

        ;; Anything with nil can be omitted, excepl :no-actior
        mb (bind! 
            :init 0.0  ;; starting value
            :var state ;; atom
            :var-fn! nil ;; use this fn with new value instead of swap! to change var
            :keyvec [:field1] ;; how to get-in the value from the var

            ;; Specify multiple targets and a common property
            :property :value
            :targets targets
            ;;:no-action nil      ;; The value returned by below fns indicating swap! should not occur
            ;;:var-to-prop-fn nil ;; Convert var value before setting property
            ;;:prop-to-var-fn nil ;; Convert property value before setting val
                  
            ;; Or specify multiple targets, properties, and fns
            #_:targets #_{slider1 {:property :value}
                      slider2 {:property :value}
                      slider3 {:property :value}
                      tf1 {:property :value}
                      tf2 {:property :value}
                      tf3 {:property :value}}



            )]

    ;; Need to change it so instead of :targets [{:target target,
    ;; :property prop}], it's :targets {target {:property prop, :var2prop v2p}}, etc.
    
    (stage vb [640 400])
    state))




#_(defn -main [& args]
  (def state (bind-test)))











