(ns toydb.editor
  (:use [jfxutils.core :exclude [-main] ]
        [clojure.pprint])
  (:require [toydb
             [canvas :as canvas]
             [grid :as grid]
             [menubars :as mb]]
            [docks.core :as docks]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as matrixop])
  (:require [toydb.viewdef :as viewdef] )
  (:import [javafx.scene Group Scene]
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

(set! *warn-on-reflection* false)
(set! *unchecked-math* false)

(defn zsc [node sc x y]
  ;; Changes the built-in scale[XY] and translate[XY] properties, not
  ;; the affine transform.
  ;;(set-xy! node :scale sc)
  (set-xy! node :translate (Point2D. x y))
  (set-scale! node sc)
  ;;(set-translate! node x y)
  )

#_(defn getxy [^javafx.geometry.Point2D pt]
  [(.getX pt) (.getY pt)])

#_(defn point ^Point2D [^javafx.scene.input.MouseEvent evt]
  (Point2D. (.getX evt) (.getY evt)))

#_(defn transpoint ^Point2D [^javafx.scene.Node node]
  (Point2D. (.getTranslateX node) (.getTranslateY node)))

(def bs (BorderStroke. Color/RED, BorderStrokeStyle/SOLID, CornerRadii/EMPTY BorderWidths/DEFAULT ))
(def border (Border. (into-array [bs])))


(defn _make-idfn
  "Returns a function that appends the given id string to another
  given string"
  [id]
  (if id #(str % "-" id)
      identity))

(def make-idfn (memoize _make-idfn))

(defn capture-mouse!
  ;; Used to capture movements for scrolling and dragging, etc.
  ;; Down -- remember click position
  ;; Up, DownUp, Drag -- Calculate delta position between now and last down click
  ;; Capture mouse movement in both pixel and unit space
  ([event mouse-state ^toydb.viewdef.ViewDef view]
   ;; mouse-state is an atom with nil or a map
   (let [[x y] [(.getX event) (.getY event)]
         state @mouse-state
         etype (.getEventType event)

         ;; Copture current
         evt-px (Point2D. x y)
         evt-u (viewdef/pixels-to-units view evt-px)

         ;; Recall previous
         ;; position at last click, could be far away
         click-px  (if state (:click-px state) evt-px)
         click-u (if state (:click-u state) evt-u)

         ;; position at last move, typically close by; we want this to be "this point" at first start
         move-px (if state (:move-px state) evt-px) 
         move-u (if state (:move-u state) evt-u)

         ;; Update deltas
         ;; distance now from last click, could be large
         click-dpx (.subtract evt-px click-px)
         click-du (.subtract evt-u click-u)

         ;; distance now from last move, typically small
         move-dpx (.subtract evt-px move-px)
         move-du (.subtract evt-u move-u)

         ;; Update "last" values to be the new ones for next time
         ;; For click, only update if clicked, otherwise use previous value
         nxt-click-px (if (= etype MouseEvent/MOUSE_PRESSED)
                        evt-px
                        click-px)
         nxt-click-u (if (= etype MouseEvent/MOUSE_PRESSED)
                       evt-u
                       click-u)

         ;; Put them all together
         new-poses {:click-px nxt-click-px
                    :click-u nxt-click-u
                    :click-dpx click-dpx
                    :click-du click-du
                    :move-px evt-px
                    :move-u evt-u
                    :move-dpx move-dpx
                    :move-du move-du}
         kbs {:keys (+ (if (.isControlDown event) 1 0)
                       (if (.isAltDown event)     2 0)
                       (if (.isShiftDown event)   4 0))
              :button (.getButton event)
              :buttons (+ (if (.isPrimaryButtonDown   event) 1 0)
                          (if (.isMiddleButtonDown event)    2 0)
                          (if (.isSecondaryButtonDown event) 4 0))}]
     (swap! mouse-state merge new-poses kbs))))

(def ___ 0)
(def L__ 1)
(def _M_ 2)
(def LM_ 3)
(def __R 4)
(def L_R 5)
(def _MR 6)
(def LMR 7)

;; Moving and modifying a node is none of the following: setting the
;; TranslateX/Y properties, setting the Affine transform, Relocating
;; it, or setting its LocationX/Y.  Moving and modifying is about
;; changing the points which define the geometry.  The methods to do
;; this vary amongst the various shapes.

(defprotocol ModifyProtocol
  (set-pos! [node, newloc])     ;; changes start/end/center points, etc.
  #_(add-pos! [node, dloc])
  (get-pos  [node])
  (reshape! [node, notsure])) ;; changes width, height, radius, etc.

(extend-protocol ModifyProtocol
  javafx.scene.shape.Circle
  (set-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D newloc]
    (set-xy! node :center newloc))
  #_(add-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D dloc]
    (set-pos! node (+p (get-xy node :center) dloc)))
  (get-pos [^javafx.scene.Node node]
    (get-xy node :center))

  javafx.scene.shape.Line
  (set-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D newloc]
    (let [oldstart (get-xy node :start)
          oldend (get-xy node :end)
          dline (.subtract oldend oldstart)
          newend (.add newloc dline)]
      (doto node
        (set-xy! :start newloc)
        (set-xy! :end newend))))
  #_(add-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D dloc]
    (set-pos! node (+p (get-xy node :start) dloc)))
  (get-pos [^javafx.scene.Node node]
    (get-xy node :start))

  javafx.scene.shape.Rectangle
  (set-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D newloc]
    (set-xy! node newloc))
  #_(add-pos! [^javafx.scene.Node node, ^javafx.geometry.Point2D dloc]
    (set-pos! node (+p (get-xy node) dloc)))
  (get-pos [^javafx.scene.Node node]
    (get-xy node)))

(defn capture-move-state!
  "Remembers target's TranslateX/Y value."
  [move-state target]
  ;;(println "capture-move-state as " (get-pos target))
  (swap! move-state assoc :old-xy (get-pos target)))
(defn clear-move-state!
  "Clear target's Translate X/Y value."
  [move-state]
  (reset! move-state nil))

#_(defprotocol EditorProtocol
  "Function for Editor defrecord"
  (show [this]))
#_(defrecord Editor [top-pane, docs, behaviors] 
  EditorProtocol
  (show [this] (stage (:top-pane this))))

;; in these fns, swap! the view data from a given doc. The watch will respond and update the drawing
(defprotocol DocumentProtocol
  "Functions for Document defrecord"
  (lookup-node [this id])                             ;; Returns whatever node is called for
  (reset-view! [this])                                ;; mutates view, triggers watch
  (redraw-view! [this])                               ;; called from watch
  (transscale-entities! [this])                       ;; called from redraw-view!
  (pan-to! [this point2d])                            ;; mutates view, triggers watch
  (pan-by! [this dpoint2d])                           ;; mutates view, triggers watch
  (zoom-to! [this zoom-level] [this zoom-level pt])   ;; mutates view, triggers watch
  (zoom-by! [this dzoom-level] [this dzoom-level pt]) ;; mutates view, triggers watch
  (resize! [this [oldw oldh] [neww newh]])            ;; resizes window
  (init-handlers! [this])) 




(defrecord Doc-View [viewdef ;; zoom, pan, grid spec
                     doc-pane ;; holds everything, including toolbar, status bar, menu
                     uuid     ;; unique identifier for this instance
                     behaviors
                     mouse-state ;; captures old and new mouse positions
                     move-state] ;; captures object position when moving
  DocumentProtocol
  ;; Anything with swap! leads to redraw-view! via the watch
  (lookup-node [doc id]
    (jfxutils.core/lookup ((make-idfn (:uuid doc)) id) (:doc-pane doc)))

  (reset-view! [doc]
    (let [canvas (lookup-node doc "grid-canvas")
          [width height] [(.getWidth canvas) (.getHeight canvas)]
          view-atom (:viewdef doc)
          view @view-atom]
      (swap! view-atom viewdef/reset-viewdef width height)))

  (pan-to! [doc point2d]
    (swap! (:viewdef doc) viewdef/pan-to point2d))

  (pan-by! [doc dpoint2d]
    (swap! (:viewdef doc) viewdef/pan-by dpoint2d))

  (resize! [doc oldsize newsize]
    (swap! (:viewdef doc) viewdef/resize oldsize newsize @(:mouse-state doc))
    ;;(swap! (:viewdef doc) map-replace (:viewdef doc))
    )

  (zoom-to! [doc zoom-level]
    (swap! (:viewdef doc) viewdef/zoom-to zoom-level)) ;; Set zoom level with respect to origin

  (zoom-to! [doc zoom-level pt]
    (swap! (:viewdef doc) viewdef/zoom-to zoom-level pt)) ;; Set zoom level centered on position pt

  (zoom-by! [doc dzoom-level]
    (swap! (:viewdef doc) viewdef/zoom-by dzoom-level)) ;; Zoom by a certain amount centered on origin

  (zoom-by! [doc dzoom-level pt]
    (swap! (:viewdef doc) viewdef/zoom-by dzoom-level pt)) ;; Zoom by a certain amount centered on point

  (redraw-view! [doc]
    (if-let [canvas (lookup-node doc "grid-canvas")]
      (toydb.grid/draw-grid! canvas @(:viewdef doc)))
    (transscale-entities! doc))

  (transscale-entities! [doc]
    ;; scales and translates entities-group for zoom and pan
    (let [viewdef @(:viewdef doc)
          view-xfrm (:transform viewdef)
          entities-group (lookup-node doc "entities-group")
          eg-xfrm (first (.getTransforms entities-group))]
      ;; Is there some way to get the matrix and send it to .setWhatever directly?
      (.setMxx eg-xfrm (matrix/mget view-xfrm 0 0))
      (.setMyy eg-xfrm (matrix/mget view-xfrm 1 1))
      (.setTx eg-xfrm (matrix/mget view-xfrm 0 2))
      (.setTy eg-xfrm (matrix/mget view-xfrm 1 2))))
  
  (init-handlers! [doc]
    ;; Add mouse events to deal with pan, zoom, drag
    (let [ ;;^javafx.scene.canvas.Canvas canvas (lookup-node doc "grid-canvas")
          ;;^javafx.scene.Node doc-pane (:doc-pane doc) ;; Borderpane holding canvas + toolbars
          ^javafx.scene.Node surface-pane (lookup-node doc "surface-pane" )
          ^javafx.scene.Node entities-pane (lookup-node doc "entities-pane")
          mouse-state (:mouse-state doc)
          move-state (:move-state doc)
          snapfn #(viewdef/units-to-snapped-units @(:viewdef doc) %)

          ;; keyboard state is probably not necessary since modifier
          ;; keys are available with every mouse event, which is a
          ;; GestureEvent
          kbd-state (atom {}) ;; Should move this to doc like mouse-state?
          capture-kbd! (fn [^KeyEvent event]
                         ;; Capture state of modifier keys
                         (swap! kbd-state assoc
                                :ctrl (.isControlDown event)
                                :shift (.isShiftDown event)
                                :alt (.isAltDown event)))

          long-id (fn [idtxt] (str idtxt "-" (:uuid doc)))
          lookup-node (memoize lookup-node)

          ;; For display of coordinates in status bar
          move-handler (event-handler [mouse-event]
                                      (let [^MouseEvent event mouse-event
                                            ppos (Point2D. (.getX event) (.getY event))
                                            upos (viewdef/pixels-to-units @(:viewdef doc) ppos)
                                            snupos (viewdef/pixels-to-snapped-units @(:viewdef doc) ppos)
                                            ppos-label (lookup-node doc "pixel-pos-label")
                                            upos-label (lookup-node doc "unit-pos-label")]
                                        ;; Apparently .setText calls canvas resize somehow
                                        ;; Do an if-else to check for whether snap-to-grid is enabled
                                        (.setText ppos-label (format "px:% 5.0f, py:% 5.0f" (.getX ppos) (.getY ppos)))
                                        (.setText upos-label (format "ux:%-10.5g, uy:%-10.5g" (.getX snupos) (.getY snupos)))))

          click-down-handler (event-handler [mouse-event] 
                                            (let [#_^MouseEvent event #_mouse-event
                                                  target (.getTarget mouse-event)]
                                              (capture-mouse! mouse-event mouse-state @(:viewdef doc))
                                              (if (and (= (:buttons @mouse-state) L__)
                                                       (not= target entities-pane))
                                                (capture-move-state! move-state target)
                                                (clear-move-state! move-state))
                                              (.requestFocus surface-pane)))
          click-up-handler (event-handler [mouse-event] 
                                          (capture-mouse! mouse-event mouse-state @(:viewdef doc))
                                          ;; Select when distance from last click is zero, and LMB was cause
                                          (when (and (= (:click-dpx @mouse-state) Point2D/ZERO)
                                                     (= (:button @mouse-state) MouseButton/PRIMARY))
                                            ;;(println "selected")
                                            )
                                          (.requestFocus surface-pane))

          drag-handler (event-handler [mouse-event]
                                      (.handle move-handler mouse-event) ;; update status bar text
                                      (capture-mouse! mouse-event  mouse-state @(:viewdef doc))
                                      (let [ms @mouse-state
                                            mv @move-state]
                                        (condp = (:buttons ms)
                                          ;; Item moves: add movement-since-click to old position, then snap
                                          L__ (when-let [tgtxy (:old-xy mv)]
                                                (set-pos! (.getTarget mouse-event)
                                                          (snapfn (.add tgtxy (:click-du ms)))))

                                          ;; Pan: just tell the doc to move the required pixels
                                          __R (pan-by! doc (:move-dpx ms) )

                                          ;; Zoom: just tell the doc to zoom-on-point the required amount 
                                          L_R (zoom-by! doc (- (.getY (:move-dpx ms))) (:move-px ms))

                                          ;; Otherise, do nothing
                                          (println "Drag condition not handled")))
                                      #_(let [event ^MouseEvent mouse-event]))
          
          scroll-handler (event-handler [scroll-event]
                                        (let [^ScrollEvent event scroll-event
                                              x (.getX event) ;; mouse position
                                              y (.getY event)
                                              dx (.getDeltaX event) ;; scroll amounts
                                              dy (.getDeltaY event)]
                                          (if (.isControlDown event)
                                            (zoom-by! doc (/ dy 10.0) (Point2D. x y))
                                            (pan-by! doc (Point2D. (/ dx 5.0) (/ dy 5.0))))))

          keyboard-handler (event-handler [key-event] (capture-kbd! key-event))
          focus-listener (change-listener [oldval newval] nil)
          reset-btn-handler (event-handler [action-event] (reset-view! doc))
          unit-btn-handler (event-handler [action-event]
                                          (let [vd (:viewdef doc)
                                                id (.. action-event getSource getId)
                                                old-ppg (get-in @vd [:zoom :pixels-per-grid])
                                                to-inch (and (= (:unit @vd) :cm) (= id (long-id "inch-button")))
                                                to-cm (and (= (:unit @vd) :inch) (= id (long-id "cm-button")))
                                                new-ppg (cond to-inch (map * old-ppg [2.54 2.54])
                                                              to-cm (map / old-ppg [2.54 2.54]))
                                                new-unit (cond to-inch :inch
                                                               to-cm :cm)]
                                            (when new-unit
                                              (swap! vd assoc-in-pairs
                                                     [:unit] new-unit
                                                     [:zoom :pixels-per-grid] new-ppg))))]
      
      (add-event-filter! surface-pane MouseEvent/MOUSE_MOVED move-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_PRESSED click-down-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_RELEASED click-up-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_DRAGGED drag-handler)
      (add-event-filter! surface-pane ScrollEvent/SCROLL scroll-handler)
      (set-event-handler! surface-pane :key-pressed keyboard-handler)
      (set-event-handler! surface-pane :key-released keyboard-handler)
      (set-event-handler! (lookup-node doc "reset-button") :action reset-btn-handler)
      (set-event-handler! (lookup-node doc "cm-button" ) :action unit-btn-handler)
      (set-event-handler! (lookup-node doc "inch-button") :action unit-btn-handler)
      (add-listener! surface-pane :focused focus-listener))))


(defn doc-tool-bar
  "Creates document tool bar, where each button has id as suffix"
  ([] (doc-tool-bar nil))
  ([^String uid]
   (let [idfn (make-idfn uid)]
     (mb/tool-bar [(jfxnew Button :id (idfn "cm-button" ) :text "cm")
                   (jfxnew Button :id (idfn "inch-button") :text "inch")
                   (jfxnew Button :id (idfn "reset-button") :text "reset")]))))

(defn editor-tool-bar
  "Creates editor tool bar, where each button has id as suffix."
  ([] (editor-tool-bar nil))
  ([^String uid]
   (let [idfn (make-idfn uid)]
     (mb/tool-bar [(jfxnew Button :id (idfn "edit-1") :text "edit-1")
                   (jfxnew Button :id (idfn "edit-2") :text "edit-2")
                   (jfxnew Button :id (idfn "edit-3") :text "edit-3")]))))

(defn doc-status-bar
  ([] (doc-status-bar nil))
  ([^String uid & {:keys [zoomlimits]}]
   (let [idfn (make-idfn uid)
         mppl (jfxnew Label "doc mouse label" :id (idfn "pixel-pos-label"))
         mupl (jfxnew Label "doc mouse label" :id (idfn "unit-pos-label"))
         spring (Region.)
         zoom-min (or (first zoomlimits) -10)
         zoom-max (or (second zoomlimits) 10)
         zoom-slider1 (jfxnew Slider zoom-min zoom-max 0.0
                              :block-increment 10
                              :show-tick-marks true
                              :show-tick-labels true
                              :major-tick-unit 100
                              :pref-width 350
                              :id (idfn "zoom-slider"))
         zoom-label-txt (jfxnew Label "Zoom Level" :id (idfn "zoom-label"))
         zoom-value-txt (jfxnew Label "" :pref-width 50 :id (idfn "zoom-value"))]
     (HBox/setHgrow spring Priority/ALWAYS)

     ;; Enforce integer slider positions
     (add-listener! zoom-slider1 :value (change-listener [oldval newval]
                                                         (.setValue zoom-slider1 (Math/round newval))))

     ;; Set up local javafx binding between slider position and text
     (.bind (.textProperty zoom-value-txt) (Bindings/format "% 5.0f" (into-array [(.valueProperty zoom-slider1)]) ))
     (mb/status-bar [mppl mupl spring zoom-label-txt zoom-slider1 zoom-value-txt]))))

(defn editor-status-bar
  ([] (editor-status-bar nil))
  ([^String uid]
   (let [idfn (make-idfn uid)
         mouse-label (jfxnew Label "editor mouse label" :id (idfn "mouse-label"))
         spring (Region.)]
     (HBox/setHgrow spring Priority/ALWAYS)
     (mb/status-bar [mouse-label spring]))))

(defn border-pane
  "Creates BorderPane with top menu/toolbar, bottom status bus, and
  background only.  Center content must be set by caller"
  [& {:keys [center top bottom size]}]
  (let [pane (jfxnew BorderPane
                     :center center
                     :top top
                     :bottom bottom)]
    (when (first size) (.setPrefWidth pane (first size)))
    (when (second size) (.setPrefHeight pane (second size)))
    pane))


;; Doc-View is a graphical thing with a grid, not some UI-less abstraction of document
(defn doc-view
  "Make doc-view with relevant handlers"
  ([width height]
   (println "Not accepting width and height")
   (doc-view))
  ([]   
   (let [uid (uuid)
         idfn (make-idfn uid)
         viewdef-atom (atom (viewdef/viewdef))
         zoomlimits (-> @viewdef-atom :zoomspecs :zoomlimits)
         ;; The doc contains the canvas, but the canvas resize
         ;; callback refers to the doc, so we have a circular
         ;; reference so make a quick atom...
         doc-atom (atom nil)
         grid-canvas (canvas/resizable-canvas (fn [canvas, [oldw oldh] [neww newh]]
                                                (when (or (not= oldw neww) (not= oldh newh))
                                                  (resize! @doc-atom [oldw oldh] [neww newh]))))
         ;; Name the canvas so we can get to it later
         _  (.setId grid-canvas (idfn "grid-canvas"))
         twocircles [(jfxnew Circle 0.5 0 0.25 :stroke Color/GRAY :stroke-width 0.025 :fill Color/SILVER)
                     (jfxnew Circle 1.0 0 0.15 :stroke Color/GRAY :stroke-width 0.025 :fill Color/SILVER)
                     ]
         little-group (jfxnew Group
                              :id (idfn "little-group")
                              :children twocircles)
         shapes [(jfxnew Line -1 -1 1 1 :stroke Color/GREEN :stroke-width 0.05)
                 (jfxnew Line -1 1 1 -1 :stroke Color/BLUE :stroke-width 0.05)
                 (jfxnew Circle -1 -1 0.1 :stroke Color/GREEN :stroke-width 0.05)
                 (jfxnew Circle -1 1 0.1 :stroke Color/BLUE :stroke-width 0.05)
                 (jfxnew Circle 0 0 0.1 :stroke Color/CADETBLUE :stroke-width 0.05 :fill Color/BISQUE)
                 (jfxnew Rectangle 0 0 1 1 :stroke Color/AQUAMARINE :stroke-width 0.05 :fill nil)
                 (jfxnew Line -3 0 0 0 :stroke Color/ORANGE :stroke-width 0.1)
                 little-group]
         entities-group (jfxnew Group
                                :id (idfn "entities-group")
                                :children  shapes
                                :transforms [(Affine.)])
         
         entities-pane (jfxnew Pane
                               :id (idfn "entities-pane")
                               :children [entities-group])
         
         surface-pane (make-clipped! (jfxnew StackPane
                                             :id (idfn "surface-pane")
                                             :children [grid-canvas entities-pane]
                                             :background (gradient-background :vertical Color/ALICEBLUE Color/LIGHTBLUE)))
         doc (map->Doc-View {:viewdef viewdef-atom
                             :doc-pane (border-pane ;; holds main grid and decorations
                                        :center surface-pane
                                        :top (doc-tool-bar uid),
                                        :bottom (doc-status-bar uid :zoomlimits zoomlimits))
                             :uuid uid
                             :behaviors []
                             :mouse-state (atom nil)
                             :move-state (atom nil)})
         zoom-slider (lookup-node doc "zoom-slider") ] ;; zoomid zoomsearchnode


     ;; ...then set the atom down here
     (reset! doc-atom doc)
     ;; This watch is what triggers a redraw whenever viewdef
     ;; changes force the slider and redraw; Attempting to redraw via
     ;; slider watch didn't work
     (add-watch viewdef-atom uid (fn [k r o n] 
                                   (.setValue zoom-slider (:zoomlevel (:zoomspecs n)))
                                   (redraw-view! doc)))

     ;; Connect zoom slider to actual zoom level
     (add-listener! zoom-slider :value (change-listener
                                        [oldval newval]
                                        (when (not= oldval newval)
                                          (zoom-to! doc newval))))
     
     (init-handlers! doc)
     doc)))

(defn doc-test []
  (let [[width height] [640 480]
        doc (doc-view ) ;;width height
        stage (jfxutils.core/stage (:doc-pane doc) [800 800])]
    doc))

(defn drag-test []
  (let [c1 (make-draggable! (jfxnew Circle 0 0 100 :fill Color/ORANGE :stroke Color/RED :stroke-width 20))
        c2 (make-draggable! (jfxnew Circle 0 0 100 :fill Color/ORANGE :stroke Color/RED :stroke-width 20))
        c3 (make-draggable! (jfxnew Circle 0 0 100 :fill Color/ORANGE :stroke Color/RED :stroke-width 20))
        bg (gradient-background :vertical Color/LIGHTBLUE Color/ALICEBLUE)
        mypane1 (make-clipped! (jfxnew Pane :children [c1], :background bg))
        mypane2 (make-clipped! (jfxnew Pane :children [c2], :background bg))
        mypane3 (make-clipped! (jfxnew Pane :children [c3], :background bg))
        paneholder (jfxnew BorderPane :center mypane3, :left mypane1, :right mypane2)]
    (stage paneholder [800 800])))

(defn editor
  ;; Start a new editor with a few doc-views
  ([] (editor nil))
  ([app]
   (let [doc1 (doc-view)
         doc2 (doc-view)
         center-dock-base (docks/base :left (docks/node (:doc-pane doc1) "doc1")
                                      :right (docks/node (:doc-pane doc2) "doc2")) 
         top-pane (border-pane
                   :center center-dock-base,
                   :top (editor-tool-bar),
                   :bottom (editor-status-bar))
         editor {:top-pane top-pane
                              :docs [doc1 doc2]
                              :behaviors []}]
     
     ;; How it works, for each doc:
     ;; 1.  Add watch to atom which calls protocol redraw
     ;; 2.  Add FX handlers to deal with mouse, keyboard input
     ;; 3.  Handler calls protocol zoom, pan, etc
     ;; 3.  Protocol fn mutates atom which triggers protocol redraw

     editor)))


(defn animate-scale [node]
  (let [xfrm (first (.getTransforms node))]
    (doseq [sc (concat (range 1 6 0.1) (range 6 1 -0.1))]
      (Thread/sleep 25)
      (doto xfrm
        (.setMxx sc)
        (.setMyy sc)))))

(defn animate-rotate [node]
  (let [xfrm (first (.getTransforms node))]
    (doseq [rot (concat (range 0 5) (range 5 0 -1))]
      (Thread/sleep 25)
      (doto xfrm
        (.append (Rotate. rot))))))

(def doc (doc-test))
(def sp (lookup-node doc "surface-pane"))
(def gc (lookup-node doc "grid-canvas"))
(def ep (lookup-node doc "entities-pane"))
(def eg (lookup-node doc "entities-group"))

(run-later (zsc eg 1 0 0))




















