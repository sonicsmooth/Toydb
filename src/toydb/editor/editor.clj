(ns toydb.editor.editor
  (:gen-class)
  (:use [jfxutils.core :exclude [-main]])
  (:require [toydb.editor
             [canvas :as canvas]
             [grid :as grid]
             [viewdef :as viewdef]]
            [toydb.bind :as bind]
            [toydb.app.menubars :as mb]
            [docks.core :as docks]
            [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as matrixop])
  (:import [javafx.scene Group Node]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button ToggleButton ToggleGroup
            Slider Label TextFormatter Separator CheckBox]
           [javafx.geometry Point2D Insets Orientation Pos]
           [javafx.scene.input MouseEvent MouseButton ScrollEvent KeyEvent]
           [javafx.event ActionEvent]
           [javafx.scene.layout BorderPane Region HBox Priority StackPane Pane
            Background BackgroundFill CornerRadii
            Border BorderStroke BorderStrokeStyle BorderWidths ]
           [javafx.scene.shape Rectangle Line Circle]
           [javafx.scene.paint Color]
           [javafx.scene.transform Affine Rotate]
           [org.controlsfx.control SegmentedButton]))


(set! *warn-on-reflection* false)
(set! *unchecked-math* false)


(def EDITOR-SETTINGS
  (atom {:background (background Color/ALICEBLUE Color/LIGHTBLUE)
         :snap true
         :grid-settings {:major-grid-display true
                         :major-line-color Color/BLACK
                         :major-line-width-px 0.15

                         :minor-grid-display true
                         :minor-line-color Color/DARKGRAY
                         :minor-line-width-px 0.5

                         :axis-display true
                         :axis-line-color Color/DARKBLUE
                         :axis-line-width-px 2

                         :major-dots-display true
                         :major-dots-color Color/RED
                         :major-dots-size-px 2

                         :minor-dots-display true
                         :minor-dots-color Color/PINK
                         :minor-dots-size-px 0.5}}))


(defn _make-idfn
  "Returns a function that appends the given id string to another
  given string"
  [id]
  (if id #(str % "-" id)
      identity))

(def make-idfn (memoize _make-idfn))

(defn capture-mouse-pos-only!
  "Simple mouse capture, used only for remembering the last position
  when the user clicks the mm/inch/whatever buttons at the top of the
  screen, so the coordinates can be updated immediately."
  [mouse-event mouse-state view]
  (let [ppos (Point2D. (.getX mouse-event) (.getY mouse-event))]
    (swap! mouse-state assoc :last-px ppos)))

(defn capture-mouse!
  ;; Used to capture movements for scrolling and dragging, etc.
  ;; Down -- remember click position
  ;; Up, DownUp, Drag -- Calculate delta position between now and last down click
  ;; Capture mouse movement in both pixel and unit space
  ([event mouse-state ^toydb.editor.viewdef.ViewDef view]
   ;; mouse-state is an atom with nil or a map
   (let [^MouseEvent event event
         [x y] [(.getX event) (.getY event)]
         state @mouse-state
         etype (.getEventType event)

         ;; Copture current
         evt-px (Point2D. x y)
         evt-u (viewdef/pixels-to-units view evt-px)

         ;; Recall previous position at last click, could be far away
         click-px (or (:click-px state) evt-px)
         click-u (or (:click-u state) evt-u)

         ;; Recall position at last move, typically close by; we want
         ;; this to be "this point" at first start
         move-px (or (:move-px state) evt-px)
         move-u (or (:move-u state) evt-u)

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
         kbs {:keys (keyword ;; generates :C__, :C_S, :_A_, etc.
                     (str (if (.isControlDown event) \C \_)
                          (if (.isAltDown event)     \A \_)
                          (if (.isShiftDown event)   \S \_)))
              :button (.getButton event)
              :buttons (keyword ;; generates :LMB, :L__, etc.
                        (str (if (.isPrimaryButtonDown   event) \L \_)
                             (if (.isMiddleButtonDown event)    \M \_)
                             (if (.isSecondaryButtonDown event) \R \_)))}]
     (swap! mouse-state merge new-poses kbs))))

;; Moving and modifying a node is none of the following: setting the
;; TranslateX/Y properties, setting the Affine transform, Relocating
;; it, or setting its LocationX/Y.  Moving and modifying is about
;; changing the points which define the geometry.  The methods to do
;; this vary amongst the various shapes.

(defprotocol ModifyProtocol
  (set-pos! [node, newloc])   ;; changes start/end/center points, etc.
  (get-pos  [node])
  (reshape! [node, notsure])) ;; changes width, height, radius, etc.

(extend-protocol ModifyProtocol
  javafx.scene.shape.Circle
  (set-pos! [node, ^Point2D newloc]
    (set-xy! node :center newloc))
  (get-pos [node]
    (get-xy node :center))

  javafx.scene.shape.Line
  (set-pos! [node, ^Point2D newloc]
    (let [oldstart (get-xy node :start)
          oldend (get-xy node :end)
          dline (.subtract oldend oldstart)
          newend (.add newloc dline)]
      (doto node
        (set-xy! :start newloc)
        (set-xy! :end newend))))
  (get-pos [ node]
    (get-xy node :start))

  javafx.scene.shape.Rectangle
  (set-pos! [node, ^Point2D newloc]
    (set-xy! node newloc))
  (get-pos [node]
    (get-xy node))

  javafx.scene.Group
  (set-pos! [node, ^Point2D newloc]
    (set-xy! node :layout newloc))
  (get-pos [node]
    (get-xy node :layout)))

(defn capture-move-state!
  "Remembers target's TranslateX/Y value."
  [move-state target]
  (swap! move-state assoc
         :target target
         :old-xy (get-pos target)))

(defn clear-move-state!
  "Clear target's Translate X/Y value."
  [move-state]
  (reset! move-state nil))


(defn get-best-target
  "Searches up the scene graph from the event's target until it
  reaches final-parent or scene.  Returns the child immediately below
  final parent which of course could just be the target itself if
  there are no parents between target and final-parent.  Nil is
  returned if no final-parent is found."
  [^MouseEvent mouse-event ^Node final-parent]
  (let [target (.getTarget mouse-event)]
    (loop [curr-node (.getTarget mouse-event)
           curr-parent (.getParent curr-node)]
      (if (nil? curr-parent)
        nil ;; no final-parent found
        (if (= curr-parent final-parent)
          curr-node ;; found!
          (recur curr-parent (.getParent curr-parent))))))) ;; keep going


(declare update-coordinates!)
(declare get-print-scale-and-label)

;; in these fns, swap! the view data from a given doc. The watch will respond and update the drawing
(defprotocol DocumentProtocol
  "Functions for Document defrecord"
  (lookup-node [this id])                             ;; Returns whatever node is called for
  (reset-view! [this])                                ;; mutates view, triggers watch
  (redraw-view! [this])                               ;; called from watch
  (transscale-entities! [this])                       ;; called from redraw-view!
  (view-metric! [this] [this metric-choice])          ;; mutates view, triggers watch
  (view-inches! [this])                               ;; mutates view, triggers watch
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
    (jfxutils.core/lookup (:doc-pane doc) ((make-idfn (:uuid doc)) id)))

  (reset-view! [doc]
    (let [^Canvas canvas (lookup-node doc "grid-canvas")
          [width height] [(.getWidth canvas) (.getHeight canvas)]]
      (swap! (:viewdef doc) viewdef/reset-viewdef width height)))

  (pan-to! [doc point2d]
    (swap! (:viewdef doc) viewdef/pan-to point2d))

  (pan-by! [doc dpoint2d]
    (swap! (:viewdef doc) viewdef/pan-by dpoint2d))

  (resize! [doc oldsize newsize]
    (swap! (:viewdef doc) viewdef/resize oldsize newsize @(:mouse-state doc)))

  (zoom-to! [doc zoom-level]
    (swap! (:viewdef doc) viewdef/zoom-to zoom-level)) ;; Set zoom level with respect to origin

  (zoom-to! [doc zoom-level pt]
    (swap! (:viewdef doc) viewdef/zoom-to zoom-level pt)) ;; Set zoom level centered on position pt

  (zoom-by! [doc dzoom-level]
    (swap! (:viewdef doc) viewdef/zoom-by dzoom-level)) ;; Zoom by a certain amount centered on origin

  (zoom-by! [doc dzoom-level pt]
    (swap! (:viewdef doc) viewdef/zoom-by dzoom-level pt)) ;; Zoom by a certain amount centered on point

  (redraw-view! [doc]
    (when-let [canvas (lookup-node doc "grid-canvas")]
      (toydb.editor.grid/draw-grid! canvas @(:viewdef doc) (:grid-settings @EDITOR-SETTINGS)))
    (transscale-entities! doc))

  (view-metric! [doc]
    (swap! (:viewdef doc) viewdef/view-metric))

  (view-metric! [doc metric-choice]
    (condp = metric-choice
      :metric (view-metric! doc)
      :inches (view-inches! doc)))
  
  (view-inches! [doc]
    (swap! (:viewdef doc) viewdef/view-inches))
  
  (transscale-entities! [doc]
    ;; scales and translates entities-group for zoom and pan
    (let [viewdef @(:viewdef doc)
          view-xfrm (:transform viewdef)
          ^Node entities-group (lookup-node doc "entities-group")
          ^Affine eg-xfrm (first (.getTransforms entities-group))]
      ;; Is there some way to get the matrix and send it to .setWhatever directly?
      (.setMxx eg-xfrm (matrix/mget view-xfrm 0 0))
      (.setMyy eg-xfrm (matrix/mget view-xfrm 1 1))
      (.setTx eg-xfrm (matrix/mget view-xfrm 0 2))
      (.setTy eg-xfrm (matrix/mget view-xfrm 1 2))))
  
  (init-handlers! [doc]
    ;; Add mouse events to deal with pan, zoom, drag
    (let [^Node surface-pane (lookup-node doc "surface-pane" )
          ^Node entities-pane (lookup-node doc "entities-pane")
          ^Node entities-group (lookup-node doc "entities-group")
          mouse-state (:mouse-state doc)
          move-state (:move-state doc)
          snapfn #(viewdef/units-to-snapped-units @(:viewdef doc) %)

          ;; keyboard state is probably not necessary since modifier
          ;; keys are available with every mouse event.
          kbd-state (atom {}) ;; Should move this to doc like mouse-state?
          capture-kbd! (fn [^KeyEvent event]
                         ;; Capture state of modifier keys
                         (swap! kbd-state assoc
                                :ctrl (.isControlDown event)
                                :shift (.isShiftDown event)
                                :alt (.isAltDown event)))
          idfn (make-idfn (:uuid doc))
          lookup-node (memoize lookup-node)
          

          ;; For display of coordinates in status bar
          move-handler (event-handler [mouse-event]
                                      (capture-mouse-pos-only! mouse-event mouse-state @(:viewdef doc))
                                      (update-coordinates! doc @mouse-state))

          click-down-handler (event-handler [mouse-event]
                                            (capture-mouse! mouse-event mouse-state @(:viewdef doc))
                                            (.requestFocus surface-pane)
                                            (if-let [target (get-best-target mouse-event entities-group)]
                                              (when (= (:buttons @mouse-state) :L__)
                                                (capture-move-state! move-state target))
                                              (clear-move-state! move-state)))
          click-up-handler (event-handler [mouse-event] 
                                          (capture-mouse! mouse-event mouse-state @(:viewdef doc))
                                          (.requestFocus surface-pane)
                                          ;; Select when distance from last click is zero, and LMB was cause
                                          (when (and (= (:click-dpx @mouse-state) Point2D/ZERO)
                                                     (= (:button @mouse-state) MouseButton/PRIMARY))))

          drag-handler (event-handler [mouse-event]
                                      (.handle move-handler mouse-event) ;; update status bar text
                                      (capture-mouse! mouse-event  mouse-state @(:viewdef doc))
                                      (let [ms @mouse-state
                                            mv @move-state]
                                        (condp = (:buttons ms)
                                          ;; Item moves: add movement-since-click to old position, then snap
                                          :L__ (when mv
                                                 (let [^Node target (:target mv)
                                                       ^Point2D tgtxy (:old-xy mv)]
                                                   (if (:snap @EDITOR-SETTINGS)
                                                     (set-pos! target (snapfn (.add tgtxy (:click-du ms))))
                                                     (set-pos! target (.add tgtxy (:click-du ms))))))

                                          ;; Pan: just tell the doc to move the required pixels
                                          :__R (pan-by! doc (:move-dpx ms) )

                                          ;; Zoom: just tell the doc to zoom-on-point the required amount 
                                          :L_R (let [move-dpxy (double (.getY (:move-dpx ms)))
                                                     ^Point2D click-px (:click-px ms)]
                                                 (zoom-by! doc (- move-dpxy) click-px))

                                          ;; Otherise, do nothing
                                          (println "Drag condition not handled"))))
          
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
          reset-btn-handler (event-handler [action-event] (reset-view! doc))]
      
      (add-event-filter! surface-pane MouseEvent/MOUSE_MOVED move-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_PRESSED click-down-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_RELEASED click-up-handler)
      (add-event-filter! surface-pane MouseEvent/MOUSE_DRAGGED drag-handler)
      (add-event-filter! surface-pane ScrollEvent/SCROLL scroll-handler)
      (set-on-event-handler! surface-pane :key-pressed keyboard-handler)
      (set-on-event-handler! surface-pane :key-released keyboard-handler)
      (set-on-event-handler! (lookup-node doc "reset-button") :action reset-btn-handler)
      (add-listener! surface-pane :focused focus-listener))))

(defn- get-print-scale-and-label [doc]
  (let [view @(:viewdef doc)
        metric? (:metric-or-inches view)
        ps ((:print-scales view) metric?)]
    (condp = metric?
      :metric (let [ms (:metric-selection view)]
                [(ps ms) (name ms)])
      :inches (let [is (:inches-selection view)]
                [(ps is) (name is)]))))

(defn- update-coordinates!
  "Use current state to update coordinates at bottom of screen"
  [doc mouse-state]
  (let [view @(:viewdef doc)
        ppos (or (:last-px mouse-state) (:origin view) )
        [unit-scale unit-label] (get-print-scale-and-label doc)
        upos (viewdef/pixels-to-units view ppos)
        snupos (if (:snap @EDITOR-SETTINGS)
                 (viewdef/pixels-to-snapped-units view ppos)
                 upos)
        snscupos (.multiply snupos unit-scale)
        ^Label upos-label (lookup-node doc "unit-pos-label")]
    ;; Apparently .setText calls canvas resize somehow
    (.setText upos-label (format "ux:%-5.5g %s, uy:%-5.5g %s"
                                 (.getX snscupos) unit-label
                                 (.getY snscupos) unit-label))))


(defn force-down!
  "Sets the onAction handler on each button so it can't be released
  with a mouse click.  buttons is seq of Buttons.  Returns buttons."
  [buttons]
  (let [handler (event-handler [evt] ;; if unpressed, set back to pressed, and eat event
                               (when (not (.. evt getSource isSelected))
                                 (set-prop-val! (.. evt getSource) :selected true)
                                 (.consume evt)))]
    (doseq [button buttons]
      (.addEventFilter button ActionEvent/ANY handler)))
  buttons)


(defn doc-tool-bar
  "Creates document tool bar, where each button has id as suffix"
  ([] (doc-tool-bar nil))
  ([^String uid]
   (let [idfn (make-idfn uid)
         ;; not using toggle-buttons function because of idfn
         metric-button (jfxnew ToggleButton :id (idfn "metric-button" ) :text "metric" :selected true)
         inches-button (jfxnew ToggleButton :id (idfn "inches-button") :text "inches")
         snap-checkbox (jfxnew CheckBox "Snap to Grid" :id (idfn "snap-checkbox") :selected true)
         metric-or-inches-buttons (force-down! [metric-button inches-button])
         metric-scale-buttons (force-down! [(jfxnew ToggleButton :id (idfn "um-button") :text "um" :selected false)
                                            (jfxnew ToggleButton :id (idfn "mm-button") :text "mm" :selected true)
                                            (jfxnew ToggleButton :id (idfn "cm-button") :text "cm" :selected false)])
         inch-scale-buttons (force-down! [(jfxnew ToggleButton :id (idfn "inch-button") :text "inch" :selected true)
                                          (jfxnew ToggleButton :id (idfn "mil-button")  :text "mil" :selected false)])
         segmented-metric-or-inches (jfxnew SegmentedButton :buttons metric-or-inches-buttons)
         segmented-metric-scale-chooser (jfxnew SegmentedButton :buttons metric-scale-buttons)
         segmented-inch-scale-chooser (jfxnew SegmentedButton :buttons inch-scale-buttons)
         spring (Region.)
         tb (jfxnew HBox
                    :children [segmented-metric-or-inches
                               (jfxnew Separator :orientation Orientation/VERTICAL :pref-width 40)
                               (jfxnew StackPane :children [segmented-metric-scale-chooser
                                                            segmented-inch-scale-chooser])
                               (jfxnew Separator :orientation Orientation/VERTICAL :pref-width 40)
                               snap-checkbox
                               spring
                               (jfxnew Button :id (idfn "reset-button") :text "reset zoom/pan")]
                    :alignment Pos/CENTER)]

     (HBox/setHgrow spring Priority/ALWAYS)
     ;; Hide or show the scale chooser group for metric and inches
     (.bind (get-property segmented-metric-scale-chooser :visible)
            (get-property metric-button :selected))
     (.bind (get-property segmented-inch-scale-chooser :visible)
            (get-property inches-button :selected) )
     tb)))

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
         ;;mppl (jfxnew Label "doc mouse label" :id (idfn "pixel-pos-label"))
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
                                                         (.setValue zoom-slider1 (Math/round (double newval)))))

     ;; Unidirectional binding between slider position and text is set in fancy bind/bind
     (mb/status-bar [#_mppl mupl spring zoom-label-txt zoom-slider1 zoom-value-txt]))))

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
  []   
  (let [uid (uuid)
        idfn (make-idfn uid)
        viewdef-atom (atom (viewdef/viewdef))
        zoomlimits (-> @viewdef-atom :zoomspecs :zoomlimits)
        ;; The doc contains the canvas, but the canvas resize
        ;; callback refers to the doc, so we have a circular
        ;; reference so make a quick atom...
        doc-atom (atom nil)
        ^Canvas grid-canvas (canvas/resizable-canvas (fn [canvas, [oldw oldh] [neww newh]]
                                                       (when (or (not= oldw neww) (not= oldh newh))
                                                         (resize! @doc-atom [oldw oldh] [neww newh]))))
        twocircles [(jfxnew Circle 0.5 0 0.25 :stroke Color/GRAY :stroke-width 0.025 :fill Color/SILVER)
                    (jfxnew Circle 1.0 0 0.15 :stroke Color/GRAY :stroke-width 0.025 :fill Color/SILVER)
                    (jfxnew Line 0 -0.1 0 0.1 :stroke Color/BLACK :stroke-width 0.015)
                    (jfxnew Line -0.1 0 0.1 0 :stroke Color/BLACK :stroke-width 0.015)]
        little-group (jfxnew Group
                             :id (idfn "little-group")
                             :children twocircles)
        shapes [(jfxnew Line -1 -1 1 1 :stroke Color/GREEN :stroke-width 0.05)
                (jfxnew Line -1 1 1 -1 :stroke Color/BLUE :stroke-width 0.05)
                (jfxnew Circle -1 -1 0.1 :stroke Color/GREEN :stroke-width 0.05)
                (jfxnew Circle -1 1 0.1 :stroke Color/BLUE :stroke-width 0.05)
                (jfxnew Circle 0 0 0.1 :stroke Color/CADETBLUE :stroke-width 0.05 :fill Color/BISQUE)
                (jfxnew Rectangle 0 0 10000 10000 :stroke Color/AQUAMARINE :stroke-width 1000 :fill nil)
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
                                            :background (:background @EDITOR-SETTINGS)))

        doc (map->Doc-View {:viewdef viewdef-atom
                            :doc-pane (border-pane ;; holds main grid and decorations
                                       :center surface-pane
                                       :top (doc-tool-bar uid),
                                       :bottom (doc-status-bar uid :zoomlimits zoomlimits))
                            :uuid uid
                            :behaviors []
                            :mouse-state (atom nil)
                            :move-state (atom nil)})
        ^Slider zoom-slider (lookup-node doc "zoom-slider")
        ^Label zoom-label (lookup-node doc "zoom-value")
        ^ToggleButton metric-button (lookup-node doc "metric-button")
        ^ToggleButton inches-button (lookup-node doc "inches-button")
        ^ToggleButton um-button (lookup-node doc "um-button")
        ^ToggleButton mm-button (lookup-node doc "mm-button")
        ^ToggleButton cm-button (lookup-node doc "cm-button")
        ^ToggleButton inch-button (lookup-node doc "inch-button")
        ^ToggleButton mil-button (lookup-node doc "mil-button")
        ^CheckBox snap-checkbox (lookup-node doc "snap-checkbox")]



    ;; Name the canvas so we can get to it later
    ;; Probably we should change the canvas construtor to accept property arguments
    (.setId grid-canvas (idfn "grid-canvas"))
    
    ;; ...then set the atom down here
    (reset! doc-atom doc)

    ;; This watch triggers a redraw whenever viewdef changes.
    (add-watch viewdef-atom :main-redraw (fn [k r o n]
                                           (redraw-view! doc)))

    ;; This watch triggers a redraw when one of the settings changes, primarily grid
    (add-watch EDITOR-SETTINGS :settings-redraw (fn [k r o n]
                                                  (redraw-view! doc)))


    ;; Use fancy double-binding to tie internal zoom level with slider value and text.
    (bind/bind! :init 0
                :var viewdef-atom
                :var-fn #(zoom-to! doc %) ;; delegate swap! to this fn instead
                :keyvec [:zoomspecs :zoomlevel]
                :targets {zoom-slider {:property :value}
                          zoom-label {:property :text,
                                      :terminal true,
                                      :var-to-prop-fn str }})

    ;; Use fancy double-binding to tie internal metric-scale
    ;; (true/false) with buttons Call view-metric! protocol fn, which
    ;; in turn calls viewdef/view-metric, because other stuff in view
    ;; besides :metric-or-inches needs to change.
    (bind/bind! :init :metric
                :var viewdef-atom
                :var-fn #(view-metric! doc %)
                :keyvec [:metric-or-inches]
                :targets {metric-button {:property :selected
                                         :var-to-prop-fn viewdef/metric?
                                         :prop-to-var-fn {true :metric false :inches}}
                          inches-button {:property :selected
                                         :var-to-prop-fn viewdef/inches?
                                         :prop-to-var-fn {false :metric true :inches}}})

    ;; Use fancy double binding to tie print scales (um/mm/cm, etc)
    ;; with buttons. No need to call protocol fn or viewdef fn because
    ;; only metric-selection or inches-selection changes, however
    ;; prop-to-var-fn needs to be written such that the var either
    ;; definitely changes when the button is pressed, or stays the
    ;; same when the button is released.
    (letfn [(metric-propfn [selector]
              (fn [propval]
                (if propval
                  selector
                  (:metric-selection @viewdef-atom))))]
      
      (bind/bind! :init :cm
                  :var viewdef-atom
                  :keyvec [:metric-selection]
                  :targets {um-button {:property :selected
                                       :var-to-prop-fn viewdef/um?
                                       :prop-to-var-fn (metric-propfn :um)}
                            mm-button {:property :selected
                                       :var-to-prop-fn viewdef/mm?
                                       :prop-to-var-fn (metric-propfn :mm)}
                            cm-button {:property :selected
                                       :var-to-prop-fn viewdef/cm?
                                       :prop-to-var-fn (metric-propfn :cm)}})) 
    
    ;; These fns update the coordinates when the buttons are pressed
    ;; Do this here instead of in the var-to-prop-fn and
    ;; prop-to-var-fn
    (add-watch viewdef-atom (rand-int 10)
               (fn [key ref old new]
                 (when (or (not= (:metric-or-inches new)
                                 (:metric-or-inches old))
                           (not= (:metric-selection new)
                                 (:metric-selection old)))
                   (update-coordinates! doc @(:mouse-state doc)))))

    
    ;; Use fancy double binding to tie internal snap setting to checkbox
    (bind/bind! :init true
                :var EDITOR-SETTINGS
                :keyvec [:snap]
                :property :selected
                :targets [snap-checkbox])

    ;; Use fancy double binding to tie background to editor settings
    (bind/bind! :init (:background @EDITOR-SETTINGS)
                :var EDITOR-SETTINGS
                :terminal true
                :keyvec [:background]
                :property :background
                :targets [surface-pane])

    (init-handlers! doc)
    doc))

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


(defn animate-scale [^Node node]
  (let [^Affine xfrm (first (.getTransforms node))]
    (doseq [sc (concat (range 1 6 0.1) (range 6 1 -0.1))]
      (Thread/sleep 25)
      (doto xfrm
        (.setMxx sc)
        (.setMyy sc)))))

(defn animate-rotate [^Node node]
  (let [^Affine xfrm (first (.getTransforms node))]
    (doseq [rot (concat (range 0 5) (range 5 0 -1))]
      (Thread/sleep 25)
      (.append xfrm (Rotate. rot)))))


(defn zsc [^Node node sc x y]
  ;; Changes the built-in scale[XY] and translate[XY] properties, not
  ;; the affine transform.
  (set-xy! node :translate (Point2D. x y))
  (set-scale! node sc))

(defn go []
  (def docx (doc-test))
  (def sp (lookup-node docx "surface-pane"))
  (def gc (lookup-node docx "grid-canvas"))
  (def ep (lookup-node docx "entities-pane"))
  (def eg (lookup-node docx "entities-group"))
  (def lg (lookup-node docx "little-group"))
  (run-later (zsc eg 1 0 0))
  )

(defn main [& args]
  (set-exit false)
  (go))

(defn -main [& args]
  (jfxutils.core/app-init)
  (go))























