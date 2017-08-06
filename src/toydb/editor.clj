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
  (:require toydb.viewdef )
  (:import [javafx.scene Group Scene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button Slider Label]
           [javafx.geometry Point2D Insets]
           [javafx.scene.input MouseEvent MouseButton ScrollEvent KeyEvent]
           [javafx.scene.layout BorderPane Region HBox VBox Priority StackPane
            Background BackgroundFill CornerRadii ]
           [javafx.scene.paint Color]
           [javafx.scene.text Font]
           [javafx.stage Stage]
           [javafx.beans.binding Bindings]))

(set! *warn-on-reflection* false)
(set! *unchecked-math* false)


(defprotocol CaptureProtocol
  (capture-mouse!
    [event mouse-state]
    [event mouse-state last-click-pos]))


(extend-protocol CaptureProtocol
  javafx.scene.input.MouseEvent
  (capture-mouse!
    ;; Used to capture movements for scrolling and dragging, etc.
    ;; Passes last-click-pos essentially as dummy variable so it
    ;; doesn't change
    ([event mouse-state]
     (capture-mouse! event mouse-state (:last-click-pos @mouse-state (Point2D/ZERO))))

    ;; Used to capture the mouse click
    ([event mouse-state ^Point2D click-pos]
     (let [oldpos (:pos @mouse-state (Point2D/ZERO)) ;; in case mouse-state is nil
           pos (Point2D. (.getX event) (.getY event))]
       (swap! mouse-state assoc
              :pos pos
              :dpos (.subtract pos oldpos)
              :last-click-pos click-pos
              :primary (.isPrimaryButtonDown event)
              :secondary (.isSecondaryButtonDown event)
              :middle (.isMiddleButtonDown event)
              :ctrl (.isControlDown event)
              :alt (.isAltDown event)
              :shift (.isShiftDown event))))))

(defprotocol EditorProtocol
  "Function for Editor defrecord"
  (show [this]))

;; in these fns, swap! the view data from a given doc. The watch will respond and update the drawing
(defprotocol DocumentProtocol
  "Functions for Document defrecord"
  (get-canvas [this]) ;; Returns resizable canvas, which is the .getCenter of the :doc-pane
  (reset-view! [this])  ;; mutates view, triggers watch
  (redraw-view! [this]) ;; called from watch
  (pan-to! [this point2d])  ;; mutates view, triggers watch
  (pan-by! [this dpoint2d]) ;; mutates view, triggers watch
  (zoom-to! [this zoom-level] [this zoom-level pt]) ;; mutates view, triggers watch
  (zoom-by! [this dzoom-level] [this dzoom-level pt]) ;; mutates view, triggers watch
  (resize! [this [oldw oldh] [neww newh]]) ;; resizes window
  (init-handlers! [this])) 


(defrecord Editor [top-pane, docs, behaviors] 
  EditorProtocol
  (show [this] (stage (:top-pane this))))

(defrecord Doc-View [view-data ;; zoom, pan, grid spec
                     doc-pane  ;; holds everything, including toolbar
                     uuid      ;; unique identifier for this instance
                     behaviors
                     mouse-state] ;; for toolbars or whatever
  DocumentProtocol
  ;; Anything with swap! leads to redraw-view! via the watch
  (get-canvas [doc] (.getCenter (:doc-pane doc)))
  (reset-view! [doc] (let [can (get-canvas doc)
                           width (.getWidth can)
                           height (.getHeight can)
                           view-atom (:view-data doc)
                           view @view-atom]
                       (swap! view-atom toydb.viewdef/reset-viewdef width height)))
  (pan-to! [doc point2d] (swap! (:view-data doc) toydb.viewdef/pan-to point2d))
  (pan-by! [doc dpoint2d] (swap! (:view-data doc) toydb.viewdef/pan-by dpoint2d))
  (resize! [doc oldsize newsize] (swap! (:view-data doc) toydb.viewdef/resize oldsize newsize @(:mouse-state doc)))
  (zoom-to! [doc zoom-level] (swap! (:view-data doc) toydb.viewdef/zoom-to zoom-level)) ;; Set zoom level with respect to origin
  (zoom-to! [doc zoom-level pt] (swap! (:view-data doc) toydb.viewdef/zoom-to zoom-level pt)) ;; Set zoom level centered on position pt
  (zoom-by! [doc dzoom-level] (swap! (:view-data doc) toydb.viewdef/zoom-by dzoom-level)) ;; Zoom by a certain amount centered on origin
  (zoom-by! [doc dzoom-level pt] (swap! (:view-data doc) toydb.viewdef/zoom-by dzoom-level pt)) ;; Zoom by a certain amount centered on point
  (redraw-view! [doc] (toydb.grid/draw-grid! (get-canvas doc) @(:view-data doc)))  
  
  (init-handlers! [doc]
    ;; Add mouse events to deal with pan, zoom, drag
    (let [^javafx.scene.canvas.Canvas canvas (get-canvas doc)
          ^javafx.scene.Node pane (:doc-pane doc)
          mouse-state (:mouse-state doc)

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
          lookup-node (memoize (fn [idtxt node] ;; always looks down starting at node
                                 (lookup (long-id idtxt) node)))

          move-handler (event-handler [mouse-event]
                                      (let [^MouseEvent event mouse-event
                                            ppos (Point2D. (.getX event) (.getY event))
                                            upos (toydb.viewdef/pixels-to-units @(:view-data doc) ppos)
                                            ppos-label (lookup-node "pixel-pos-label" pane)
                                            upos-label (lookup-node "unit-pos-label" pane)]
                                        ;; Apparently .setText calls canvas resize somehow
                                        (.setText ppos-label (format "px:% 5.0f, py:% 5.0f" (.getX ppos) (.getY ppos)))
                                        (.setText upos-label (format "ux:% 5.3f, uy:% 5.3f" (.getX upos) (.getY upos)))))

          click-handler (event-handler [mouse-event] ;; We only need this to capture the initial state of the mouse
                                       ;; We look at mouse event (down+up = 2) * (2^2=4 mouse states) * (2^3=8 key states)
                                       ;; = 2*4*8 = 64 combinations of this event.  How to encode nicely?
                                       (let [^MouseEvent event mouse-event]
                                         (capture-mouse! event mouse-state (Point2D. (.getX event) (.getY event)))
                                         (.requestFocus pane)
                                         #_(let [ms @mouse-state
                                                 button (.getButton event)]
                                             #_(cond (:primary ms) (println "primary down")
                                                     (= button MouseButton/PRIMARY) (println "primary up, pick or select item"))
                                             #_(cond(:secondary ms) (println "secondary down")
                                                    (= button MouseButton/SECONDARY) (println "secondary up"))
                                             #_(cond (:middle ms) (println "middle down")
                                                     (= button MouseButton/MIDDLE) (println "middle up"))
                                             #_(cond (:ctrl ms) (println "ctrl at click"))
                                             #_(cond (:alt ms) (println "alt at click"))
                                             #_(cond (:shift ms) (println "shift at click")))))

          drag-handler (event-handler [mouse-event]
                                      (let [event ^MouseEvent mouse-event]
                                        (capture-mouse! event mouse-state))
                                      (let [ms @mouse-state]
                                        (cond ;;(and (:primary ms) (not (:secondary ms)))
                                          ;;(println "move picked item or selected items")

                                          (and (not (:primary @mouse-state)) (:secondary ms))
                                          (pan-by! doc (:dpos @mouse-state))

                                          (and (:primary @mouse-state) (:secondary ms))
                                          (let [^Point2D dpos (:dpos @mouse-state)
                                                amt (- (.getY dpos)) 
                                                lcp (:last-click-pos @mouse-state) ]
                                            (zoom-by! doc amt lcp))
                                              
                                          ;;:else (println "not supported")
                                          )))
          
          scroll-handler (event-handler [scroll-event]
                                        (let [^ScrollEvent event scroll-event
                                              x (.getX event) ;; mouse position
                                              y (.getY event)
                                              dx (.getDeltaX event) ;; scroll amounts
                                              dy (.getDeltaY event)]
                                          (if (.isControlDown event)
                                            (zoom-by! doc (/ dy 10.0) (Point2D. x y))
                                            (pan-by! doc (Point2D. (/ dx 5.0) (/ dy 5.0))))))

          keyboard-handler (event-handler [key-event]  (capture-kbd! key-event))
          focus-listener (change-listener [oldval newval] nil)
          reset-btn-handler (event-handler [action-event] (reset-view! doc))
          unit-btn-handler (event-handler [action-event]
                                          (let [vd (:view-data doc)
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
      
      (set-event-handler! canvas :mouse-moved move-handler)
      (set-event-handler! canvas :mouse-pressed click-handler)
      (set-event-handler! canvas :mouse-released click-handler)
      (set-event-handler! canvas :mouse-dragged drag-handler)
      (set-event-handler! canvas :scroll scroll-handler)
      (set-event-handler! canvas :key-pressed keyboard-handler)
      (set-event-handler! canvas :key-released keyboard-handler)
      (set-event-handler! (lookup-node "reset-button" pane) :action reset-btn-handler)
      (set-event-handler! (lookup-node "cm-button" pane) :action unit-btn-handler)
      (set-event-handler! (lookup-node "inch-button" pane) :action unit-btn-handler)
      (add-listener! canvas :focused focus-listener))))

(defn make-idfn
  "Returns a function that appends the given id string to another
  given string"
  [id]
  (if id #(str % "-" id)
      identity))

(defn doc-tool-bar
  "Creates document tool bar, where each button has id as suffix"
  ([] (doc-tool-bar nil))
  ([^String id]
   (let [idfn (make-idfn id)]
     (mb/tool-bar [(jfxnew Button :id (idfn "cm-button" ) :text "cm")
                   (jfxnew Button :id (idfn "inch-button") :text "inch")
                   (jfxnew Button :id (idfn "reset-button") :text "reset")]))))

(defn editor-tool-bar
  "Creates editor tool bar, where each button has id as suffix."
  ([] (editor-tool-bar nil))
  ([^String id]
   (let [idfn (make-idfn id)]
     (mb/tool-bar [(jfxnew Button :id (idfn "edit-1") :text "edit-1")
                   (jfxnew Button :id (idfn "edit-2") :text "edit-2")
                   (jfxnew Button :id (idfn "edit-3") :text "edit-3")]))))

(defn doc-status-bar
  ([] (doc-status-bar nil))
  ([^String id & {:keys [zoomlimits]}]
   (let [idfn (make-idfn id)
         mppl (jfxnew Label "doc mouse label" :id (idfn "pixel-pos-label"))
         mupl (jfxnew Label "doc mouse label" :id (idfn "unit-pos-label"))
         spring (Region.)
         zoom-min (or (first zoomlimits) -10)
         zoom-max (or (second zoomlimits) 10)
         zoom-slider1 (jfxnew Slider zoom-min zoom-max 0.0 :block-increment 10
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
  ([^String id]
   (let [idfn (make-idfn id)
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
         view-data-atom (atom (toydb.viewdef/viewdef))
         zoomlimits (:zoomlimits @view-data-atom)
         ;; The doc contains the canvas, but the canvas resize
         ;; callback refers to the doc, so we have a circular
         ;; reference so make a quick atom...
         doc-atom (atom nil)
         grid-canvas (canvas/resizable-canvas (fn [canvas, [oldw oldh] [neww newh]]
                                                (when (or (not= oldw neww) (not= oldh newh))
                                                  (resize! @doc-atom [oldw oldh] [neww newh]))))
         doc (map->Doc-View {:view-data view-data-atom
                             :doc-pane (border-pane ;; holds main grid and decorations
                                        :center grid-canvas ;;surface-pane,
                                        :top (doc-tool-bar uid),
                                        :bottom (doc-status-bar uid :zoomlimits zoomlimits))
                             :uuid uid
                             :behaviors []
                             :mouse-state (atom nil)})
         zoomid (str "zoom-slider-" uid)
         zoom-slider (lookup zoomid (:doc-pane doc)) ]
     ;; ...then set the atom down here
     (reset! doc-atom doc)
     ;; This watch is what triggers a redraw whenever view-data changes
     (add-watch view-data-atom uid (fn [k r o n]
                                     ;; Redraw either via forcing the
                                     ;; zoom slider, or by just
                                     ;; calling redraw
                                     (if (not= (:zoomlevel n) (:zoomlevel o))
                                       (.setValue zoom-slider (:zoomlevel n))
                                       (redraw-view! doc))))

     ;; Connect zoom slider to actual zoom level
     (add-listener! zoom-slider :value
                    (change-listener [oldval newval]
                                     (when (not= oldval newval)
                                       (zoom-to! doc newval))))
     
     (init-handlers! doc)
     doc)))

(defn doc-test []
  (let [[width height] [640 480]
        doc (doc-view ) ;;width height
        stage (jfxutils.core/stage (:doc-pane doc) [800 800])]
    doc))

(defn editor
  ;; Start a new editor with a few doc-views
  ([app] (editor app [nil nil]))
  ([app [width height]]
   (let [doc1 (doc-view) ;;width height
         doc2 (doc-view);; width height
         center-dock-base (docks/base :left (docks/node (:doc-pane doc1) "doc1")
                                      :right (docks/node (:doc-pane doc2) "doc2")) 
         top-pane (border-pane
                   :center center-dock-base,
                   :top (editor-tool-bar),
                   :bottom (editor-status-bar)
                   :size [width height])
         editor (map->Editor {:top-pane top-pane
                              :docs [doc1 doc2]
                              :behaviors []})]
     
     ;; How it works, for each doc:
     ;; 1.  Add watch to atom which calls protocol redraw
     ;; 2.  Add FX handlers to deal with mouse, keyboard input
     ;; 3.  Handler calls protocol zoom, pan, etc
     ;; 3.  Protocol fn mutates atom which triggers protocol redraw
     
     (when width (.setPrefWidth top-pane width))
     (when height (.setPrefHeight top-pane height))

     editor)))



