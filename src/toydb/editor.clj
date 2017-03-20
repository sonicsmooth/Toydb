(ns toydb.editor
  (:use [jfxutils.core :exclude [-main]]
        [clojure.pprint])
  (:require [toydb
             [canvas :as canvas]
             [grid :as grid]
             [menubars :as mb]]
            [docks.core :as docks])
  (:import [javafx.scene Group Scene]
           [javafx.scene.canvas Canvas]
           [javafx.scene.control Button Slider Label]
           [javafx.geometry Point2D Insets]
           [javafx.scene.input MouseEvent MouseButton ScrollEvent KeyEvent]
           [javafx.scene.layout BorderPane Region HBox VBox Priority StackPane
            Background BackgroundFill CornerRadii ]
           [javafx.scene.paint Color]
           [javafx.scene.text Font]
           [javafx.stage Stage]))

(set! *warn-on-reflection* false)
(set! *unchecked-math* false)

(def DEFAULT-GRID-COLORS
  {:background (simple-vert-gradient Color/SLATEGRAY Color/SILVER)
   :major-line-color Color/BLACK
   :minor-line-color Color/LIGHTGRAY
   :axis-line-color Color/DARKBLUE})

(def DEFAULT-VIEW-SPECS
  {:unit :cm      ;; so we know how to convert
   :origin (Point2D. 10 -10) ;; in canvas space pixels, relative to bottom left corner, so negative y means up from bottom
   :zoom {:pixels-per-grid    [100 100] ;; Major step size [x,y] in pixels per grid unit at zoom level 0
          :units-per-grid     [ 1    1] ;; Reality units per major grid unit.
          :minors-per-major   [ 5    5] ;; Minor steps per major step
          :ratio  10   ;; how many zoom clicks per major-div replacement.  Positive integer.
          :level  0.0  ;; Basically the log of the zoom, an integer when using scroll wheel
          :minmax [-10 30]}
   ;;:canvas-border    [10 10 ] ;;[ 1.2 1.2] ;; upper left corner in canvas space
   :colors DEFAULT-GRID-COLORS})

(defprotocol EditorProtocol
  "Function for Editor defrecord"
  (show [this]))

;; in these fns, swap! the view data from a given doc. The watch will respond and update the drawing
(defprotocol DocumentProtocol
  "Functions for Document defrecord"
  (reset-view! [this])  ;; mutates view, triggers watch
  (redraw-view! [this]) ;; called from watch
  (pan-to! [this point2d])  ;; mutates view, triggers watch
  (pan-by! [this dpoint2d]) ;; mutates view, triggers watch
  (zoom-to! [this zoom-level] [this zoom-level pt]) ;; mutates view, triggers watch
  (zoom-by! [this dzoom-level] [this dzoom-level pt]) ;; mutates view, triggers watch
  (init-handlers! [this])) 


(defrecord Editor [top-pane
                   docs
                   behaviors]
  EditorProtocol
  (show [this] (stage (:top-pane this))))

(defrecord Document [view-data   ;; zoom, pan, grid spec
                     grid-canvas ;; the background canvas
                     surface-pane ;; the pane which receives mouse input; holds grid-canvas and components
                     doc-pane   ;; holds everything, including toolbar
                     uuid       ;; unique identifier for this instance
                     behaviors] ;; for toolbars or whatever
  DocumentProtocol
  (reset-view! [doc] (do ;; sqeeze to nothing so resizing back up again works
                       (.resize (:grid-canvas doc) 0.0 0.0)
                       (swap! (:view-data doc) map-replace DEFAULT-VIEW-SPECS)))
  (redraw-view! [doc] (toydb.grid/draw-grid! (:grid-canvas doc) @(:view-data doc)))
  (pan-to! [doc point2d] (swap! (:view-data doc) assoc-in [:origin] point2d))
  (pan-by! [doc dpoint2d] (swap! (:view-data doc) update-in [:origin] #(let [^Point2D x %] (.add x dpoint2d))))
  (zoom-to! [doc zoom-level] ;; Set zoom level with respect to origin
    (zoom-to! doc zoom-level Point2D/ZERO))
  (zoom-to! [doc zoom-level pt] ;; Set zoom level centered on position pt
    )
  (zoom-by! [doc dzoom-level] ;; Zoom by a certain amount centered on origin
    (zoom-by! doc dzoom-level Point2D/ZERO))
  (zoom-by! [doc dzoom-level pt]
    ;; For zoom-on-point, we take the difference in grid coordinates between the pre-zoom
    ;; and post-zoom state, then calculate the equivalent pixel coordinates, and move
    ;; the origin by that amount.
    (let [^double amt dzoom-level
          specs @(:view-data doc)
          zoomspecs (:zoom specs)
          ^double level (:level zoomspecs) ;; previous zoom level
          [^double minzoom ^double maxzoom] (:minmax zoomspecs)
          limit (fn [^double x [^double mn ^double mx]] (cond (< x mn) (double mn)
                                                              (> x mx) (double mx)
                                                              :else x))]
      (when (or (and (< amt 0.0) (> level minzoom))  ;; zoom out
                (and (> amt 0.0) (< level maxzoom))) ;; zoom in
        (let [^Point2D pos-units (grid/pixels-to-units pt specs :translate)
              newspecs (update-in specs [:zoom :level] #(limit (+ amt (double %)) [minzoom maxzoom]))
              ^Point2D new-pos-units (grid/pixels-to-units pt newspecs :translate)
              ^Point2D dpos-units (.subtract new-pos-units pos-units)
              ^Point2D dpos-pixels (grid/units-to-pixels dpos-units newspecs)
              ^Point2D new-origin (.add dpos-pixels (:origin specs))
              newspecs (assoc-in newspecs [:origin] new-origin)]
          (swap! (:view-data doc) map-replace newspecs)))))  
  (init-handlers! [doc]
    ;; Add mouse events to deal with pan, zoom, drag
    (let [^javafx.scene.Node pane (:doc-pane doc)
          mouse-state (atom {})
          capture-mouse! (fn [^MouseEvent event & [^Point2D last-click-pos]]
                           ;; Capture current mouse status and the delta move since last call
                           (let [oldpos (:pos @mouse-state (Point2D/ZERO))
                                 last-click-pos (or last-click-pos (:last-click-pos @mouse-state (Point2D/ZERO)))
                                 pos (Point2D. (.getX event) (.getY event))]
                             (swap! mouse-state assoc
                                    :pos pos
                                    :dpos (.subtract pos oldpos)
                                    :last-click-pos last-click-pos
                                    :primary (.isPrimaryButtonDown event)
                                    :secondary (.isSecondaryButtonDown event)
                                    :middle (.isMiddleButtonDown event)
                                    :ctrl (.isControlDown event)
                                    :alt (.isAltDown event)
                                    :shift (.isShiftDown event))))
          ;; keyboard state is probably not necessary since modifier
          ;; keys are available with every mouse event, which is a
          ;; GestureEvent
          kbd-state (atom {})
          capture-kbd! (fn [^KeyEvent event]
                         ;; Capture state of modifier keys
                         (swap! kbd-state assoc
                                :ctrl (.isControlDown event)
                                :shift (.isShiftDown event)
                                :alt (.isAltDown event)))

          long-id (fn [idtxt] (str idtxt "-" (:uuid doc)))
          lookup-node (memoize (fn [idtxt node] ;; always looks down starting at node
                                 (let [         ;;id (:uuid doc)
                                       ;;long-id (str idtxt "-" id)
                                       ]
                                   (lookup (long-id idtxt) node))))

          move-handler (event-handler [mouse-event]
                                      (let [^MouseEvent event mouse-event
                                            ppos (Point2D. (.getX event) (.getY event))
                                            upos (grid/pixels-to-units ppos @(:view-data doc) :translate)
                                            ppos-label (lookup-node "pixel-pos-label" pane)
                                            upos-label (lookup-node "unit-pos-label" pane)]                                        
                                        (.setText ppos-label (format "px:% 5.5f, % 5.5f" (.getX ppos) (.getY ppos)))
                                        (.setText upos-label (format "ux:% 5.5f, uy: % 5.5f" (.getX upos) (.getY upos)))))

          click-handler (event-handler [mouse-event] ;; We only need this to capture the initial state of the mouse
                                       ;; We look at mouse event (down+up = 2) * (2^2=4 mouse states) * (2^3=8 key states)
                                       ;; = 2*4*8 = 64 combinations of this event.  How to encode nicely?
                                       (let [^MouseEvent event mouse-event]
                                         (capture-mouse! event (Point2D. (.getX event) (.getY event)))
                                         (.requestFocus pane)
                                         (let [ms @mouse-state
                                               button (.getButton event)]
                                           (cond (:primary ms) (println "primary down")
                                                 (= button MouseButton/PRIMARY) (println "primary up, pick or select item"))
                                           (cond(:secondary ms) (println "secondary down")
                                                (= button MouseButton/SECONDARY) (println "secondary up"))
                                           (cond (:middle ms) (println "middle down")
                                                 (= button MouseButton/MIDDLE) (println "middle up"))
                                           (cond (:ctrl ms) (println "ctrl at click"))
                                           (cond (:alt ms) (println "alt at click"))
                                           (cond (:shift ms) (println "shift click")))))

          drag-handler (event-handler [mouse-event]
                                      (capture-mouse! mouse-event)
                                      (let [ms @mouse-state]
                                        (cond (and (:primary ms) (not (:secondary ms)))
                                              (println "move picked item or selected items")

                                              (and (not (:primary @mouse-state)) (:secondary ms))
                                              (pan-by! doc (:dpos @mouse-state))

                                              (and (:primary @mouse-state) (:secondary ms))
                                              (let [^Point2D dpos (:dpos @mouse-state)
                                                    amt (/ (.getY dpos) -10.0)]
                                                (zoom-by! doc amt (:last-click-pos @mouse-state)))
                                             
                                              :else (println "not supported"))))
          
          scroll-handler (event-handler [scroll-event]
                                        (let [^ScrollEvent event scroll-event
                                              x (.getX event)
                                              y (.getY event)
                                              dx (.getDeltaX event)
                                              dy (.getDeltaY event)
                                              ctrl (.isControlDown event) 
                                              scroll-amt (/ dy 40.0)
                                              pan-amt-x (/ dx 5.0)
                                              pan-amt-y (/ dy 5.0)]
                                          (if ctrl (zoom-by! doc scroll-amt (Point2D. x y))
                                              (pan-by! doc (Point2D. pan-amt-x pan-amt-y))))) ;; pan vertically or horizontally
          
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
                                                     [:zoom :pixels-per-grid] new-ppg))))
]
      
      (set-event-handler! pane :mouse-moved move-handler)
      (set-event-handler! pane :mouse-pressed click-handler)
      (set-event-handler! pane :mouse-released click-handler)
      (set-event-handler! pane :mouse-dragged drag-handler)
      (set-event-handler! pane :scroll scroll-handler)
      (set-event-handler! pane :key-pressed keyboard-handler)
      (set-event-handler! pane :key-released keyboard-handler)
      (set-event-handler! (lookup-node "reset-button" pane) :action reset-btn-handler)
      (set-event-handler! (lookup-node "cm-button" pane) :action unit-btn-handler)
      (set-event-handler! (lookup-node "inch-button" pane) :action unit-btn-handler)
      (add-listener! pane :focused focus-listener))))

(defn make-idfn [id]
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
  ([^String id]
   (let [idfn (make-idfn id)
         mppl (jfxnew Label "doc mouse label" :id (idfn "pixel-pos-label"))
         mupl (jfxnew Label "doc mouse label" :id (idfn "unit-pos-label"))
         spring (Region.)
         zoom-slider1 (jfxnew Slider -10.0 10.0 0.0 :id (idfn "zoom-slider"))
         zoom-txt (jfxnew Label "Zoom level" :id (idfn "zoom-label"))]
     (HBox/setHgrow spring Priority/ALWAYS)
     (mb/status-bar [mppl mupl spring zoom-slider1 zoom-txt]))))

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


(defn document
  "Make document with relevant handlers"
  [width height]
  (let [id (uuid)
        view-data (atom DEFAULT-VIEW-SPECS)
        grid-canvas (canvas/resizable-canvas (grid/resize-callback view-data))
        surface-pane (jfxnew StackPane :children [grid-canvas])
        doc-pane (border-pane
                  :center surface-pane,
                  :top (doc-tool-bar id),
                  :bottom (doc-status-bar id)
                  :size [width height])
        doc (->Document view-data grid-canvas surface-pane doc-pane id [])]
    (add-watch view-data id (fn [k r o n] (redraw-view! doc)))
    (init-handlers! doc)
    doc))

(defn editor
  ;; Start a new editor with a few documents
  ([app] (editor app [nil nil]))
  ([app [width height]]
   (let [doc1 (document width height)
         doc2 (document width height)
         center-dock-base (docks/base :left (docks/node (:doc-pane doc1) "document1")
                                      :right (docks/node (:doc-pane doc2) "document2"))
         top-pane (border-pane
                   :center center-dock-base,
                   :top (editor-tool-bar),
                   :bottom (editor-status-bar)
                   :size [width height])
         editor (->Editor top-pane [doc1 doc2] [])]
     
     ;; How it works, for each doc:
     ;; 1.  Add watch to atom which calls protocol redraw
     ;; 2.  Add FX handlers to deal with mouse, keyboard input
     ;; 3.  Handler calls protocol zoom, pan, etc
     ;; 3.  Protocol fn mutates atom which triggers protocol redraw
     
     (when width (.setPrefWidth top-pane width))
     (when height (.setPrefHeight top-pane height))

     editor)))





(defn draw-test! [^Canvas canvas which]
  (cond (= which :grid) (doto canvas (toydb.grid/draw-grid! DEFAULT-VIEW-SPECS))
        (= which :square)
        (let [w (.getWidth canvas)
              h (.getHeight canvas)
              bor 50.0]
          (doto (.getGraphicsContext2D canvas)
            (.clearRect 0 0 w h)
            (.setFill Color/BLUE)
            (.fillRect bor bor (- w bor bor) (- h bor bor))))
        :else nil))

(defn draw-callback [^Canvas canvas ^Point2D old-size]
  (draw-test! canvas :grid))

(defn test1 []
  (let [canvas (canvas/resizable-canvas draw-callback)]
    (stage canvas [300 300])
    (println (.getWidth canvas) (.getHeight canvas))
    canvas))
