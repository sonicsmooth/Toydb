(ns ^{:doc "Low level grid functions" }
    toydb.grid
    (:use [jfxutils.core :exclude [-main]])
    (:use [clojure.data :only [diff]])
    (:use [clojure.pprint])

    (:import ;;[javafx.beans.property SimpleDoubleProperty
             ;;SimpleLongProperty] [javafx.geometry Orientation
             ;;Point2D] [javafx.scene CacheHint Group Scene]
             ;;[javafx.scene.control Button Label Menu MenuBar
             ;;MenuItem ToolBar] [javafx.scene.input MouseEvent
             ;;ScrollEvent] [javafx.scene.layout Pane StackPane VBox
             ;;HBox Background BackgroundFill CornerRadii]
             ;;[javafx.scene.shape Rectangle Circle Ellipse
             ;;StrokeLineCap] [javafx.stage Stage FileChooser]
             [javafx.geometry Insets VPos Point2D]
             [javafx.scene.canvas Canvas GraphicsContext]
             [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
             [javafx.scene.text Font Text TextAlignment]))
;;[javafx.application Platform Application]

;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(defn vec-to-pt
  "Returns new Point2D based on 2-seq"
  [[x y]]
  (Point2D. x y))

(defn point-mult
  "Element-by-element multiply of two Point2D objects.  Returns single
  Point2D."
  [^Point2D p1 ^Point2D p2]
  (Point2D. (* (.getX p1) (.getX p2)) (* (.getY p1) (.getY p2))))

(defn point-div
  "Element-by-element divide of two Point2D objects.  Returns single
  Point2D."
  [^Point2D p1 ^Point2D p2]
  (Point2D. (/ (.getX p1) (.getX p2)) (/ (.getY p1) (.getY p2))))

(defn point-add
  "Add single value to both X and Y"
  [^Point2D p ^double i]
  (.add p (Point2D. i i)))

(defn point-inc
  "Adds 1.0 to both X and Y"
  [^Point2D p]
  (Point2D. (+ (.getX p) 1.0) (+ (.getY p) 1.0)))

(defn point-inc!
  "Adds 1.0 to both X and Y"
  [^Point2D p]
  (Point2D. (+ (.getX p) 1.0) (+ (.getY p) 1.0)))

(defn point-ceil
  "Element-by-element ceiling of one Point2D object.  Returns single
  Point2D."
  [^Point2D p]
  (Point2D. (Math/ceil (.getX p)) (Math/ceil (.getY p))))

(defn point-floor
  "Element-by-element floor of one Point2D object.  Returns single
  Point2D."
  [^Point2D p]
  (Point2D. (Math/floor (.getX p)) (Math/floor (.getY p))))

(defn _calc-conversions
  "Calculates and returns scaled pixels-per-grid, units-per-grid,
  units-per-pixel, and pixels-per-unit based on zoom specs, which is
  map with :minors-per-major, :zoom-ratio, and :zoom-level keys"
  [zoomspecs]
  (let [calc-Amult ;; Rescales pixels-per-grid.
        (fn ([{:keys [minors-per-major zoom-ratio zoom-level]}] 
                        (Math/pow (first minors-per-major)
                                  (mod (/ zoom-level zoom-ratio) 1.0))))
        
        calc-Bmult ;; Rescales units-per-grid.
        (fn [{:keys [minors-per-major zoom-ratio zoom-level]}]
                     (/ 1.0
                        (Math/pow (first minors-per-major)
                                  (Math/floor (/ zoom-level zoom-ratio)))))
        Amult (calc-Amult zoomspecs)
        Bmult (calc-Bmult zoomspecs)
        ppgX (* Amult (first (:pixels-per-grid zoomspecs)))
        ppgY (* Amult (second (:pixels-per-grid zoomspecs))) 
        upgX (* Bmult (first (:units-per-grid zoomspecs)))
        upgY (* Bmult (second (:units-per-grid zoomspecs)))
        ppg (Point2D. ppgX ppgY)
        upg (Point2D. upgX upgY)
        upp (point-div upg ppg)
        ppu (point-div ppg upg)]
    [ppg upg upp ppu]))

;;(def calc-conversions (memoize _calc-conversions))
(def calc-conversions _calc-conversions)

(defn snap-to-nearest
  "Rounds all elements of seq to closest integer multiqple of m, which
  can be non-integer."
  [seq m]
  (letfn [(f [x] (* m (Math/round (double (/ x m)))))]
    (map f seq)))

(defn snap-pt-to-nearest ^Point2D [^Point2D p ^Double m]
  (Point2D. (* m (Math/round (double (/ (.getY p) m))))
            (* m (Math/round (double (/ (.getX p) m))))))

(defn grid-range
  "Returns range of grid values in units not pixels, suitable for use
  in canvas with transfrom applied. upp is used for scaling the half
  pixel offset."
  [ustart ustop numpts upp]
  (letfn [(linspace [start end n] ;;"Returns a sequence of n numbers from start to end inclusive."
            (if (= start end) nil
                (for [i (range 0 n)]
                  (+ start (* i (/ (- end start) (dec n)))))))
          (offset [seq x] ;;  "Adds x to everything in seq"
            (map #(+ x %) seq))]
    (let [rng (linspace ustart ustop numpts)
          snapped-rng (snap-to-nearest rng upp) ;; upp is shorthand for 1 pixel * upp
          offset-snapped-rng (offset snapped-rng (* 0.5 upp))]
      offset-snapped-rng)))

(defn pixels-to-units
  "Returns unit coordinates given x,y location in canvas and
  appropriate grid specs.  If :translate is passed as final argument,
  pixels are translated from/to origin before scaling to units.
  Otherwise, without :translate option, only scaling is performed."
  ^Point2D [^Point2D point specs & [option]]
  (let [[ppg upg upp ppu] (calc-conversions (:zoom specs))]
    (if (= option :translate)
      (point-mult upp (.subtract point (:origin specs)))
      (point-mult upp point))))

(defn units-to-pixels 
  "Returns pixel coordinates given x,y location in units and
  appropriate grid specs.  If :translate is passed as final argument,
  units are translated from/to origin before scaling to pixels.
  Otherwise, without :translate option, only scaling is performed."
  ^Point2D [^Point2D point specs & [option]]
  (let [ [ppg upg upp ppu] (calc-conversions (:zoom specs))
        new-pixels (point-mult ppu point)]
    (if (= option :translate)
      (.add (:origin specs) new-pixels)
      new-pixels)))

(defn ^long mm
  "Returns nanometers given mm"
  [x]
  (long (* x 1000000)))

(defn ^long inch
  "Returns nanometers given inches"
  [x]
  (long (* x 25400000)))


(defn resize-grid!
  "Updates origin so grid sticks to bottom left, then resizes
  grid. canvas is the Canvas thas is being resized.  old-size is the
  Point2D describing the size of the Canvas before the resizing event.
  view-data has an :origin key which is a Point2D describing the
  center of the grid in pixels, on an untransformed Canvas, from the
  top-left of the Canvas."
  [canvas old-size view-data]
  ;; Compute how far the bottom edge has moved, and add that to the
  ;; origin's y value
  (let [origin (:origin @view-data)
        dy (- (.getHeight canvas) (.getY old-size))
        new-origin (Point2D. (.getX origin) (+ (.getY origin) dy))]
    (swap! view-data assoc-in [:origin] new-origin)
    (draw-grid! canvas @view-data)))

(defn draw-grid!
  "Draws grid onto canvas using provided view-data"
  [canvas view-data] 
  (let [gc (.getGraphicsContext2D canvas)
        zoomspecs (:zoom view-data)
        border (:canvas-border view-data 0.0 )
        border-pt (vec-to-pt border)
        width (.getWidth canvas)
        height (.getHeight canvas)
        size-pt (Point2D. width height)


        pleft (.getX border-pt)
        pright (- width (.getX border-pt))
        pupper (.getY border-pt)
        plower (- height (.getY border-pt))
        upper-left-pixel (Point2D. pleft pupper)
        upper-right-pixel (Point2D. pright pupper)
        lower-left-pixel (Point2D. pleft plower)
        lower-right-pixel (Point2D. pright plower)

        mpm (vec-to-pt (:minors-per-major zoomspecs))
        zoomlevel (:zoom-level zoomspecs)
        zoomratio (:zoom-ratio zoomspecs)
        totalzoom (Math/pow (.getX mpm) (/ zoomlevel zoomratio))
        [ppg upg upp ppu] (calc-conversions zoomspecs) ;; in Point2D
        origin (:origin view-data) ;; always location in canvas, in pixels, no transform

        ;; Distances in pixels from origin to border
        pleft-distance (- pleft (.getX origin)) ;; typically negative, d
        pright-distance (- pright (.getX origin)) ;; typically positive
        pupper-distance (- (.getY origin) pupper) ;; typically positive
        plower-distance (- (.getY origin) plower)  ;; typically negative

        ulower-left (point-mult upp (Point2D. pleft-distance plower-distance))  ;; pixels->units
        uupper-right (point-mult upp (Point2D. pright-distance pupper-distance)) ;; pixels->units
        umajstep upg 
        uminstep (point-div upg mpm)
        umajstart (point-mult (point-ceil (point-div ulower-left umajstep)) umajstep)
        umajstop (point-mult (point-floor (point-div uupper-right umajstep)) umajstep)
        uminstart (point-mult (point-ceil (point-div ulower-left uminstep)) uminstep)
        uminstop (point-mult (point-floor (point-div uupper-right uminstep)) uminstep)
        num-maj-pts (point-inc (point-div (.subtract umajstop umajstart) umajstep))
        num-min-pts (point-inc (point-div (.subtract uminstop uminstart) uminstep))
        
        xs-majors (grid-range (.getX umajstart) (.getX umajstop) (.getX num-maj-pts) (.getX upp))
        ys-majors (grid-range (.getY umajstart) (.getY umajstop) (.getY num-maj-pts) (.getY upp))
        xs-minors (grid-range (.getX uminstart) (.getX uminstop) (.getX num-min-pts) (.getX upp))
        ys-minors (grid-range (.getY uminstart) (.getY uminstop) (.getY num-min-pts) (.getY upp))

        background-paint (-> view-data :colors :background)
        major-line-color (-> view-data :colors :major-line-color) 
        minor-line-color (-> view-data :colors :minor-line-color)]

    ;; Clear everything in identity state...
    ;; Transform takes us to unit space, with Y upside down
    (doto gc
      (.setTransform 1 0  0 1  1 1) ;; identity
      ;;(.clearRect 0 0 (.getWidth canvas) (.getHeight canvas))
      (.setFill background-paint)
      (.fillRect 0 0 (.getWidth canvas) (.getHeight canvas))) ;; main transform


    ;; ...then set transform
    ;; We are drawing a grid in unit space transformed to pixel space.
    ;; We are not drowing in grid space.
    ;; The order here matters!  translate, then scale!
    ;;(.setTransform (.getX ppu) 0  0 (.getY ppu) (.getX origin) (.getY origin))
    (doto gc
      (.translate (+ (.getX origin)) (+ (.getY origin)))
      (.scale (.getX ppu) (- (.getY ppu))))

    ;; Draw minor verticals and horizontals
    (.setStroke gc minor-line-color)
    (.setLineWidth gc (* 0.25 (.getX upp)))
    (doseq [x xs-minors] (.strokeLine gc x (.getY ulower-left)  x (.getY uupper-right))) ;; vertical lines
    (doseq [y ys-minors] (.strokeLine gc (.getX ulower-left) y (.getX uupper-right) y)) ;; horizontal lines

    #_(doto gc
      (.setFill Color/RED)
      (.fillRect -0.1 -0.1 0.2 0.2)

      (.setFill Color/BLUE)
      (.fillRect (- (first xs-minors) 0.05) (- (.getY ulower-left) 0.05) 0.1 0.1)
      (.fillRect (- (last xs-minors) 0.05) (- (.getY uupper-right) 0.05) 0.1 0.1)

      (.setStroke minor-line-color)
      (.setLineWidth (* 0.25 (.getX upp)))
      (.strokeLine  (first xs-minors) (.getY ulower-left) (last xs-minors) (.getY uupper-right)))
    
    (.setLineWidth gc (* 0.5 (.getX upp)))
    (.setStroke gc major-line-color)
    (doseq [x xs-majors] (.strokeLine gc x (.getY ulower-left)  x (.getY uupper-right)))
    (doseq [y ys-majors] (.strokeLine gc (.getX ulower-left) y  (.getX uupper-right) y))
    
    ;; Then the axes
    (.setStroke gc Color/DARKBLUE)
    (.setLineWidth gc (* 2 (.getX upp)))
    (when (and (> (.getY origin) pupper)
               (< (.getY origin) plower))
      (.strokeLine gc (.getX ulower-left) 0 (.getX uupper-right) 0)) ;; horizontal

    (when (and (> (.getX origin) pleft)
               (< (.getX origin) pright))
      (.strokeLine gc 0 (.getY ulower-left) 0 (.getY uupper-right) )) ;; vertical

    ;; Test diagonal
    (.strokeLine gc 0 0 1 1)

    ;; Restore back to non-transformed then draw borders
    (.setFont gc (Font/font 10.0))
    (doto gc
      (.setTransform 1 0  0 1  0 0) ;; identity
      (.setLineWidth 2.0)
      (.setStroke Color/CHOCOLATE)
      (.strokeRect pleft pupper (- pright pleft) (- plower pupper)) ;; inner border
      (.setStroke Color/RED)
      (.strokeRect 0 0 width height) ;; outer border
      
      (.setFont (Font. 16.0))
      (.setFill Color/WHITE)
      (.setTextBaseline VPos/CENTER)
      (.fillText (format "%5.3f %s per major gridline" (.getX upg) (:unit-name view-data)) (+ 10 pleft) (- plower 80))
      (.fillText (format "%5.3f px per major gridline" (.getX ppg)) (+ 10 pleft) (- plower 60))
      (.fillText (format "Zoom: %5.3f total zoom " totalzoom)       (+ 10 pleft) (- plower 40))
      (.fillText (format "Scale: %5.3f pixels per %s" (.getX ppu) (:unit-name view-data))   (+ 10 pleft) (- plower 20)))
    canvas))









