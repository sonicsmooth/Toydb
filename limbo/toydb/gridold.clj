(ns ^{:doc "Low level grid functions" }
    toydb.grid
    (:use [jfxutils.core :exclude [-main]])
    (:use [clojure.data :only [diff]])
    (:use [clojure.pprint])

    (:import [javafx.geometry Insets VPos Point2D]
             [javafx.scene.canvas Canvas GraphicsContext]
             [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
             [javafx.scene.text Font Text TextAlignment]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

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
  ([^Point2D p]
   (.add p 1.0 1.0)))

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
  units-per-pixel, and pixels-per-unit based on zoom specs, which is a
  map with :minors-per-major, :zoom-ratio, and :zoom-level keys"
  [zoomspecs]
  (let [calc-Amult ;; Rescales pixels-per-grid.
        (fn ([{:keys [minors-per-major ^double ratio ^double level]}] 
                     (Math/pow (first minors-per-major)
                               (mod (/ level ratio) 1.0))))
        
        calc-Bmult ;; Rescales units-per-grid.
        (fn  [{:keys [minors-per-major ^double ratio ^double level]}]
          (/ 1.0
             (Math/pow (first minors-per-major)
                       (Math/floor (/ level ratio)))))
        Amult (double (calc-Amult zoomspecs))
        Bmult (double (calc-Bmult zoomspecs))
        ppgX (* Amult (double (first (:pixels-per-grid zoomspecs))))
        ppgY (* Amult (double (second (:pixels-per-grid zoomspecs)))) 
        upgX (* Bmult (double (first (:units-per-grid zoomspecs))))
        upgY (* Bmult (double (second (:units-per-grid zoomspecs))))
        ppg (Point2D. ppgX (- ppgY))
        upg (Point2D. upgX upgY)
        upp (point-div upg ppg)
        ppu (point-div ppg upg)]
    [ppg upg upp ppu]))

;;(def calc-conversions (memoize _calc-conversions))
(def calc-conversions _calc-conversions)


(defn snap-to-nearest
  "Rounds all elements of seq to closest integer multiqple of m, which
  can be non-integer."
  [seq ^double m ]
  (letfn [(f [^double x] (* m (Math/round (/ x m))))]
    (map f seq)))

(defn snap-pt-to-nearest  [^Point2D p ^double m]
  (Point2D. (* m (Math/round (/ (.getY p) m)))
            (* m (Math/round (/ (.getX p) m)))))

(defn grid-range
  "Returns range of grid values in units not pixels, suitable for use
  in canvas with transfrom applied. upp is used for scaling the half
  pixel offset."
  [^double ustart ^double ustop ^long numpts ^double upp]
  (letfn [(linspace [^double start ^double end ^long n] ;;"Returns a sequence of n numbers from start to end inclusive."
            (if (= start end) nil
                (for [^long i (range 0 n)]
                  (+ start (* i (/ (- end start) (dec n)))))))
          (offset [seq ^double x] ;;  "Adds x to everything in seq"
            (map #(+ x (double %)) seq))]
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
  (let [[ppg upg upp ppu] (calc-conversions (:zoom specs))
        ^Point2D new-pixels (point-mult ppu point)]
    (if (= option :translate)
      (let [^Point2D origin (:origin specs)]
        (.add origin new-pixels))
      new-pixels)))

(defn ^long mm
  "Returns nanometers given mm"
  [^double x]
  (long (* x 1000000)))

(defn ^long inch
  "Returns nanometers given inches"
  [^double x]
  (long (* x 25400000)))


(defn resize-grid!
  "Updates origin so grid sticks to bottom left, then resizes
  grid. canvas is the Canvas that is being resized.  old-size is the
  Point2D describing the size of the Canvas before the resizing event.
  view-data is an Atom which has an :origin key which is a Point2D
  describing the center of the grid in pixels, on an untransformed
  Canvas, from the top-left of the Canvas."
  [^Canvas canvas ^Point2D old-size view-data]
  ;; Compute how far the bottom edge has moved, and add that to the
  ;; origin's y value
  (let [^Point2D origin (:origin @view-data)
        dy (- (.getHeight canvas) (.getY old-size))
        new-origin (Point2D. (.getX origin) (+ (.getY origin) dy))]
    (swap! view-data assoc-in [:origin] new-origin)))

(defn draw-grid!
  "Draws grid onto canvas using provided view-data"
  [^Canvas canvas view-data]
  (let [^GraphicsContext gc (.getGraphicsContext2D canvas)
        zoomspecs (:zoom view-data)
        border-pt Point2D/ZERO
        width (.getWidth canvas)
        height (.getHeight canvas)
        upper-left-pixel Point2D/ZERO
        upper-right-pixel (Point2D. width 0)
        lower-left-pixel (Point2D. 0 height)
        lower-right-pixel (Point2D. width height)

        ^Point2D mpm (vec-to-pt (:minors-per-major zoomspecs))
        ^double zoomlevel (:level zoomspecs)
        ^double zoomratio (:ratio zoomspecs)
        totalzoom (Math/pow (.getX mpm) (/ zoomlevel zoomratio))
        [^Point2D ppg ^Point2D upg ^Point2D upp ^Point2D ppu] (calc-conversions zoomspecs) ;; in Point2D
        ^Point2D origin (:origin view-data) ;; always location in canvas, in pixels, no transform

        ;; Distances in pixels from origin to border
        pleft-distance (- (.getX origin)) ;; typically negative, d
        pright-distance (- width (.getX origin)) ;; typically positive
        pupper-distance (.getY origin)   ;; typically positive
        plower-distance (- (.getY origin) height)  ;; typically negative

        ^Point2D ulower-left (point-mult upp (Point2D. pleft-distance plower-distance))  ;; pixels->units
        ^Point2D uupper-right (point-mult upp (Point2D. pright-distance pupper-distance)) ;; pixels->units
        ^Point2D umajstep upg 
        ^Point2D uminstep (point-div upg mpm)
        ^Point2D umajstart (point-mult (point-ceil (point-div ulower-left umajstep)) umajstep)
        ^Point2D umajstop (point-mult (point-floor (point-div uupper-right umajstep)) umajstep)
        ^Point2D uminstart (point-mult (point-ceil (point-div ulower-left uminstep)) uminstep)
        ^Point2D uminstop (point-mult (point-floor (point-div uupper-right uminstep)) uminstep)
        ^Point2D num-maj-pts (point-inc (point-div (.subtract umajstop umajstart) umajstep))
        ^Point2D num-min-pts (point-inc (point-div (.subtract uminstop uminstart) uminstep))
        
        xs-majors (grid-range (.getX umajstart) (.getX umajstop) (.getX num-maj-pts) (.getX upp))
        ys-majors (grid-range (.getY umajstart) (.getY umajstop) (.getY num-maj-pts) (.getY upp))
        xs-minors (grid-range (.getX uminstart) (.getX uminstop) (.getX num-min-pts) (.getX upp))
        ys-minors (grid-range (.getY uminstart) (.getY uminstop) (.getY num-min-pts) (.getY upp))

        background-paint (-> view-data :colors :background)
        major-line-color (-> view-data :colors :major-line-color) 
        minor-line-color (-> view-data :colors :minor-line-color)
        axis-line-color (-> view-data :colors :axis-line-color)]

    ;; Clear everything in identity state...
    ;; Transform takes us to unit space, with Y upside down
    (doto gc
      (.setTransform 1 0  0 1  0 0) ;; identity
      ;;(.clearRect 0 0 (.getWidth canvas) (.getHeight canvas))
      (.setFill background-paint)
      (.fillRect 0 0 (.getWidth canvas) (.getHeight canvas))) ;; main transform


    ;; ...then set transform
    ;; We are drawing a grid in unit space transformed to pixel space.
    ;; We are not drowing in grid space.
    ;; The order here matters!  translate, then scale! (or does it?)
    ;;(.setTransform (.getX ppu) 0  0 (.getY ppu) (.getX origin) (.getY origin))
    (doto gc
      (.save)
      (.translate (+ (.getX origin)) (+ (.getY origin)))
      (.scale (.getX ppu) (- (.getY ppu))))

    ;; Draw minor verticals and horizontals
    (.setStroke gc minor-line-color)
    (.setLineWidth gc (* 0.25 (.getX upp)))
    (doseq [x xs-minors] (.strokeLine gc x (.getY ulower-left)  x (.getY uupper-right))) ;; vertical lines
    (doseq [y ys-minors] (.strokeLine gc (.getX ulower-left) y (.getX uupper-right) y)) ;; horizontal lines

    ;; Draw major verticals and horizontals
    (.setStroke gc major-line-color)
    (.setLineWidth gc (* 0.5 (.getX upp)))
    (doseq [x xs-majors] (.strokeLine gc x (.getY ulower-left)  x (.getY uupper-right))) ;; vertical lines
    (doseq [y ys-majors] (.strokeLine gc (.getX ulower-left) y  (.getX uupper-right) y)) ;; horizontal lines
   
    ;; Then the axes
    (.setStroke gc axis-line-color)
    (.setLineWidth gc (* 2 (.getX upp)))
    (when (and (> (.getY origin) 0) (< (.getY origin) height))
      (.strokeLine gc (.getX ulower-left) 0 (.getX uupper-right) 0)) ;; horizontal

    (when (and (> (.getX origin) 0) (< (.getX origin) width))
      (.strokeLine gc 0 (.getY ulower-left) 0 (.getY uupper-right) )) ;; vertical

    ;; Test diagonal.  Should go from origin to upper right
    (.strokeLine gc 0 0 1 1)

    ;; Restore back to non-transformed then draw text
    (doto gc
      (.restore)
      (.setFont (Font. 16.0))
      (.setFill Color/DARKBLUE)
      (.setTextBaseline VPos/CENTER)
      (.fillText (format "%5.3f zoom level" (:level zoomspecs) ) 10 (- height 100))
      (.fillText (format "%5.3f %s per major gridline" (.getX upg) (name (:unit view-data))) 10  (- height 80))
      (.fillText (format "%5.3f px per major gridline" (.getX ppg)) 10  (- height 60))
      (.fillText (format "Zoom: %5.3f total zoom " totalzoom)       10  (- height 40))
      (.fillText (format "Scale: %5.3f pixels per %s" (.getX ppu) (name (:unit view-data)))  10 (- height 20)))
    canvas))

(defn resize-callback
  "Returns function which is called by .resize callback in reified Canvas"
  [view-data]
  (fn [canvas old-size]
    (resize-grid! canvas old-size view-data)))


















