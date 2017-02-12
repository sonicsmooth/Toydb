(ns ^{:doc "Low level grid functions" }
    toydb.grid
  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure.data :only [diff]])
  (:use [clojure.pprint])

  (:import ;;[javafx.application Platform Application]
   ;;[javafx.beans.property SimpleDoubleProperty SimpleLongProperty]
   [javafx.geometry Insets VPos]
   ;;[javafx.stage Stage FileChooser]
   ;;[javafx.scene CacheHint Group Scene]
   [javafx.scene.canvas Canvas GraphicsContext]
   ;;[javafx.scene.control Button Label Menu MenuBar MenuItem ToolBar]
   ;;[javafx.geometry Orientation Point2D]
   ;;[javafx.scene.input MouseEvent ScrollEvent]
   ;;[javafx.scene.shape Rectangle Circle Ellipse StrokeLineCap]
   ;;[javafx.scene.layout Pane StackPane VBox HBox Background BackgroundFill CornerRadii]
   [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
   [javafx.scene.text Font Text TextAlignment]))

;;(set! *warn-on-reflection* true)
;;(set! *unchecked-math* :warn-on-boxed)

(defprotocol GridProtocol
  (draw-grid! [this view-data]))


(defn _calc-conversions
  "Calculates scaled ppg, upg, upp, ppu according to specs and accessor.
  accesior is usually first or second, depending on whether it's X or
  Y."
  [zoomspecs accessor]
  (letfn [(calc-Amult
            ([{:keys [minors-per-major zoom-ratio zoom-level]}] ;; "Rescales pixels-per-grid.  Specs is map  with :minors-per-major, :zoom-ratio, and :zoom-level keys.  Returns double."
             (Math/pow (first minors-per-major)
                       (mod (/ zoom-level zoom-ratio) 1.0))))
          (calc-Bmult
            ([{:keys [minors-per-major zoom-ratio zoom-level] :as zoomspecs}] ;;"Rescales units-per-grid.  Specs is map with :minors-per-major, :zoom-ratio, and :zoom-level keys.  Returns double."
             (/ 1.0
                (Math/pow (first minors-per-major)
                          (Math/floor (/ zoom-level zoom-ratio))))))]
    (let [Amult (calc-Amult zoomspecs)
          Bmult (calc-Bmult zoomspecs)
          ppg (* Amult (accessor (:pixels-per-grid zoomspecs)))
          upg (* Bmult (accessor (:units-per-grid zoomspecs)))
          upp (/ upg ppg)
          ppu (/ ppg upg)]
      [ppg upg upp ppu])))

(def calc-conversions (memoize _calc-conversions))

(defn  snap-to-nearest
  "Rounds all elements of seq to closest integer multiple of m, which  can be non-integer."
  [seq m] 
  (letfn [(f [x] (* m (Math/round (double (/ x m)))))]
    (map f seq)))

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




(extend-protocol GridProtocol
  javafx.scene.canvas.Canvas
  #_(reset-grid! [canvas view-atom new-view-data]
    (swap! view-atom conj new-view-data)
    (draw-grid! canvas @view-atom))
  
  (draw-grid! [canvas view-data] ;; "Draws grid onto canvas, assuming canvas already has userdata with the specs"
    (let [gc (.getGraphicsContext2D canvas)
          zoomspecs (:zoom view-data)
          border (or (:canvas-border view-data) [0.0 0.0])
          [cw ch] [(.getWidth canvas) (.getHeight canvas)]
          [minX minY] border
          [maxX maxY] [(- cw (first border)) (- ch (second border))]
          [mpmX mpmY] (:minors-per-major zoomspecs)
          zoomlevel (:zoom-level zoomspecs)
          zoomratio (:zoom-ratio zoomspecs)
          totalzoom (Math/pow mpmX (/ zoomlevel zoomratio))
          [ppgX upgX uppX ppuX] (calc-conversions zoomspecs first) ;; px/grid; units/grid; units/px; px/unit
          [ppgY upgY uppY ppuY] (calc-conversions zoomspecs second)
          origin-pt2d (:origin view-data)
          oX (.getX origin-pt2d)
          oY (.getY origin-pt2d)
          [uminX uminY] [(* uppX (- minX oX)) (* uppY (- minY oY))]
          [umaxX umaxY] [(* uppX (- maxX oX)) (* uppY (- maxY oY))]
          [umajstepX umajstepY] [upgX upgY]
          [uminstepX uminstepY] [(/ upgX mpmX) (/ upgY mpmY)]
          [umajstartX umajstartY] [(* (Math/ceil  (/ uminX umajstepX)) umajstepX)
                                   (* (Math/ceil  (/ uminY umajstepY)) umajstepY)]
          [umajstopX umajstopY]   [(* (Math/floor (/ umaxX umajstepX)) umajstepX)
                                   (* (Math/floor (/ umaxY umajstepY)) umajstepY)]
          [uminstartX uminstartY] [(* (Math/ceil  (/ uminX uminstepX)) uminstepX)
                                   (* (Math/ceil  (/ uminY uminstepY)) uminstepY)]
          [uminstopX uminstopY]   [(* (Math/floor (/ umaxX uminstepX)) uminstepX)
                                   (* (Math/floor (/ umaxY uminstepY)) uminstepY)]
          [majptsX majptsY] [(inc (/ (- umajstopX umajstartX) umajstepX)) (inc (/ (- umajstopY umajstartY) umajstepY))]
          [minptsX minptsY] [(inc (/ (- uminstopX uminstartX) uminstepX)) (inc (/ (- uminstopY uminstartY) uminstepY))]
          xs-majors (grid-range umajstartX umajstopX majptsX uppX)
          ys-majors (grid-range umajstartY umajstopY majptsY uppY)
          xs-minors (grid-range uminstartX uminstopX minptsX uppX)
          ys-minors (grid-range uminstartY uminstopY minptsY uppY)]

      ;; Clear everything in identity state, then set transform
      ;; Transform takes us to unit space.
      (doto gc
        (.setTransform 1 0  0 1  1 1) ;; identity
        (.clearRect 0 0 (.getWidth canvas) (.getHeight canvas))
        (.setTransform ppuX 0  0 ppuY  oX oY)) ;; main transform

      ;; We are drawing a grid in unit space transformed to pixel space.
      ;; We are not drowing in grid space.
      (.setStroke gc Color/BLACK)
      (.setLineWidth gc (* 0.25 uppX))
      (.setFont gc (Font/font 10.0))
      (doseq [x xs-minors]  (.strokeLine gc x uminY x umaxY)) ;; vertical lines
      (doseq [y ys-minors] (.strokeLine gc uminX y umaxX y)) ;; horizontal lines
      

      (.setLineWidth gc (* 0.5 uppX))
      (doseq [x xs-majors] (.strokeLine gc x uminY x umaxY)) ;; vertical lines
      (doseq [y ys-majors] (.strokeLine gc uminX y umaxX y)) ;; horizontal lines
      
      ;; Then the axes
      (.setStroke gc Color/DARKBLUE)
      (.setLineWidth gc (* 2 uppX))
      (when (and (> oY minY) (< oY maxY))
        (.strokeLine gc uminX 0 umaxX 0)) ;; horizontal

      (when (and (> oX minX) (< oX maxX))
        (.strokeLine gc 0 uminY 0 umaxY)) ;; vertical

      ;; Restore back to non-transformed then draw borders
      (doto gc
        (.setTransform 1 0  0 1  0 0) ;; identity
        (.setLineWidth 2.0)
        (.setStroke Color/CHOCOLATE)
        (.strokeRect minX minY (- maxX minX) (- maxY minY)) ;; inner border
        (.setStroke Color/RED)
        (.strokeRect 0 0 (.getWidth canvas) (.getHeight canvas)) ;; outer border
        
        (.setFont (Font. 16.0))
        (.setFill Color/WHITE)
        (.setTextBaseline VPos/CENTER)
        (.fillText (format "%5.3f %s per major gridline" (float upgX) (:unit-name view-data)) (+ 10 minX) (- maxY 80))
        (.fillText (format "%5.3f px per major gridline" (float ppgX)) (+ 10 minX) (- maxY 60))
        (.fillText (format "Zoom: %5.3f total zoom " totalzoom)             (+ 10 minX) (- maxY 40))
        (.fillText (format "Scale: %5.3f pixels per %s" ppuX (:unit-name view-data))   (+ 10 minX) (- maxY 20))))))        






(defn pixels-to-units
  "Returns unit coordinates given x,y location in canvas and
  appropriate grid specs.  If :translate is passed as final argument,
  pixels are translated from/to origin before scaling to units.
  Otherwise, without :translate option, only scaling is performed."
  [[xpixel ypixel] specs & [option]]
  (let [[ppgX upgX uppX ppuX] (calc-conversions (:zoom specs) first)
        [ppgY upgY uppY ppuY] (calc-conversions (:zoom specs) second)
        [oX oY] (:origin (:canvas specs))]
    (if (= option :translate)
      [(* uppX (- xpixel oX)) (* uppY (- ypixel oY))]
      [(* uppX    xpixel)     (* uppY    ypixel)])))

(defn units-to-pixels
  "Returns pixel coordinates given x,y location in units and
  appropriate grid specs.  If :translate is passed as final argument,
  units are translated from/to origin before scaling to pixels.
  Otherwise, without :translate option, only scaling is performed."
  [[xunit yunit] specs & [option]]
  (let [[ppgX upgX uppX ppuX] (calc-conversions (:zoom specs) first)
        [ppgY upgY uppY ppuY] (calc-conversions (:zoom specs) second)
        [oX oY] (:origin (:canvas specs))]
    (if (= option :translate)
      [(+ oX (* ppuX xunit)) (+ oY (* ppuY yunit))]
      [      (* ppuX xunit)        (* ppuY yunit)])))

(defn ^long mm
  "Returns nanometers given mm"
  [x]
  (long (* x 1000000)))

(defn ^long inch
  "Returns nanometers given inches"
  [x]
  (long (* x 25400000)))









