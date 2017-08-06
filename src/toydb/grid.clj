(ns ^{:doc "Low level grid functions" }
    toydb.grid
    (:use [jfxutils.core :exclude [-main]])
    (:use [clojure.data :only [diff]])
    (:use [clojure.pprint])
    (:require toydb.viewdef)
    (:require [clojure.core.matrix :as matrix]
              [clojure.core.matrix.operators :as matrixop])
    (:import [javafx.geometry Insets VPos Point2D]
             [javafx.scene.canvas Canvas GraphicsContext]
             [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
             [javafx.scene.text Font Text TextAlignment]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(matrix/set-current-implementation :vectorz)

(def DEFAULT-GRID-COLORS
  {:background (simple-vert-gradient Color/SLATEGRAY Color/SILVER)
   :major-line-color Color/BLACK
   :minor-line-color Color/LIGHTGRAY
   :axis-line-color Color/DARKBLUE})

(defrecord GridSpacing [^double majspacing,
                        ^double minspacing])

(defrecord GridSpecs [^double left
                      ^double right
                      ^double top
                      ^double bottom
                      ^doubles majhsteps
                      ^doubles minhsteps
                      ^doubles majvsteps
                      ^doubles minvsteps
                      ^GridSpacing spacing])

(defrecord LineSpec [^Point2D p1
                     ^Point2D p2
                     ^double width
                     ^Color color])

;; Prematurely optimized, so faster than necessary
(defn compute-steps
  "Computes the locations, in unit space, of grid points.
The points are located spacing units apart, and are on the integer
  spacing grid.  Start and stop are the full extent, so the grid
  points will be between start and stop.  For example, if start is
  -3.2 and stop is 4.5, with spacing 1, then the output is [-3 -2 -1 0
  1 2 3 4]"
  (^doubles [^double start, ^double stop, ^double spacing]
   (let [strtd (Math/ceil (/ start spacing))
         stpd (Math/floor (/ stop spacing))
         nsteps (long (+ (- stpd strtd) 1))
         steps (double-array nsteps)]
     (dotimes [i (count steps)]
         (aset-double steps i (*  (+ strtd i) spacing)))
     steps)))

(defn grid-specs
  "Returns a GridSpecs structure with enough stuff to draw a grid"
  [^toydb.viewdef.ViewDef view]
  (let [;; Feed the known pixel values to the inverse transform to get the points
        ;; for the upper left and bottom right corners of the canvas ,in units, not pixels
        invt (.inv-transform view)
        left_top_vec_px (matrix/matrix [0 0 1])
        right_bottom_vec_px (matrix/matrix [(- (.width view) 1) (- (.height view) 1) 1])
        [left top] (vec (matrix/mmul invt left_top_vec_px))
        [right bottom] (vec (matrix/mmul invt right_bottom_vec_px))

        ;; Update grids per unit
        zoom_exp_step (Math/floor (/ (.zoomlevel view) (.zoomratio view)))
        kgpu (.kgpu view)
        kmpm (.kmpm view)
        majgpu (* kgpu (Math/pow kmpm zoom_exp_step)) ; major grids per unit after zoom
        mingpu (* kgpu (Math/pow kmpm (+ zoom_exp_step 1)))
        majspacing (/ 1 majgpu)
        minspacing (/ 1 mingpu)
        majhsteps (compute-steps left right majspacing)
        minhsteps (compute-steps left right minspacing)
        majvsteps (compute-steps bottom top majspacing)
        minvsteps (compute-steps bottom top minspacing)]
    (->GridSpecs left right top bottom
                 majhsteps minhsteps
                 majvsteps minvsteps
                 (->GridSpacing majspacing minspacing))))



(defn collect-lines
  "Returns list of LineSpecs, each spec describing a line"
  [^GridSpecs gs]
  (let  [collect-pts
         (fn [^doubles steps, ^double startu, ^double stopu, dir]
           ;; steps is the X's and startu, stopu are the Y's or
           ;; vice-versa.  Returns pair of points with a vertical or
           ;; horizontal relation
           (case dir
             :vertical (for [xstep steps]
                         [(Point2D. xstep startu) (Point2D. xstep stopu)])
             :horizontal (for [ystep steps]
                           [(Point2D. startu ystep) (Point2D. stopu ystep)])))
         bot (.bottom gs)
         top (.top gs)
         left (.left gs)
         right (.right gs)]
    (array-map
     :minvertical    (collect-pts (.minhsteps gs) bot top :vertical)
     :minhorizontal  (collect-pts (.minvsteps gs) left right :horizontal)
     :majvertical    (collect-pts (.majhsteps gs) bot top :vertical)
     :majhorizontal  (collect-pts (.majvsteps gs) left right :horizontal)
     :axisvertical   (collect-pts [0.0] bot top :vertical)
     :axishorizontal (collect-pts [0.0] left right :horizontal))))


(defn draw-lines!
  "Put lines on the canvas using the current transform.
Lines is a list with each member a list of Point2D"
  [^GraphicsContext gc lines ^double linewidthpx]
  (.save gc)
  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        linewidthu (* linewidthpx recipscale)  ;; Divide requested linewidth down by Mxx
        pixel-offset (* 0.5 recipscale)] ;; Half-pixel offset
    (.setLineWidth gc linewidthu)
    (doseq [line lines]
      (doseq [ptpair line]
        (let [p1 ^Point2D (first ptpair)
              p2 ^Point2D (second ptpair)
              xp1 (+ pixel-offset (.getX p1))
              xp2 (+ pixel-offset (.getX p2))
              yp1 (+ pixel-offset (.getY p1))
              yp2 (+ pixel-offset (.getY p2))]
          (.strokeLine gc xp1 yp1 xp2 yp2)))))
  (.restore gc))


(defn draw-grid!
  "Draws grid onto canvas using provided view-data"
  [^Canvas canvas ^toydb.viewdef.ViewDef view-data]
  (let [gs (grid-specs view-data)
        lines (collect-lines gs)
        gc (.getGraphicsContext2D canvas)
        width (.getWidth canvas)
        height (.getHeight canvas)
        xfrm (.transform view-data)
        xvals (apply seq (matrix/reshape xfrm [1 9]))
        mxx (nth xvals 0)
        mxy (nth xvals 1)
        mxt (nth xvals 2)
        myx (nth xvals 3)
        myy (nth xvals 4)
        myt (nth xvals 5)]
    
    ;; Background
    (.setFill gc Color/WHITE)
    (.fillRect gc 0 0 width  height)

    (.save gc)
    (.setTransform gc mxx myx mxy myy mxt myt)
    (draw-lines! gc (select-values lines [:minvertical :minhorizontal]) 0.2)
    (draw-lines! gc (select-values lines [:majvertical :majhorizontal]) 0.5)
    (draw-lines! gc (select-values lines [:axisvertical :axishorizontal]) 2)
    (.restore gc)))

(defn resize-callback
  "Returns function which is called by .resize callback in reified Canvas"
  [view-data-atom]
)



#_(defn test-canvas []
  (let [[width height] [640 480]
        view-data-atom (atom (toydb.viewdef/viewdef {:width width, :height height}))
        rscallback (fn [canvas old-size]
                     (draw-grid! canvas @view-data-atom))
        grid-canvas (toydb.canvas/resizable-canvas rscallback)
        stage (jfxutils.core/stage grid-canvas [width height])]
    (add-watch view-data-atom "id" (fn [k r o n ]
                                     (draw-grid! grid-canvas @view-data-atom)))
    stage))




















