(ns toydb.viewdef
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as matrixop])
  (:import [javafx.geometry Point2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(matrix/set-current-implementation :vectorz)

;; Separate zoom from pan (aka origin) and the rest of it
;; This allows memoization of spacing which is used for all the moving/dragging/snapping
(defrecord ZoomSpecs [^long zoomratio
                      ^long zoomlevel
                      zoomlimits ;; what no typehint?
                      ^double kppu
                      ^double kgpu
                      ^long kmpm])

(defrecord ViewDef [^long width
                    ^long height
                    ^Point2D origin ;; in canvas space pixels, relative to top left corner
                    ^mikera.matrixx.Matrix transform
                    ^mikera.matrixx.Matrix inv-transform
                    ^ZoomSpecs zoomspecs])

(defn compute-ppu
  "Compute pixels per unit given parameters"

  ([^long zoomlevel, ^long zoomratio, ^double kppu, ^long kmpm]
   (let [zoom-exp (/ zoomlevel zoomratio)]
     (* kppu (Math/pow kmpm zoom-exp))))

  ([^ViewDef view]
   (let [zs (:zoomspecs view)]
     (compute-ppu (:zoomlevel zs) (:zoomratio zs) (:kppu zs) (:kmpm zs)))))


(defn transform
  "Returns transform matrix which converts units to pixels.  This
  fuction is intended to be used when creating or 'modifying' an
  existing viewdef"

  ([^Point2D origin, ^Long zoomlevel, ^Long zoomratio, ^Double kppu, ^Long kmpm]
   (let [ppu (double (compute-ppu zoomlevel zoomratio kppu kmpm))]
     (matrix/matrix [[ppu 0       (.getX origin)]
                     [0   (- ppu) (.getY origin)]
                     [0   0    1            ]])))

  ;; This one takes a ZoomSpecs map
  ([^Point2D origin, {:keys [^long zoomlevel ^long zoomratio ^long kppu ^long kmpm]}]
   (transform origin zoomlevel zoomratio kppu kmpm)))


;; Constructor for ViewDef
(defn viewdef
  ([] (viewdef {})) ;; call real constructor
  
  ;; defaults are defined here
  ([{:keys [^long width
            ^long height
            ^ZoomSpecs zoomspecs]
     :or {width 0
          height 0
          zoomspecs (map->ZoomSpecs {:zoomratio 100
                                     :zoomlevel 0
                                     :zoomlimits [-400 400]
                                     :kmpm 10
                                     :kppu 100.0
                                     :kgpu 1.0})}}]
   (let [origin (Point2D. (/ width 2) (/ height 2))
         trans (transform origin 0
                          (:zoomratio zoomspecs)
                          (:kppu zoomspecs)
                          (:kmpm zoomspecs))
         inv-trans (matrix/inverse trans)]
     (map->ViewDef {:width width
                    :height height
                    :origin origin
                    :transform trans
                    :inv-transform inv-trans
                    :zoomspecs zoomspecs}))))

(defn compute-ppu-ratio
  "Computes the ratio of PPU between the new zoom level and the zoom
  level in the old zoomspecs"
  (^double [^ZoomSpecs old-zoomspecs, ^long new-zoomlevel]
   (let [oldkmpm (.kmpm old-zoomspecs)
         oldzl (.zoomlevel old-zoomspecs) ;; dot notation allows us to avoid casting to Long
         oldzr (.zoomratio old-zoomspecs)]
     (Math/pow oldkmpm (/ (- new-zoomlevel oldzl) oldzr)))))


(defn _compute-maj-spacing
"Computes major grid spacing for both grid and snap-to functionality"
  [^toydb.viewdef.ZoomSpecs zoomspecs]
  (let [zoom_exp_step (Math/floor (/ (.zoomlevel zoomspecs) (.zoomratio zoomspecs)))
        kgpu (.kgpu zoomspecs)
        kmpm (.kmpm zoomspecs)
        majgpu (* kgpu (Math/pow kmpm zoom_exp_step))]  ; major grids per unit after zoom
    (/ 1 majgpu)))
(def compute-maj-spacing (memoize _compute-maj-spacing))

;; This should be optimized further so a mere pan doesn't cause a new
;; compute-min-spacing
(defn _compute-min-spacing
  "Computes minor gridspacing for both grid and snap-to functionality"
  (^double [^toydb.viewdef.ZoomSpecs zoomspecs]
   (let [zoom_exp_step (Math/floor (/ (.zoomlevel zoomspecs) (.zoomratio zoomspecs)))
         kgpu (.kgpu zoomspecs)
         kmpm (.kmpm zoomspecs)
         mingpu (* kgpu (Math/pow kmpm (+ zoom_exp_step 1)))] ;; minor grids per unit after zoom
     (/ 1 mingpu))))
(def compute-min-spacing (memoize _compute-min-spacing))


(defn pan-to
  "Return new Viewdef based on pan coordinates, including updated transform"

  (^ViewDef [^ViewDef view, ^Point2D new-origin-pt]
   (let [new-transform (transform new-origin-pt (:zoomspecs view))
         new-inv-transform (matrix/inverse new-transform)]
     (assoc view
            :origin new-origin-pt
            :transform new-transform
            :inv-transform new-inv-transform)))

  (^ViewDef [^ViewDef view, ^double x, ^double y]
   (pan-to view (Point2D. x y))))


(defn pan-by
  "Return new Viewdef based on delta pan coordinates, including updated transform"

  (^ViewDef [^ViewDef view, ^Point2D newdpt]
   (pan-to view (.add newdpt (:origin view))))

  (^ViewDef [^ViewDef view, ^double dx, ^double dy]
   (pan-by view (Point2D. dx dy))))


(defn resize
  "Just changes width and height so grid calc can proceed.  Origin Y
  value tracks window height.  Special case for new windows initially
  sized 0,0."
  (^ViewDef [^ViewDef view, [^double oldw, ^double oldh], [^double neww, ^double newh], mouse-state]
   (let [view (assoc view :width neww, :height newh)]
     (if (nil? mouse-state)
       (pan-to view (/ neww 2) (/ newh 2))
       (pan-by view 0 (- newh oldh))))))

(defn clip
  "Clips z such that lower <= z <= upper"
  [[^long lower ^long upper] ^long z]
  (cond
    (< z lower) lower
    (> z upper) upper
    :else z))


(defn zoom-to
  "Changes zoom to new zoom level.  Accepts pixel location for
  zoom-on-point, otherwise assumes zoom-on-origin.  For zoom-on-point,
  first change the origin by an amount proportional to a) the
  difference between the origin and the selected point, and b) the
  ratio of pixels-per-unit."

  ;; Zoom-on-origin
  (^ViewDef [^ViewDef view, ^long newz]
   (let [clippedz (-> view :zoomspecs :zoomlimits (clip newz))]
     (if (not= clippedz newz)
       view ;; already at clipped limit, so just return
       (let [new-zoomspecs (assoc (:zoomspecs view) :zoomlevel newz)
             new-transform (transform (:origin view) new-zoomspecs)
             new-inv-transform (matrix/inverse new-transform)]
         (assoc view
                :transform new-transform
                :inv-transform new-inv-transform
                :zoomspecs new-zoomspecs)))))

  ;; Zoom-on-point
  (^ViewDef [^ViewDef view, ^long newz, ^Point2D ptpx]
   (let [clippedz (-> view :zoomspecs :zoomlimits (clip newz));;   (clip (:zoomlimits (:zoomspecs view)) newz)
         origin ^Point2D (:origin view)
         diffdistpx (.multiply (.subtract origin ptpx)
                               (- (compute-ppu-ratio (:zoomspecs view) clippedz) 1))
         newview (assoc view :origin (.add origin diffdistpx))]
     (zoom-to newview clippedz))))


(defn zoom-by
  "Zooms by a delta amount.  Just delgeates to the zoom-to function"

  (^ViewDef [^ViewDef view, ^long dz]
   (zoom-to view (+ dz (long (-> view :zoomspecs :zoomlevel) #_(:zoomlevel view)))))

  (^ViewDef [^ViewDef view , ^long dz, ^Point2D ptpx]
   (zoom-to view (+ dz (long (-> view :zoomspecs :zoomlevel) #_(:zoomlevel view))) ptpx)))


(defn units-to-snapped-units
  "Returns a point in unit space, snapped to the nearest minor grid,
  given a point in unit space.  If arg is a Point2D, return is a
  Point2D.  If arg is a vector of two doubles, return is a vector of
  two doubles."

  (^Point2D [^ViewDef view, ^Point2D ptu]
   (let [[x y] [(.getX ptu) (.getY ptu)]
         [snx sny] (units-to-snapped-units view x y)]
     (Point2D. snx sny)))

  ([^ViewDef view, ^double ux, ^double uy]
   (let [spacing (double (compute-min-spacing (:zoomspecs view)))
         quantx (Math/round (/ ux spacing))
         quanty (Math/round (/ uy spacing))
         snapx (* spacing quantx)
         snapy (* spacing quanty)]
     [snapx snapy])))


(defn pixels-to-units 
  "Returns a Point2D in unit space, given a point in pixel space"

  (^Point2D [^ViewDef view, ^Point2D ptpx]
   (pixels-to-units view (.getX ptpx) (.getY ptpx)))

  (^Point2D [^ViewDef view, ^double x, ^double y]
   (let [m (matrix/matrix [x y 1])
         mupos (matrix/mmul (:inv-transform view) m)]
     (Point2D. (matrix/mget mupos 0) (matrix/mget mupos 1)))))


(defn pixels-to-snapped-units
  "Returns a Point2D in unit space, snapped to the nearest minor grid,
  given a point in pixel space"

  (^Point2D [^ViewDef view, ^Point2D ptpx]
   (pixels-to-snapped-units view (.getX ptpx) (.getY ptpx)))

  (^Point2D [^ViewDef view, ^double x, ^double y]
   (let [m (matrix/matrix [x y 1])
         mupos (matrix/mmul (:inv-transform view) m)
         ux (double (matrix/mget mupos 0))
         uy (double (matrix/mget mupos 1))
         [snapx snapy] (units-to-snapped-units view ux uy)]
     (Point2D. snapx snapy))))



(defn reset-viewdef
  "Returns new viewdef with zero zoom, centered origin.  Width and
  height are self-explanatory.  If a ViewDef is provided, its width,
  height, origin, and recomputed transform will be replaced.  If not
  provided, a default ViewDef is created with width and height as
  given, and centered origin"

  (^ViewDef [^double width, ^double height]
   (reset-viewdef (viewdef) width height))

  (^ViewDef [^ViewDef view, ^double width, ^double height]
   (let [origin (Point2D. (/ width 2) (/ height 2))
         zoomspecs (assoc (:zoomspecs view) :zoomlevel 0)
         trans (transform origin zoomspecs)
         inv-trans (matrix/inverse trans)]
     (assoc view
            :width width
            :height height
            :origin origin
            :transform trans
            :inv-transform inv-trans
            :zoomspecs zoomspecs))))
















