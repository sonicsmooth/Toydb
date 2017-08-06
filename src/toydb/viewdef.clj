(ns toydb.viewdef
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.matrix.operators :as matrixop])
  (:import [javafx.geometry Point2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(matrix/set-current-implementation :vectorz)


(defrecord ViewDef [^long width
                    ^long height
                    ^mikera.matrixx.Matrix transform
                    ^mikera.matrixx.Matrix inv-transform
                    ^Point2D origin ;; in canvas space pixels, relative to top left corner
                    ^long zoomratio
                    ^long zoomlevel
                    zoomlimits ;; what no typhint?
                    ^double kppu
                    ^double kgpu
                    ^long kmpm])

(defn compute-ppu
  "Compute pixels per unit given parameters"
  (^double [^long zoomlevel, ^long zoomratio, ^double kppu, ^long kmpm]
   (let [zoom-exp (/ zoomlevel zoomratio)]
     (* kppu (Math/pow kmpm zoom-exp))))
  (^double [^ViewDef view]
   (compute-ppu (.zoomlevel view) (.zoomratio view) (.kppu view) (.kmpm view))))

(defn transform
  "Returns transform matrix which converts units to pixels.  This
  fuction is intended to be used when creating or 'modifying' an
  existing viewdef"
  ([^Point2D origin, ^Long zoomlevel, ^Long zoomratio, ^Double kppu, ^Long kmpm]
   (let [ppu (double (compute-ppu zoomlevel zoomratio kppu kmpm))]
     (matrix/matrix [[ppu 0       (.getX origin)]
                     [0   (- ppu) (.getY origin)]
                     [0   0    1            ]]))))

(defn viewdef
  ([] (viewdef {}))
  ;; defaults are defined here
  ([{:keys [^long width, ^long height, ^long zoomratio, zoomlimits, ^long kmpm, ^double kppu, ^double kgpu]
     :or {width 0, height 0, zoomratio 100, zoomlimits [-400 400], kmpm 5, kppu 100.0, kgpu 1.0}}]
   (let [origin (Point2D. (/ width 2) (/ height 2))
         trans (transform origin 0 zoomratio kppu kmpm)
         inv-trans (matrix/inverse trans)]
     (map->ViewDef {:width width,
                    :height height,
                    :transform trans,
                    :inv-transform inv-trans
                    :origin origin
                    :zoomratio zoomratio
                    :zoomlevel 0
                    :zoomlimits zoomlimits
                    :kppu kppu,
                    :kgpu kgpu,
                    :kmpm kmpm}))))

(defn compute-ppu-ratio
  "Computes the ratio of PPU between the new zoom level and the zoom
  level in the old view"
  (^double [^ViewDef old-view, ^long new-zoomlevel]
   (let [oldkmpm (.kmpm old-view)
         oldzl (.zoomlevel old-view)
         oldzr (.zoomratio old-view)]
     (Math/pow oldkmpm (/ (- new-zoomlevel oldzl) oldzr)))))


(defn pan-to
  "Return new Viewdef based on pan coordinates, including updated transform"
  (^ViewDef [^ViewDef view, ^Point2D new-origin-pt]
   (let [new-transform (transform new-origin-pt
                                  (:zoomlevel view)
                                  (:zoomratio view)
                                  (:kppu view)
                                  (:kmpm view))
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
  sized 0,0"
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
  first change the origin by an amount proportional to a. the
  difference between the origin and the selected point, and b. the
  ratio of pixels-per-unit."
  (^ViewDef [^ViewDef view, ^long newz]
   (let [clippedz (clip (:zoomlimits view) newz)]
     (if (not= clippedz newz)
       view ;; already at clipped limit, so just return
       (let [new-transform (transform (:origin view)
                                      newz
                                      (:zoomratio view)
                                      (:kppu view)
                                      (:kmpm view))
             new-inv-transform (matrix/inverse new-transform)]
         (assoc view
                :zoomlevel newz
                :transform new-transform
                :inv-transform new-inv-transform)))))

  (^ViewDef [^ViewDef view, ^long newz, ^Point2D ptpx]
   (let [clippedz (clip (:zoomlimits view) newz)
         origin ^Point2D (:origin view)
         diffdistpx (.multiply (.subtract origin ptpx) (- (compute-ppu-ratio view clippedz) 1))
         newview (assoc view :origin (.add origin diffdistpx))]
     (zoom-to newview clippedz))))

(defn zoom-by
  "Zooms by a delta amount.  Just delgeates to the zoom-to function"
  (^ViewDef [^ViewDef view, ^long dz]
   (zoom-to view (+ dz (long (:zoomlevel view)))))

  (^ViewDef [^ViewDef view , ^long dz, ^Point2D ptpx]
   (zoom-to view (+ dz (long (:zoomlevel view))) ptpx)))


(defn pixels-to-units 
  "Returns a Point2D in unit space, given a pointin pixel space"
  (^Point2D [^ViewDef view ^Point2D ptpx]
   (pixels-to-units view (.getX ptpx) (.getY ptpx)))

  (^Point2D [^ViewDef view, ^double x, ^double y]
   (let [m (matrix/matrix [x y 1])
         mupos (matrix/mmul (:inv-transform view) m)]
     (Point2D. (matrix/mget mupos 0) (matrix/mget mupos 1)))))

(defn reset-viewdef
  "Returns new viewdef with zero zoom, centered origin.  Width and
  height are self-explanatory.  If a ViewDef is provided, its width,
  height, origin, and recomputed transform will be replaced.  If not
  provided, a default ViewDef is created with width and height as
  given, and centered origin"
  (^ViewDef [^double width, ^double height]
   (reset-viewdef (viewdef) width height))
  (^ViewDef [^ViewDef view, ^double width, ^double height]
   (let [zoomratio (:zoomratio view)
         zoomlevel 0
         origin (Point2D. (/ width 2) (/ height 2))
         kppu (:kppu view)
         kgpu (:kgpu view)
         kmpm (:kmpm view)
         trans (transform origin zoomlevel zoomratio kppu kmpm)
         inv-trans (matrix/inverse trans)]
     (assoc view
            :width width
            :height height
            :transform trans
            :inv-transform inv-trans
            :origin origin
            :zoomlevel zoomlevel))))
















