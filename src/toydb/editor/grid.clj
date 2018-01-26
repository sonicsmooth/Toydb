(ns ^{:doc "Low level grid functions" }
    toydb.editor.grid
    (:use [jfxutils.core :exclude [-main]])
    (:use [clojure.data :only [diff]])
    (:use [clojure.pprint])
    (:require toydb.editor.viewdef)
    (:require [clojure.core.matrix :as matrix]
              [clojure.core.matrix.operators :as matrixop])
    (:import [javafx.geometry Insets VPos Point2D]
             [javafx.scene.canvas Canvas GraphicsContext]
             [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
             [javafx.scene.text Font Text TextAlignment]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)
(matrix/set-current-implementation :vectorz)

;; These get are used if a settings map is not passed to draw-grid!
(def DEFAULT-GRID-SETTINGS
  {:major-grid-display true
   :major-line-color Color/BLACK
   :major-line-width-px 0.5

   :minor-grid-display true
   :minor-line-color Color/GRAY
   :minor-line-width-px 0.15

   :axis-display true
   :axis-line-color Color/DARKBLUE
   :axis-line-width-px 3

   :major-dots-display true
   :major-dots-color Color/RED
   :major-dots-size-px 5

   :minor-dots-display true
   :minor-dots-color Color/PINK
   :minor-dots-size-px 1.5})


;; Separate so panning memoization doesn't eat up memory for each pan
;; position
(defrecord GridSpacing [^double majspacing,
                        ^double minspacing])

;; Just the geometry, no colors here
(defrecord GridSpecs [^double left
                      ^double right
                      ^double top
                      ^double bottom
                      ^doubles majhsteps
                      ^doubles minhsteps
                      ^doubles majvsteps
                      ^doubles minvsteps
                      ^GridSpacing spacing])

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
  [^toydb.editor.viewdef.ViewDef view]
  (let [;; Feed the known pixel values to the inverse transform to get the points
        ;; for the upper left and bottom right corners of the canvas ,in units, not pixels
        invt (.inv-transform view)
        left_top_vec_px (matrix/matrix [0 0 1])
        right_bottom_vec_px (matrix/matrix [(- (.width view) 1) (- (.height view) 1) 1])
        [left top] (vec (matrix/mmul invt left_top_vec_px))
        [right bottom] (vec (matrix/mmul invt right_bottom_vec_px))

        ;; Update grids per unit
        majspacing (toydb.editor.viewdef/compute-maj-spacing (:zoomspecs view))
        minspacing (toydb.editor.viewdef/compute-min-spacing (:zoomspecs view))
        
        majhsteps (compute-steps left right majspacing)
        minhsteps (compute-steps left right minspacing)
        majvsteps (compute-steps bottom top majspacing)
        minvsteps (compute-steps bottom top minspacing)]
    (->GridSpecs left right top bottom
                 majhsteps minhsteps
                 majvsteps minvsteps
                 (->GridSpacing majspacing minspacing))))

(defn collect-lines
  "Returns list of line specs, each spec describing a line"
  [^GridSpecs gsp]
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
         bot (.bottom gsp)
         top (.top gsp)
         left (.left gsp)
         right (.right gsp)]
    (array-map
     :minvertical    (collect-pts (.minhsteps gsp) bot top :vertical)
     :minhorizontal  (collect-pts (.minvsteps gsp) left right :horizontal)
     :majvertical    (collect-pts (.majhsteps gsp) bot top :vertical)
     :majhorizontal  (collect-pts (.majvsteps gsp) left right :horizontal)
     :axisvertical   (collect-pts [0.0] bot top :vertical)
     :axishorizontal (collect-pts [0.0] left right :horizontal))))


(defn draw-lines!
  "Put lines on the canvas using the current transform.
Lines is a list with each member a list of Point2D"
  [^GraphicsContext gc lines ^double line-width-px ^Color color]
  (.save gc)
  (.setStroke gc color)
  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        line-width-u (* line-width-px recipscale)  ;; Divide requested linewidth down by Mxx
        pixel-offsetx (* 0.0 recipscale)
        pixel-offsety (* 0.5 recipscale)]
    (.setLineWidth gc line-width-u)
    (doseq [line lines]
      (doseq [ptpair line]
        (let [p1 ^Point2D (first ptpair)
              p2 ^Point2D (second ptpair)
              xp1 (+ pixel-offsetx (.getX p1))
              xp2 (+ pixel-offsetx (.getX p2))
              yp1 (+ pixel-offsety (.getY p1))
              yp2 (+ pixel-offsety (.getY p2))]
          (.strokeLine gc xp1 yp1 xp2 yp2)))))
  (.restore gc))

(defn draw-dots!
  ;;(count lines) must be exactly 2
  [^GraphicsContext gc lines ^double dot-width-px ^Color color]
  (.save gc)
  (.setFill gc color)
  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        dot-width-u (* dot-width-px recipscale)
        center-offset-u (/ dot-width-u 2.0)
        center-offset-u (- center-offset-u (* 0.5 recipscale))
        ;;pixel-offset 0.0 ;;(* 0.5 recipscale)
        vlines (first lines)
        hlines (second lines)]
    ;;(.setLineWidth gc dot-width-u)

    ;; These both work...
    #_(doseq [pt (for [vline vlines, hline hlines]
                 (Point2D. (.getX ^Point2D (first vline)) (.getY ^Point2D (first hline))))]
      (.strokeOval gc (.getX ^Point2D pt) (.getY ^Point2D pt) dot-width-u dot-width-u))
    
    ;; But this one is slightly more explicit
    (doseq [vline vlines] ;; get X from this
        (let [x (.getX ^Point2D (first vline))]
          (doseq [hline hlines] ;; get Y from this
            (let [y (.getY ^Point2D  (first hline))]
              (.fillOval gc
                         (- x center-offset-u)
                         (- y center-offset-u)
                         
                         dot-width-u
                         dot-width-u))))))

  (.restore gc))

(defn draw-grid!
  "Draws grid onto canvas using provided view-data and grid-settings,
  or DEFAULT-GRID-SETTINGS if not provided."
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view-data]
   (draw-grid! canvas view-data DEFAULT-GRID-SETTINGS))
  
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view-data grid-settings]
   (let [gst grid-settings
         lines (collect-lines (grid-specs view-data))
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
     
     ;; Draw a clear rectangle to expose the Pane background
     (.clearRect gc 0 0 width height)

     (.save gc)
     (.setTransform gc mxx myx mxy myy mxt myt)
     
     ;; Draw minor first if enabled, then major if enabled, then axes if enabled
     (when  (:major-grid-enable gst)
       (when (:minor-grid-enable gst)
         (when (:minor-lines-visible gst)
           (draw-lines! gc (select-values lines [:minvertical :minhorizontal])
                        (:minor-line-width-px gst)
                        (:minor-line-color gst)))

         (when (:minor-dots-visible gst)
           (draw-dots! gc
                       (select-values lines [:minvertical :minhorizontal])
                       (:minor-dot-width-px gst)
                       (:minor-dot-color gst))))

       (when (:major-lines-visible gst)
         (draw-lines! gc (select-values lines [:majvertical :majhorizontal])
                      (:major-line-width-px gst)
                      (:major-line-color gst)))

       (when (:major-dots-visible gst)
         (draw-dots! gc  (select-values lines [:majvertical :majhorizontal])
                     (:major-dot-width-px gst)
                     (:major-dot-color gst))))
     
     (when (:axes-visible gst)
       (draw-lines! gc (select-values lines [:axisvertical :axishorizontal])
                    (:axis-line-width-px gst)
                    (:axis-line-color gst)))
     
     (.restore gc))))









