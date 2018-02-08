(ns ^{:doc "Low level grid functions" }
    toydb.editor.grid
    (:use [jfxutils.core :exclude [-main]])
    (:use [clojure.data :only [diff]])
    (:use [clojure.pprint])
    (:require [toydb.editor.viewdef :as viewdef])
    (:require [clojure.core.matrix :as matrix]
              [clojure.core.matrix.operators :as matrixop])
    (:import [javafx.geometry Insets VPos Point2D]
             [javafx.scene.canvas Canvas GraphicsContext]
             [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]
             [javafx.scene.text Font Text TextAlignment]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* false; :warn-on-boxed
      )
(matrix/set-current-implementation :vectorz)

;; These get are used if a settings map is not passed to draw-grid!
(def DEFAULT-GRID-SETTINGS ;; this needs to be redone
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
   :minor-dots-size-px 1.5

   :dynamic-grid-enable true
   :minor-grid-ratio 5
   :zoom-ppmm 0.01})


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
        ;; for the upper left and bottom right corners of the canvas in units, not pixels
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
  (let [collect-pts
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
Lines is a list with each member a pair of Point2D."
  [^GraphicsContext gc, lines, ^double line-width-px, ^Color color]
  (.save gc)
  (.setStroke gc color)
  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        line-width-u (* line-width-px recipscale)  ;; Divide requested linewidth down by Mxx
        pixel-offsetx (* 0.0 recipscale)
        pixel-offsety (* 0.5 recipscale)]
    (.setLineWidth gc line-width-u)
    (doseq [line lines]
      (let [p1 ^Point2D (first line)
            p2 ^Point2D (second line)
            xp1 (+ pixel-offsetx (.getX p1))
            xp2 (+ pixel-offsetx (.getX p2))
            yp1 (+ pixel-offsety (.getY p1))
            yp2 (+ pixel-offsety (.getY p2))]
        (.strokeLine gc xp1 yp1 xp2 yp2))))
  (.restore gc))


(defn draw-dots!
  "Put dots on the canvas using the current transform.  lines is a
  2-vector with each element a list, with each member a
  pair of Point2D."
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

(defn draw-center-circle!
  [^GraphicsContext gc,
   ^double line-width-px,
   ^Color color]
  (.save gc)

  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        line-width-u (* line-width-px recipscale)
        circ-dia-u (* 10.0 recipscale)
        -circ-dia-u (- circ-dia-u)]
    (.setLineWidth gc line-width-u)
    (.setStroke gc color)
    (.strokeOval gc -circ-dia-u -circ-dia-u (* 2.0 circ-dia-u) (* 2.0 circ-dia-u)))
  (.restore gc))

(defn draw-scale!
  "Draws linear scale with text"
  [^GraphicsContext gc, ^toydb.editor.viewdef.ViewDef view, ^GridSpecs gsp]
  ;; This whole routine is done in pixel space, ie, with the default unit transform
  (let [xfrm (.getTransform gc)
        mxx (.getMxx xfrm)
        myy (.getMyy xfrm)
        majsp (get-in gsp [:spacing :majspacing])
        minsp (get-in gsp [:spacing :minspacing])
        height (:height view)
        pxos 0.5 
        line-width 1.25
        txt-line-width 1.0
        scale-cap-height 3.5
        common-scale-left-edge-offset 100
        major-scale-y-edge-offset 65
        minor-scale-y-edge-offset 40
        majlength (* majsp mxx)
        majx1 (+ pxos common-scale-left-edge-offset)
        majx2 (+ pxos (round-to-nearest (+ majx1 majlength) 1))
        majy  (+ pxos (- height major-scale-y-edge-offset))
        minlength (* minsp mxx)
        minx1 (+ pxos common-scale-left-edge-offset)
        minx2 (+ pxos (round-to-nearest (+ minx1 minlength) 1))
        miny  (+ pxos (- height minor-scale-y-edge-offset))
        moi (:metric-or-inches view)
        [unit-scale unit-label] (viewdef/get-print-scale-and-label view)
        majdis (* unit-scale majsp)
        mindis (* unit-scale minsp)
        majlabel (str (format "% .4g" majdis) unit-label)
        minlabel (str (format "% .4g" mindis) unit-label)]

    (.save gc)
    (.setTransform gc 1 0 0 1 0 0)
    (.setLineCap gc javafx.scene.shape.StrokeLineCap/BUTT)
    (.setLineWidth gc line-width)
    (.setStroke gc Color/GREEN)
    (.strokeLine gc majx1 majy majx2 majy)
    (.strokeLine gc majx1 (- majy scale-cap-height) majx1 (+ majy scale-cap-height))
    (.strokeLine gc majx2 (- majy scale-cap-height) majx2 (+ majy scale-cap-height))
    (.strokeLine gc minx1 miny minx2 miny)
    (.strokeLine gc minx1 (- miny scale-cap-height) minx1 (+ miny scale-cap-height))
    (.strokeLine gc minx2 (- miny scale-cap-height) minx2 (+ miny scale-cap-height))

    (.setFont gc (javafx.scene.text.Font. "Arial" 14))
    (.setTextBaseline gc javafx.geometry.VPos/CENTER)
    (.setTextAlign gc javafx.scene.text.TextAlignment/RIGHT)
    (.setLineWidth gc txt-line-width)
    (.strokeText gc majlabel (- majx1 5) majy )
    (.strokeText gc minlabel (- minx1 5) miny )





    (.restore gc)))

(defn draw-grid!
  "Draws grid onto canvas using provided view-data and grid-settings,
  or DEFAULT-GRID-SETTINGS if not provided."
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view-data]
   (draw-grid! canvas view-data DEFAULT-GRID-SETTINGS))
  
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view-data grid-settings]
   (let [gst grid-settings
         gsp (grid-specs view-data)
         lines (collect-lines gsp)
         gc (.getGraphicsContext2D canvas)
         width (.getWidth canvas)
         height (.getHeight canvas)
         xfrm (.transform view-data)
         xvals (apply seq (matrix/reshape xfrm [1 9]))
         mxx (double (nth xvals 0))
         mxy (double (nth xvals 1))
         mtx (double (nth xvals 2))
         myx (double (nth xvals 3))
         myy (double (nth xvals 4))
         mty (double (nth xvals 5))]
     
     ;; Draw a clear rectangle to expose the Pane background
     (.clearRect gc 0 0 width height)

     (.save gc)
     (.setTransform gc mxx myx mxy myy mtx mty)
     
     ;; Draw minor first if enabled, then major if enabled, then axes if enabled
     (let [ac (partial apply concat)
           selcat (comp ac select-values)]
       (when (:major-grid-enable gst)
         (when (:minor-grid-enable gst)
           (when (:minor-lines-visible gst)
             (draw-lines! gc (selcat lines [:minvertical :minhorizontal 3])
                          (:minor-line-width-px gst)
                          (:minor-line-color gst)))

           (when (:minor-dots-visible gst)
             (draw-dots! gc
                         (select-values lines [:minvertical :minhorizontal])
                         (:minor-dot-width-px gst)
                         (:minor-dot-color gst))))

         (when (:major-lines-visible gst)
           (draw-lines! gc (selcat lines [:majvertical :majhorizontal])
                        (:major-line-width-px gst)
                        (:major-line-color gst)))

         (when (:major-dots-visible gst)
           (draw-dots! gc (select-values lines [:majvertical :majhorizontal])
                       (:major-dot-width-px gst)
                       (:major-dot-color gst))))
       
       (when (:axes-visible gst)
         (draw-lines! gc (selcat lines [:axisvertical :axishorizontal])
                      (:axis-line-width-px gst)
                      (:axis-line-color gst)))

       (when (:origin-visible gst)
         (let [du (/ 10.0 mxx)
               -du (- du)]
           (condp = (:origin-marker gst)
             :crosshair
             (draw-lines! gc [[(Point2D. 0 -du) (Point2D. 0 du)]
                              [(Point2D. -du 0) (Point2D. du 0)]]
                          (:origin-line-width-px gst)
                          (:origin-line-color gst))
             
             :diag-crosshair
             (draw-lines! gc [[(Point2D. -du -du) (Point2D. du du)]
                              [(Point2D. -du du)  (Point2D. du -du)]]
                          (:origin-line-width-px gst)
                          (:origin-line-color gst))

             :circle
             (draw-center-circle! gc
                                  (:origin-line-width-px gst)
                                  (:origin-line-color gst)))))

       (when (:scale-visible gst)
         (draw-scale! gc view-data gsp)))
     
     (.restore gc))))


















