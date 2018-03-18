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


(set! *warn-on-reflection*  true  )
(set! *unchecked-math*  :warn-on-boxed)
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

(defmemo count-steps
  "Computes the locations, in unit space, of grid points.
  The points are located spacing units apart, and are on the integer
  spacing grid.  Start and stop are the full extent, so the grid
  points will be between start and stop.  For example, if start is
  -3.2 and stop is 4.5, with spacing 1, then the output is [-3 -2 -1 0
  1 2 3 4]"
  (^long [^double start, ^double stop, ^double spacing]
   (let [strtd (Math/ceil (/ start spacing))
         stpd (Math/floor (/ stop spacing))
         nsteps (long (+ (- stpd strtd) 1))]
     nsteps)))


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
         nsteps (count-steps start stop spacing)
         steps (double-array nsteps)]
     (dotimes [i nsteps]
         (aset-double steps i (*  (+ strtd i) spacing)))
     steps)))


(defn grid-specs
  "Returns a GridSpecs structure with enough stuff to draw a grid.  If
  the density of lines is too high, then returns an empty list."
  [^toydb.editor.viewdef.ViewDef view]
  (let [;; Feed the known pixel values to the inverse transform to get the points
        ;; for the upper left and bottom right corners of the canvas in units, not pixels
        ;;xfrm (:transform view)
        invt (:inv-transform view)
        imxx (double (matrix/mget invt 0 0))
        left_top_vec_px (matrix/matrix [0 0 1])
        right_bottom_vec_px (matrix/matrix
                             [(max 0 (dec (.width view)))
                              (max 0 (dec (.height view)))
                              1])
        [left top] (vec (matrix/mmul invt left_top_vec_px))
        [right bottom] (vec (matrix/mmul invt right_bottom_vec_px))
        
        ;; Update grids per unit
        ^double majspacing (toydb.editor.viewdef/compute-maj-spacing view)
        ^double minspacing (toydb.editor.viewdef/compute-min-spacing view)

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


(defn draw-pt-lines!
  "Put lines on the canvas using the current transform.
Lines is a list with each member a pair of Point2D."
  [^GraphicsContext gc, lines, ^double line-width-px, ^Color color]
  (.save gc)
  (.setStroke gc color)
  (let [recipscale (/ 1.0 (.. gc getTransform getMxx))
        line-width-u (* line-width-px recipscale)  ;; Divide requested linewidth down by Mxx
        pixel-offsetx (* -0.5 recipscale)
        pixel-offsety (* -0.5 recipscale)]
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

(defn draw-matrix-lines!
  "Put lines on the canvas using the current transform.
  Lines is Nx4 matrix with each row the x1,y1,x2,y2 coordinates."
  [^GraphicsContext gc
   ^mikera.matrixx.impl.DenseColumnMatrix lines
   ^double line-width-px
   ^Color color]
  (.save gc)
  (.setStroke gc color)
  (let [offset 0.5
        recipscale (/ 1.0 (.. gc getTransform getMxx))
        line-width-u (* line-width-px recipscale)]   ;; Divide requested linewidth down by Mxx
    (.setLineWidth gc line-width-u)
    (doseq [line lines]
      (let [x1 (+ offset (double (matrix/mget line 0)))
            y1 (+ offset (double (matrix/mget line 1)))
            x2 (+ offset (double (matrix/mget line 2)))
            y2 (+ offset (double (matrix/mget line 3)))]
        (.strokeLine gc x1 y1 x2 y2))))
  (.restore gc))


(defn draw-pt-dots!
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

(defn xyvecs-to-matrix
  "Takes two-vector of point-pairs (M and N long), and returns (M*N)x2
  matrix of point coordinates.  Returns empty matrix if M*N is too
  big."
  [[V H]]
  ;; V represents vertical lines, ie X coords
  ;; [[P2D(10,-50) P2D(10,50)]     N pairs
  ;;  [P2D(20,-50) P2D(20,50)]]
  
  ;; H represents horizontal lines, ie Y coords
  ;; [[P2D(-50, 20) P2D(50, 20)]   M pairs
  ;;  [P2D(-50, 30) P2D(50, 30)]]
  ;; Could not optimize with aset or aset-double -- much slower
  (let [M (count H) ;; how many down
        N (count V)  ;; how many across
        M*N (* M N)
        dotlimit 20000]
    (if (< M*N dotlimit)
      (let [m (matrix/zero-matrix (* M N) 2)]
        (doseq [[^long col [^Point2D xpt _]] (map-indexed vector V)
                [^long row [^Point2D ypt _]] (map-indexed vector H)]
          (let [idx (+ col (* row N))]
            (matrix/mset! m idx 0 (.getX xpt))
            (matrix/mset! m idx 1 (.getY ypt))))
        m)
      (matrix/matrix []))))

(defn xyvecs-to-pixel-matrix
  [VH scale]
  (-> (xyvecs-to-matrix VH)
      (matrix/mul! scale)
      (matrix/round!)
      (matrix/add! 0.5)))

(defn draw-matrix-dots!
  "Put dots on the canvas using the current transform.  dots is a Nx2
  matrix with each row a dot, colums are x and y."
  [^GraphicsContext gc
   ^mikera.matrixx.impl.DenseColumnMatrix dots
   ^double dot-width-px
   ^Color color]
  (.save gc)
  (.setFill gc color)
  (let [dot-width-u (/ dot-width-px (.. gc getTransform getMxx))
        center-offset-u (/ dot-width-u 2.0)
        newdots (matrix/sub dots center-offset-u)]
       (doseq [dot newdots]
         (let [x (first dot)
               y (second dot)]
          (.fillOval gc x y dot-width-u dot-width-u))))
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
  (let [xfrm (:transform view)
        mxx (double (matrix/mget xfrm 0 0)) ;; (.getMxx xfrm)
        ;;myy (.getMyy xfrm)
        majsp (double (get-in gsp [:spacing :majspacing]))
        minsp (double (get-in gsp [:spacing :minspacing]))
        height (.height view)
        pxos 0.5 
        line-width 1.0
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
        [^double unit-scale unit-label] (viewdef/get-print-scale-and-label view)
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


(defn line-endpts-to-matrix
  "Takes N-list of pairs of Point2Ds, and returns Nx4 matrix, where
  each row is x1,y1,x2,y2 coordinate."
  [line-pts]
  (let [xpts-to-vec (fn [pts] (vec (for [^Point2D p pts] (.getX p))))
        ypts-to-vec (fn [pts] (vec (for [^Point2D p pts] (.getY p))))
        x1 (xpts-to-vec (map first line-pts))
        y1 (ypts-to-vec (map first line-pts))
        x2 (xpts-to-vec (map second line-pts))
        y2 (ypts-to-vec (map second line-pts))]
    (matrix/transpose (matrix/matrix [x1 y1 x2 y2]))))

(defn line-endpts-to-pixel-matrix
  "Takes N-list of pairs of Point2Ds, and returns Nx4 matrix, where
  each row is x1,y1,x2,y2 coordinate, scaled by mxx, rounded and
  offset by 0.5."
  [line-pts scale]
  (-> line-pts line-endpts-to-matrix
      (matrix/mul! scale)
      (matrix/round!)
      (matrix/add! 0.5)))

(defn draw-gridlines!
  "Draws minor lines directly on GC using viewdef.  Does not collect
  points or lines ahead of time, or use matrix.  Draws in pixel space
  with default transform"
  [^GraphicsContext gc, ^toydb.editor.viewdef.ViewDef view, grid-settings, whichgrid]
  (let [xfrm (:transform view)
        mxx (double (matrix/mget xfrm 0 0))
        myy (double (matrix/mget xfrm 1 1))
        mxt (double (matrix/mget xfrm 0 2))
        myt (double (matrix/mget xfrm 1 2))
        stopx (dec (.width view))
        stopy (dec (.height view))
        spacing (double (condp = whichgrid
                          :minor (viewdef/compute-min-spacing view)
                          :major (viewdef/compute-maj-spacing view)))
        linewidth (double (condp = whichgrid
                            :minor (:minor-grid/line-width-px grid-settings)
                            :major (:major-grid/line-width-px grid-settings)))
        color (condp = whichgrid
                :minor (:minor-grid/line-color grid-settings)
                :major (:major-grid/line-color grid-settings))
        step (* mxx (double spacing))]

    (.save gc)
    (.setTransform gc (javafx.scene.transform.Affine.))
    (.setLineWidth gc linewidth)
    (.setStroke gc color)
    
    ;; Draw vertical lines
    (loop [x (double (mod mxt step))]
      (when (<= x stopx)
        (let [rx (+ 0.5 (round-to-nearest x 1.0))]
          (.strokeLine gc rx 0.0 rx stopy)
          (recur (+ x step)))))

    ;; Draw horizontal lines
    (loop [y (double (mod myt step))]
      (when (<= y stopy)
        (let [ry (+ 0.5 (round-to-nearest y 1.0))]
          (.strokeLine gc 0.0 ry stopx ry)
          (recur (+ y step))))))
  (.restore gc))

(defn draw-griddots!
  "Draws minor dots directly on GC using viewdef.  Does not collect
  points or lines ahead of time, or use matrix.  Draws in pixel space
  with default transform."
  [^GraphicsContext gc,  ^toydb.editor.viewdef.ViewDef view, grid-settings, whichgrid]
  (let [xfrm (:transform view)
        mxx (double (matrix/mget xfrm 0 0))
        spacing (double (condp = whichgrid
                          :minor (viewdef/compute-min-spacing view)
                          :major (viewdef/compute-maj-spacing view)))
        spacingpx (* mxx spacing)]
    (when (> spacingpx 4) ;; Skip if spacing <= 4 px
      (let [myy (double (matrix/mget xfrm 1 1))
            mxt (double (matrix/mget xfrm 0 2))
            myt (double (matrix/mget xfrm 1 2))
            rightpx (dec (.width view))
            botpx (dec (.height view))

            dotwidth (double (condp = whichgrid
                               :minor (:minor-grid/dot-width-px grid-settings)
                               :major (:major-grid/dot-width-px grid-settings)))
            color (condp = whichgrid
                    :minor (:minor-grid/dot-color grid-settings)
                    :major (:major-grid/dot-color grid-settings))

            startx (double (mod mxt spacingpx))
            starty (double (mod myt spacingpx))
            offset (/ (double dotwidth) -2.0)

            ;; Draw dots horizontally at y
            horiz-dots (fn [y]
                         (loop [x startx]
                           (when (<= x rightpx)
                             (let [rx (+ offset 0.5 (round-to-nearest x 1.0))]
                               (.fillOval gc rx y dotwidth dotwidth)
                               (recur (+ x spacingpx))))))]

        (.save gc)
        (.setTransform gc (javafx.scene.transform.Affine.))
        (.setLineWidth gc 0)
        (.setFill gc color)

        ;; Draw bunch of horizontal dots
        (loop [y starty]
          (when (<= y botpx)
            (let [ry (+ offset 0.5 (round-to-nearest y 1.0))]
              (horiz-dots ry)
              (recur (+ y spacingpx)))))
        (.restore gc)))))

(defn draw-axes!
  "Draws minor dots directly on GC using viewdef.  Does not collect
  points or lines ahead of time, or use matrix.  Draws in pixel space
  with default transform."
  [^GraphicsContext gc,  ^toydb.editor.viewdef.ViewDef view, grid-settings]
  (let [xfrm (:transform view)
        mxt (double (matrix/mget xfrm 0 2))
        myt (double (matrix/mget xfrm 1 2))
        x (+ 0.5 (round-to-nearest mxt 1.0))
        y (+ 0.5 (round-to-nearest myt 1.0))]
    (.save gc)
    (.setTransform gc (javafx.scene.transform.Affine.))
    (.setLineWidth gc (:axes/line-width-px grid-settings))
    (.setStroke gc (:axes/line-color grid-settings))
    (.strokeLine gc 0 y (.width view) y)
    (.strokeLine gc x 0 x (.height view))
    (.restore gc)))

(defn draw-grid!
  "Draws grid onto canvas using provided view-data and grid-settings,
  or DEFAULT-GRID-SETTINGS if not provided."
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view-data]
   (draw-grid! canvas view-data DEFAULT-GRID-SETTINGS))
  
  ([^Canvas canvas ^toydb.editor.viewdef.ViewDef view grid-settings]
   (let [gst grid-settings
         gsp (grid-specs view)
         linesmap (collect-lines gsp)
         gc (.getGraphicsContext2D canvas)
         xfrm (:transform view)
         mxx (double (matrix/mget xfrm 0 0))
         mxt (double (matrix/mget xfrm 0 2))
         myt (double (matrix/mget xfrm 1 2))]

     ;; Draw a clear rectangle to expose the Pane background
     (.clearRect gc 0 0 (.getWidth canvas) (.getHeight canvas))

     (.save gc)
     ;;(.setTransform gc mxx myx mxy myy (Math/round mxt) (Math/round myt))
     (.setTransform gc 1 0 0 1 (Math/round mxt) (Math/round myt))

     ;; Draw minor first if enabled, then major if enabled, then axes if enabled
     (let [selcat (fn [map keys] (apply concat (select-values map keys)))]
       (when (:major-grid/enable gst)
         (when (:minor-grid/enable gst)
           (when (:minor-grid/lines-visible gst)
             (draw-gridlines! gc view gst :minor))

           (when (:minor-grid/dots-visible gst)
             (draw-griddots! gc view gst :minor)))

         (when (:major-grid/lines-visible gst)
           (draw-gridlines! gc view gst :major))

         (when (:major-grid/dots-visible gst)
           (draw-griddots! gc view gst :major)))
       
       (when (:axes/visible gst)
         (draw-axes! gc view gst))

       (when (:origin/visible gst)
         (let [du (double (:origin/size-px gst))
               -du (- du)
               width (double (:origin/line-width-px gst))
               color (:origin/line-color gst)]
           (condp = (:origin/marker gst)
             :diag-crosshair
             (draw-matrix-lines! gc (matrix/matrix [[-du -du du du]
                                                    [-du du du -du]])
                                 width color)

             :crosshair
             (draw-matrix-lines! gc (matrix/matrix [[0 -du 0 du]
                                                    [-du 0 du 0]])
                                 width color)

             :circle
             (draw-center-circle! gc width color))))

       
       (when (:zoom/scale-visible gst)
         (draw-scale! gc view gsp)))
     (.restore gc))))


















