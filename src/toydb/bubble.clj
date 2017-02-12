(ns toydb.bubble
  (:use [jfxutils.core :exclude [-main]])
  (:import [javafx.geometry Insets]
           [javafx.stage Stage]
           [javafx.scene.canvas Canvas]
           [javafx.scene CacheHint Group Scene ]
           [javafx.scene.shape Rectangle Circle Ellipse]
           [javafx.scene.layout StackPane VBox Background BackgroundFill CornerRadii]
           [javafx.scene.paint Color LinearGradient RadialGradient CycleMethod Stop]))

;; From http://www.canoo.com/blog/2012/09/21/take-care-of-the-javafx-scene-graph/

(defn bubble
  "Returns a bubble, which is a Group node of given size."
  [size]
  (let [width size
        height size
        bubble (jfxnew Group
                       :children [(jfxnew Rectangle ;; bounding box for group size
                                          0 0 width height :opacity 0.0)
                                  (jfxnew Circle ;; Main
                                          (* 0.5 width) (* 0.5 height) (* 0.48 width)
                                          :stroke nil
                                          :fill (jfxnew LinearGradient
                                                        (* 0.5 width) (* 0.02 height)
                                                        (* 0.5 width) (* 0.98 height)
                                                        false CycleMethod/NO_CYCLE
                                                        [(Stop. 0.0   (Color/color 1 1 1 0.0470588235))
                                                         (Stop. 0.85  (Color/color 1 1 1 0.0470588235))
                                                         (Stop. 0.851 (Color/color 1 1 1 0.0745098039))
                                                         (Stop. 1.0   (Color/color 1 1 1 0.8980392157))]))
                                  (jfxnew Circle ;; Frame
                                          (* 0.5 width) (* 0.5 height) (* 0.48 width)
                                          :stroke nil
                                          :fill (jfxnew RadialGradient
                                                        0.0 0.0
                                                        (* 0.5 width) (* 0.5 height) (* 0.48 width)
                                                        false CycleMethod/NO_CYCLE
                                                        [(Stop. 0.0  (Color/color 1 1 1 0))
                                                         (Stop. 0.92 (Color/color 1 1 1 0.4549019608))
                                                         (Stop. 1.0  (Color/color 1 1 1 0.4980392157))]))
                                  (jfxnew Ellipse ;; Highlight
                                          (* 0.5 width) (* 0.27 height)
                                          (* 0.36 width) (* 0.23 height)
                                          :stroke nil
                                          :fill (jfxnew LinearGradient
                                                        (* 0.5 width) (* 0.04 height)
                                                        (* 0.5 width) (* 0.5 height)
                                                        false CycleMethod/NO_CYCLE
                                                        [(Stop. 0.0 (Color/color 1 1 1 0.6980392157))
                                                         (Stop. 1.0 (Color/color 1 1 1 0))]))])]
    (.setCache bubble true)
    (.setCacheHint bubble CacheHint/SCALE)
    bubble))

(defn bubble-window [width height qty]
  (let [halfwidth (/ 800 2)
        halfheight (/ 800 2)
        bubbles (for [_ (range qty)]
                  (make-draggable! (bubble (+ 10 (rand-int 140)))))
        scene (Scene. (jfxnew StackPane
                              :children bubbles
                              :background (jfxnew Background
                                                  [(BackgroundFill.
                                                    Color/BLACK
                                                    CornerRadii/EMPTY
                                                    Insets/EMPTY)])))
        window (jfxnew Stage 
                       :scene scene 
                       :width width
                       :height height)]

    (doseq [bubble bubbles]
      (.setTranslateX bubble (- (rand-int width) halfwidth))
      (.setTranslateY bubble (- (rand-int height) halfheight)))
    window))



        
