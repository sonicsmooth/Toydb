;;; This file shows some examples of in-memory database-like structures

;;; What are the primitives?  Assume no nurb
;;; line             IsA: polyline, open-shape, path, [points]
;;; regular-polygon  IsA: polygon, closed-shape, path, [#sides, radius]
;;; star-polygon     IsA: polygon, closed-shape, path [num-sides, radius1, radius2]
;;; ellipse          IsA: ellipse, closed-shape, path [focus1, focus2, R]
;;; ellipse-arc      IsA: ellipse, open-shape, path [focus1, focus2, R, start, stop]
;;; open-curve       IsA: open-curve, open-shape, path [bezier? cubic? quadratic? nurb?]
;;; closed-curve     IsA: closed-curve, closed-shape, path [bezier? cubic? quadratic? nurb?]
;;; text             IsA: text, ["whatever", font]
;;; net              IsA: net (no graphics), network-element, [name, [connect-points]]
;;; node             IsA: node (no graphics), network-element [net]

;;; open shapes have options for endpoints, such as arrowheads, dots, etc.
;;; open shapes line width is split evenly along the path
;;; closed shapes have options for fill, gradient, etc.
;;; closed shape line width has option to be inside, centered, or outside the path

;;; Text converted to path/curve is now a real (boy) path/curve, but
;;; retains history so you can change the text later.  Does this mean
;;; the text "IsA" path always? 

;;; Next level -- can something here be (IsA) something from above,
;;; and also contain items from above?  How to do schematic nets which
;;; can be branched, and presumably don't have arrows, and also
;;; visio-style connectors which may be branched but also have multiple
;;; arrows?

;;; Connectors/nets always have a "net name".  User can display and
;;; change from default auto-generated name, or leave it hidden.  Nets
;;; are somewhat abstract and usually exist as being a connector This
;;; is to allows manipulation of net list, etc.  Maybe not such a good
;;; idea if you create a new net without a corresponding graphic, but
;;; this should somehow be allowed, with warning.  System will never
;;; create a net without a graphic, and will delete the net if the
;;; last graphic that references it is deleted.  

;;; Next level
;;; connector IsA: net, [open-shapes, [texts == net name]] (can be branched,
;;;                segments can overlap and endpoint graphics can be configured individually)
;;; shape     IsA: polygon, ellipse, [name]

;;; Visio - Component editor
;;; Box -- IsA: symbol, [shape, nodes, name]
;;; etc.
;;; Visio - Main editor
;;; library of [Boxes], etc., access to previous levels

;;; EE - Symbol editor (conflated with library editor?)
;;; resistor  IsA: symbol, [open-shapes, nodes, parameters]
;;; capacitor IsA: symbol, [open-shapes, nodes, parameters]
;;; chip-box  IsA: symbol, [closed-shapes, nodes, parameters]
;;; EE -- Library editor
;;; load symbols, add pins, add parameters
;;; chip-box





;;; Junk
;;; named-open-shape   IsA: named-open [line|ellipse-arc|curve, text]
;;; named-closed-shape IsA: named-closed [regular-polygon|star-polygon|ellipse|closed-curve, text]
;;; named-text         IsA: text



;;;my-sensor-lib.edn
;;; Just lists prototypes.  Can refer to other prototypes
;;; How to refer to real database for vendor and manufacturer details, or other parameters?
{:prototype/name "KXR94-2353"
 :prototype/tags ["chip" "sensor" "accelerometer" "soic" "soic8" "kionix"]
 :prototope/schematic-symbol [{:reference "eesym/chip-box"
                               :instance-name "main symbol"
                               :parameters {:width 10
                                            :height 20
                                            :pins [{:x 10 :y 20 :number "1" :name "vdd" :rotation 90 :length 5}
                                                   {:x 15 :6 20 :number "2" :name "gnd" :rotation 90 :length 5}
                                                   {:reference "generic pin" :name "input" :rotation 90}]}
                               :x 0 :y 0}
                              {:primitive :text
                               :parameters {:text "KXR accel"
                                            :font "Times New Roman"
                                            :size 12}
                               :x 12 :y 13}]
 :footprint [:reference "eefootprints/soic8"]}

{:name "LM35"
 :tags ["chip" "sensor" "temperature" "soic"]
 :reference #dblookup ("sql or datomic search request")}

;;;my-cpu-lib.edn
;;; Just lists prototypes.  Can refer to other prototypes
{:name "MK60DN512ZCAB"
 :tags ["chip" "mcu" "bga"]
 :schematic-symbol [{:reference "eesym/chip-box"
                     :instance-name "main symbol"
                     :parameters {:width 10
                                  :height 20
                                  :pins [{:x 10 :y 20 :number "1" :name "vdd" :rotation 90 :length 5}
                                         {:x 15 :6 20 :number "2" :name "gnd" :rotation 90 :length 5}
                                         {:reference "generic pin" :name "input" :rotation 90}]}}]
  :footprint [:reference "eefootprints/bga100"]}


;; Presumably the proto-ee-db is a conglomeration of many lib (.edn)
;; files.  How to get the elements from many lib files into one?
;; The files have to be infinitely recursive, so they must have the same format


(def proto-ee-db (atom [{:source-file "my-sensor-lib.edn"
                         :name "KXR94-2353"
                         :tags ["chip" "sensor" "accelerometer" "soic"]}
                        {:source-file "my-sensor-lib.edn"
                         :name "LM35"}
                        {:source-file "my-mcu-lib.lib"
                         :name "MK60DN512ZCAB-4N22R"}]))

(def proto-draw-db (atom ["Box"
                          "TextBox"
                          ]))


(def main-db (atom {:components []
                    :netlist []}))



;;; Normally in altium the .lib file immediately points to a database.
;;; The database has parameters for the part, and points to another
;;; .lib file for the symbol and another one for the footprint.  This
;;; should be generalized, or at least *could* be generalized if it
;;; doesn't get too confusing.
