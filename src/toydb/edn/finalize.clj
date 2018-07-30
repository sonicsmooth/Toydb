(ns toydb.edn.finalize)


(defprotocol FinalizeProtocol
  (final [x]))

(extend-protocol FinalizeProtocol
  nil
  (final [x]
    ;;(println "Finalizing nil")
    )
  java.lang.Object
  (final [x]
    ;;(println "Finalizing generic object" x)
    ))

