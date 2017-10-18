
(ns toydb.units)


(defprotocol DistanceProtocol
  (km [u])
  (m [u])
  (cm [u])
  (mm [u])
  (um [u])
  (mil [u])
  (inch [u]))

(defrecord kilometer [value])
(defrecord meter [value])
(defrecord centimeter [value])
(defrecord millimeter [value])
(defrecord micrometer [value])

(extend-protocol DistanceProtocol
  kilometer
  (km [u] u)
  (m [u] (->meter        (* 1e3 (.value u))))
  (cm [u] (->centimeter  (* 1e5 (.value u))))
  (mm [u] (->millimeter (* 1e6 (.value u))))
  (um [u] (->micrometer  (* 1e9 (.value u))))

  meter
  (km [u] (->kilometer   (* 1e-3 (.value u))))
  (m [u] u)
  (cm [u] (->centimeter  (* 1e2 (.value u))))
  (mm [u] (->millimeter (* 1e3 (.value u))))
  (um [u] (->micrometer  (* 1e6 (.value u))))

  centimeter
  (km [u] (->kilometer   (* 1e-5 (.value u))))
  (m [u] (->meter        (* 1e-2 (.value u))))
  (cm [u] u)
  (mm [u] (->millimeter  (* 1e1 (.value u))))
  (um [u] (->micrometer  (* 1e4 (.value u))))

  millimeter
  (km [u] (->kilometer   (* 1e-6 (.value u))))
  (m [u] (->meter        (* 1e-3 (.value u))))
  (cm [u] (->centimeter  (* 1e-1 (.value u))))
  (mm [u] u)
  (um [u] (->micrometer  (* 1e3  (.value u))))

  micrometer
  (km [u] (->kilometer   (* 1e-9 (.value u))))
  (m [u] (->meter       (* 1e-6 (.value u))))
  (cm [u] (->centimeter (* 1e-4 (.value u))))
  (mm [u] (->micrometer (* 1e-3 (.value u))))
  (um [u] u))


