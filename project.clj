(defproject toydb "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[jfxutils "0.1.0-SNAPSHOT"]
                 [docks/docks "0.1.0-SNAPSHOT"]
                 ;;[uncomplicate/neanderthal "0.14.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 [aprint "0.1.3"]]
  :resource-paths ["resources/anchor_resources"
                   "resources/dock_resources"
                   "resources"
                   "../../"] ;; back to /c/dev/, eg for icons
  :jvm-opts ["-Djavafx.animation.fullspeed=true"]
  :main toydb.core)
  
