(defproject toydb "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [jfxutils "0.1.0-SNAPSHOT"]
                 [docks/docks "0.1.0-SNAPSHOT"]
                 ;;[uncomplicate/neanderthal "0.14.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 ;;[org.jfxtras/jfxtras-common "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-fxml "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-agenda "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-window "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-menu "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-gauges-linear "8.0-r5"]
                 ;;[org.jfxtras/jfxtras-font-robto "8.0-r5"]
                 ;;p[org.jfxtras/jfxtras-labs "8.0-r5"]q
                 [org.controlsfx/controlsfx "8.40.13"]
                 ;;[com.taoensso/tufte "1.1.2"] ;; profiling
                 ;;[net.java.dev.jna/jna "4.5.0"]
                 ]
  :resource-paths ["resources/fxml"
                   "resources/css"
                   "resources"
                   "settings"]

  ;; Still not sure why this is here
  ;;:clean-targets ^{:protect false} [:target-path]

  
  :profiles {:dev {:jvm-opts ["-Dtoolkit-compile-timeout=10000"
                              "-Dtoolkit-debug=true"
                              "-XX:-OmitStackTraceInFastThrow"
                              ]}
             #_:uberjar #_{:jvm-opts ["-Dtoolkit-compile-timeout=5000"
                                  "-Dtoolkit-debug=true"]}
             :ui {:aot [toydb.ui.grid-settings-pane]}
             }

  ;;:aot :all
  :main toydb.ui.GridSettingsPane
  ;;:main toydb.core


  










  )
