(defproject toydb "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[jfxutils "0.1.0-SNAPSHOT"]
                 [docks "0.1.0-SNAPSHOT"]]
  :resource-paths ["resources/anchor_resources"
                   "resources/dock_resources"
                   "../../"] ;; back to /c/dev/, eg for icons
  :main toydb.core
  )
  
