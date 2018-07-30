(ns toydb.app.application-panes
  (:require [jfxutils.core :as jfxc :exclude [main]]
            [toydb.app.treeview :as treeview]
            [toydb.app.menubars :as mb]
            [toydb.editor.canvas :as canvas]
            [toydb.editor.editor :as editor]
            [toydb.ui.SettingsPane])

  (:import 
   [java.net InetAddress]
   [javafx.application Application]
   [javafx.geometry Pos]
   [javafx.scene Scene Group]
   [javafx.scene.control Slider Label Button]
   [javafx.scene.layout BorderPane Pane StackPane AnchorPane HBox VBox Region Priority]
   [javafx.scene.paint Color Paint]
   [javafx.scene.shape Circle Ellipse Line]
   [javafx.scene.text Text Font]))



;; populates as you go
(defn explorer-pane
  "Returns tree-view immediately.  Expects
  tree-view to launch background threads to populate pane with
  explorer items as they come in."
  [parent-app]
  (treeview/treeview(InetAddress/getLocalHost) ))


(defn db-pane
  "Returns database pane"
  [parent-app]
  (treeview/treeview @(:database parent-app) ))

(defn add-menu [menubar & menuspecs]
  "Adds stuff to menubar, merging as needed.  menuspecs is list of
  maps describing menus hierarchically.  Each menu specification looks
  something like this.  Each map is a menu.  Items are a vector, etc.

{:text 'File'
 :id 'whatever'
 :items [{:id 'menu-file-exit'
          :text 'Exit'
          :icon whatever
          :action some-function}
         {:text 'Submenu'
          :items [bla bla]}]}}"

  )

(defn application-pane
  "Creates BorderPane with menus at top, and DockPane in center where
  everything attaches.  No DockNodes are created here.  parent-app is
  the top level MyApplication instance.  Behaviors is list of maps
  defining additional behaviors, such as menus from particular modules
  such as editor."
  [parent-app & behaviors]
  ;; Clear out the old settings pane
  (toydb.ui.SettingsPane/reset-pane!)
  
  (let [open-item (mb/menu-item :id "menu-file-open", :text "Open", :icon "icons/tango-icon-theme-0.8.90/16x16/actions/document-open.png")
        new-item  (mb/menu-item :id "menu-file-new",  :text "New",  :icon "icons/tango-icon-theme-0.8.90/16x16/actions/window-new.png")        
        save-item (mb/menu-item :id "menu-file-save", :text "Save", :icon "icons/tango-icon-theme-0.8.90/16x16/actions/document-save.png")
        saveas-item (mb/menu-item :id "menu-file-save-as", :text "Save As...", :icon "icons/tango-icon-theme-0.8.90/16x16/actions/document-save-as.png")
        submenu-item (mb/menu :name "Submenu", :icon "icons/tango-icon-theme-0.8.90/16x16/actions/list-add.png"
                              :items [(mb/menu-item :id "menu-file-sub-1", :text "Sub1")
                                      (mb/menu-item :id "menu-file-sub-2", :text "Sub2")])
        separator-item (mb/separator-menu-item)
        close-item (mb/menu-item :id "menu-file-exit",
                                 :text "Exit",
                                 :icon "icons/tango-icon-theme-0.8.90/16x16/actions/system-log-out.png"
                                 :action #(jfxc/close-all-stages))

        settings-pane (toydb.ui.SettingsPane/settings-pane nil)
        settings-tab-pane (jfxc/lookup settings-pane "settings-tab-pane")
        app-menu-bar (mb/menu-bar [(mb/menu :text "File"
                                            :items [open-item new-item save-item saveas-item submenu-item separator-item close-item])
                                   (mb/menu :text "Settings"
                                            :items [(mb/menu-item :id "settings-grid"
                                                                  :text "Grid"
                                                                  :icon "icons/tango-icon-theme-0.8.90/16x16/categories/preferences-desktop.png"
                                                                  :action #(toydb.ui.SettingsPane/show settings-pane))])])
        pane-label (Label. "App status bar")
        mouse-label (Label.)
        stage-label (Label.)
        app-status-bar (mb/status-bar [pane-label mouse-label stage-label])
        pane (jfxc/jfxnew BorderPane ;; center is set in (main-window...)
                          :top app-menu-bar
                          :bottom app-status-bar
                          ;; Set ugly horizontal gray gradient
                          :background (jfxc/gradient-background :horizontal (Color/web "#222222") (Color/web "#cccccc")))]

    ;; Implement tabs, menus and stuff from behaviors
    (doseq [behavior behaviors]
      (condp = (:type behavior)
        :settings-pane (jfxc/add-tabs! settings-tab-pane
                                       [(javafx.scene.control.Tab.
                                         (:name behavior)
                                         (:root behavior))])))


    pane))




       
      










