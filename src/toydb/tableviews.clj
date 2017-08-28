(ns toydb.tableviews
  (:require [toydb.siconverter :as si])
  (:require [toydb.db :as db])
  (:require [clojure.reflect :as r])
  (:use [jfxutils.core :exclude [-main]])
  (:use [clojure repl pprint])


  (:import
   [javafx.beans.property SimpleObjectProperty]
   [javafx.geometry Pos]
   [javafx.scene Node]
   [javafx.scene.control CheckBox ComboBox ContentDisplay Label ListCell TableCell
    TableColumn TableColumn$CellEditEvent TableView TextField Tooltip ColorPicker]
   [javafx.scene.control.cell  TextFieldTableCell]
   [javafx.scene.input KeyCode]
   [javafx.scene.layout HBox Priority]
   [javafx.scene.paint Color]
   [javafx.scene.shape Rectangle]
   [javafx.util Callback StringConverter]))


(comment
  ;;The following cells are available
  [TextCell CheckBoxCell EngStringCell SiStringCell ColorBoxCell LineStyleCell LineWidthCell ]
  ;; The following multiple-cell views are available
  [InspectorView TableView TreeView]
  ;; The following drawing or miscellaneous views are available
  [PlotView SchematicView LayoutView ComponentView]

  ;; If map or maps a passed, assume read only, use :access key to dig in to data
  ;; If var is passed, assume read/write, use :access key if supplied for read/edit
  (tableview [map1 map2]) ;; show maps only, not editable
  (tableview myvar) ;; show and edit, assume simple list of maps at top level of myvar
  (tableview myvar {:access [:keys :to :list-of :maps]}) ;; show and edit
  (inspectorview map1) ;; show one map only, not editable
  (inspectorview myvar) ;; show and edit one map, @myvar must be the map.
  (inspectorview map1 :as {:color1 TextCell ;; show map, not editable, force values 
                           :color2 ColorBoxCell ;; of given keys to render as certain
                           :linewidth LineWidthCell ;; type of cell
                           :value1 EngStrCell}) 
  ;; :options are ignored if there is no :access key
  (inspectorview myvar
                 :accesspath [:keys :to :one :map 0] ;; show and edit
                 :as {:color1 TextCell
                      :color2 {:render-as TextCell :options {:combo-items [list-of-colors]}} ;; Makes dropdown of text
                      :color3 {:render-as ColorBoxCell :options {:color-chooser True}} ;; Starts color chooser
                      :option1 {:render-as TextCell :options {:combo-items [list-of-options]}} ;; Makes dropdown
                      :option2 {:options :spinner}}) ;; Puts spinner and uses inc/dec on value   

  ;; mmm... I don't like the above.  How about this;
  (inspectorview myvar
                 :accesspath [:keys :to :starting :node]
                 :field-options {:<some-field-name> {:combo-items list-of-things ;; show dropdown items, may be exclusive with below
                                                     :render-as someCellType ;; where SomeCellType takes field value in constructor
                                                     :options moreOptions }}) ;; where options are passed to SomeCellType

  ;; So in above case, SomeCellType could be dropDownCell, and
  ;; moreOptions could be a list of items that are passed to dropDownCell after the cell value
  ;; Otherwise, cells are rendered by default according to the value type
  ;; string->string, keyword->string, numbers->string, color->colorCell, bool->checkBox
  
  ;; field names are labels, field values have text representation if
  ;; there is no other default representation. additional arguments
  ;; must be passed to add helpful modifiers, such as data for
  ;; listitems, drop-downs, arrow up/down, etc.
  ;; Any cad references must be dereferenced before use -- no showing "reference" in text
  )


(def color-fields (.getFields (class Color/BLUE)))
(def color-vals (map #(. % get nil) color-fields))
(def color-names (map #(. % getName ) color-fields))
(def color-map (zipmap color-names color-vals))
(def color-map-inverse (zipmap color-vals color-names))

(defn set-text-colors!
  "Sets background and font color to indicate error"
  [node bg fg]
  (letfn [(color-string [color]
            "Extracts 0xrrggbbpp from color and returns #rrggbb"
            (let [hash-string (clojure.string/replace (.toString color) #"0x" "#")]
              (apply str (take 7 hash-string))))]
    (.setStyle node (str "-fx-background-color: " (color-string bg) ";"
                         "-fx-text-fill: " (color-string fg) ";"))))

(defn calc-full-accesspath
  "In inspector-view, accesspath just takes us to map, not item.  Need
  to get additional path element.  If we're in a table-view (not
  inpsector-view) then our col's userdata will match (last
  accesspath).  If not then we are in an inspector-view, and our col's
  userdata is probably either key or val function and we must find the
  userdata of the other col.  Returns vector of var and full
  accesspath.  cellevent is either cell or event, as they support
  some of the same functions..."
  [cell-or-event]
  (let [tv (.getTableView cell-or-event)
        items (.getItems tv)
        {:keys [var accesspath field-options]} (.getUserData tv)
        column (.getTableColumn cell-or-event)
        {:keys [accessfn]} (.getUserData column)
        list-item (cond (instance? TableCell cell-or-event) (nth items (.getIndex cell-or-event)) ;; Comes from startEdit
                       (instance? TableColumn$CellEditEvent cell-or-event) (.getRowValue cell-or-event))] ;; Comes from commit event
    (cond
      (map? list-item) ;; for table-view
      ;; Do O(n) search through items to see where we are.  Cannot use
      ;; known var or initial list since the local ObservableList inside
      ;; the TableView could be in a different sort order.  There is also
      ;; No (easy) way to get a sort mapping from the current sort state,
      ;; So we just search through the list for the entire list-item.  It's
      ;; pretty fast (enough), eg 3ms to search through 100k of random maps.
      (let [index (.indexOf items list-item)
            accesspath (conj accesspath index accessfn)]
        [var accesspath]) ;; in table-view, so basically do nothing

      (instance? clojure.lang.MapEntry list-item) ;; for inspector-view
      (let [columns (.getColumns tv) ;; two columns, but we don't know which column we're in
            cellkey (key list-item) ;; Each item is map entry
            accesspath (conj accesspath cellkey)]
        [var accesspath])

      :else
      (throw (Exception. "Reached else condition in calc-full-accesspath")))))

(defn get-field-options
  "Return field options for cell, or nil if none.  The contents of the
  cell must be non-nil"
  [cell]
  (let [[_ full-accesspath] (calc-full-accesspath cell)
        all-field-options (:field-options (.getUserData (.getTableView cell)))]
    (if all-field-options
      (all-field-options (last full-accesspath))
      nil)))

(defn set-graphic-text!
  "Just sets the graphic and text of a node, cell, whatever.  If
  either graphic or text is nil, then the other one will be the only
  one displayed.  If they're both nil or both not-nil, then it reverts
  to the default of placing the graphic to the left of the text.  Does
  NOT clear out a value by setting it to nil."
  [cell graphic text]
  (when graphic (.setGraphic cell graphic))
  (when text (.setText cell text))
  (cond (and (nil? graphic) text) (.setContentDisplay cell ContentDisplay/TEXT_ONLY)
        (and graphic (nil? text)) (.setContentDisplay cell ContentDisplay/GRAPHIC_ONLY)
        :else (.setContentDisplay cell ContentDisplay/LEFT)))

(defn get-leaf-columns-from-column
  "Recursively find all editable leaf columns from single base column"
  [column]
  (let [sub-columns (.getColumns column)]
    (if (and (empty? sub-columns)
             (.isEditable column))
      [column]
      (mapcat get-leaf-columns-from-column sub-columns))))

(defn get-leaf-columns-from-table
  "Returns list of leaf columns from tableview"
  [tv]
  (mapcat get-leaf-columns-from-column (.getColumns tv)))

(defn get-next-cell-position
  "Returns next available TableCell in first-right-then-down order.
  Returns cells from editable columns only, but ignares whether the
  cell itself is editable.  Returns a vector with next index and next
  column, for use directly in TableView.edit() function.  If no next
  column is available, returns column of given cell.  If no next index
  is available (such as at the bottom of the table), returns -1 for
  the index"
  [cell forward]
  (try
    (let [dir (if forward inc dec)
          tv (.getTableView cell)
          numrows (count (.getItems tv))
          leaf-cols (get-leaf-columns-from-table tv)
          this-col (.getTableColumn cell)
          this-col-index (.indexOf leaf-cols this-col)
          next-col-index (mod (dir this-col-index) (count leaf-cols))
          next-col (nth leaf-cols next-col-index)
          this-index (.getIndex cell)
          next-index (let [dni (dir this-index)]
                       (if (= next-col this-col) ;; try to go up or down if we're in the same column
                         (if (or (< dni 0) (>= dni numrows)) -1 dni) ;; but return -1 if we're at the end
                         this-index))] ;; otherwise go left or right
      [next-index next-col])
    (catch Exception e
      (println "Exception in get-next-cell-position\n" e))))

(defn make-color-box
  "Makes rectangle filled with color, suitable for a cell"
  [color]
  (jfxnew Rectangle :width 100, :height 20, :fill color))

(defn column-commit
  "For some reason this is the standard place to update things from
  after an edit.  "
  [event]
  (apply db/greedy-commit! (.getNewValue event) (calc-full-accesspath event)))

(defn labeled-tfhbox
  "Make an HBox with textfield surrounded by two labels.  The left
  label is the name of the field.  The right label is the type of the
  field.  The field and fieldname are passed directly, not the
  strings.  Field will pretty much always be either an integer or a
  keyword.  Stringification happens in this function.  Return both
  hbox and the TexField's textProperty"
  [field fieldtype & [prompt-text]]
  (let [leftstring (if (instance? clojure.lang.Keyword field) 
                     (name field)
                     (str "nth: " field) )
        rightstring (str fieldtype)
        tf (jfxnew TextField 
                    :min-width 50
                    :prompt-text prompt-text
                    :extra [(HBox/setHgrow Priority/ALWAYS)])
        hb (jfxnew HBox
                    :children [(jfxnew Label leftstring :min-width 50)
                               tf 
                               (jfxnew Label rightstring :min-width 50)]
                    :spacing 10
                    :alignment Pos/CENTER
                    :fill-height true)]
    [hb tf (.textProperty tf)]))

(defn render-cell-with-item!
  "Puts correct item in cell graphic and/or text property based on item
  type.  Additional arguments for editing such as drop-down, are
  handled in the startEdit function; this function just renders the
  cell when called by updateItem or cancelEdit."
  [cell item]
  (let [klasstr (str (class item))
        klassname (last (.split klasstr "\\."))
        tooltip (jfxnew Tooltip klassname)]
    (Tooltip/install cell tooltip))

  (cond
    (instance? Node item) (do (.setEditable cell false)
                              (set-graphic-text! cell item nil))
    (instance? Boolean item) (let [[var accesspath] (calc-full-accesspath cell)
                                   cb (jfxnew CheckBox
                                              :text (str item)
                                               :selected item
                                               :disable (not (db/mutable? var)))]
                               (.setEditable cell false)
                               (set-graphic-text! cell cb nil)
                               #_(when (db/mutable? var)
                                 (uni-bind! (.selectedProperty cb) #(db/greedy-commit! % var accesspath))))
    (instance? clojure.lang.PersistentVector item) (do (.setEditable cell false)
                                                       (set-graphic-text! cell (Label. "Put vector editor here") nil))
    (instance? Color item) (doto cell (.setEditable true)
                                 (set-graphic-text! (make-color-box item)
                                                    (color-map-inverse item)))
    ;; All other types go here, presumably text types, so assume editable
    :else (doto cell (.setEditable true)
                (set-graphic-text! nil (si/to-normstr item)))))

(def FANCY-LISTCELL-FACTORY ;; For drop-down
  (callback [listview]
            (proxy [ListCell] []
              (updateItem [item empty]
                (proxy-super updateItem item empty)
                (when (and (not empty) item)
                  (render-cell-with-item! this item))))))

(defn make-focus-change-listener
  ;; Creates listener that commits current value and cancels edit when
  ;; defocused.  getterfn retrieves the value to commit
  [cell getterfn]
  (change-listener [oldval newval]
                   (if (true? newval)
                     ;;(println "focused graphic in cell" cell)
                     nil
                     (do  ;; on defocus, commit what's there
                       ;;(println "defocused graphic in cell" cell)
                       (apply db/greedy-commit! (getterfn) (calc-full-accesspath cell))
                       (.cancelEdit cell)))))

(defn add-cell-handlers!
  "Adds common keyboard handler and focus listener to temporary editing graphic.
  graphic is typically textfield or combo-box. cell is tablecell which
  is being edited.  getterfn is function to get value from graphic so
  it can be commited to database."
  [graphic cell getterfn]
  (let [focus-listener (make-focus-change-listener cell getterfn)]
    (add-listener! graphic :focused focus-listener)
    (.setOnKeyPressed graphic
                      (event-handler [e] ;; here "cell" still refers to the tablecell
                                    (try
                                      (condp = (.getCode e)
                                        KeyCode/ENTER (do ;;(println "ENTER pressed.  Removing focus listener")
                                                        (remove-listener!* cell :focused focus-listener)
                                                        (.commitEdit cell (getterfn)))
                                        KeyCode/ESCAPE (do ;;(println "ESC pressed. Removing focus listener")
                                                         (remove-listener!* cell :focused focus-listener)
                                                         (.cancelEdit cell)) ;; Removes textfield
                                        KeyCode/TAB (let [[next-row-index next-tablecolumn] (get-next-cell-position cell (not (.isShiftDown e)))]
                                                      (println "tab")
                                                      (remove-listener!* cell :focused focus-listener)
                                                      (.commitEdit cell (getterfn)) 
                                                      (.edit (.getTableView cell) next-row-index next-tablecolumn)
                                                      (.consume e)) ;; Prevents tab from reaching tableview parent which would select the next row
                                        nil) ;; do nothing
                                      (catch Exception e
                                        (println "Exception in cell keyboard eventhandler:\n" e)))))))
(defn make-combobox
  "Implements dropdown combobox.  'cell' is fancy table cell in
  question.  'items' is list of things for dropdown, which can be
  anything that the dropdown can render and choose as the final item"
  [cell initvalue & [items]]
  (let [cmb (jfxnew ComboBox (observable items))
        cell-factory FANCY-LISTCELL-FACTORY
        blank-cell (.call cell-factory nil)]
    (doto cmb
      (add-cell-handlers! cell #(.getValue cmb))
      (.setValue initvalue)
      (.setButtonCell blank-cell)
      (.setCellFactory cell-factory))))

(def SIMPLE-TABLECELL-FACTORY
  ;; Just creates a string label, but allows any object which is subject
  ;; to siconverter.  Essentially this allows keywords to be used as
  ;; labels.  The toString fn here could just be (name obj) and it would
  ;; work just fine
  (TextFieldTableCell/forTableColumn
   (proxy [StringConverter] []
     (toString [obj] (si/to-normstr obj))
     (fromString [s]
       (println (str "From string \"" s "\""))
       (eval (read-string s))))))



(defn textish?
  "Returns true if item can easily be represented and edited as text, ie
  number, string, keyword etc."
  [item]
  (or (instance? String item)
      (instance? clojure.lang.Keyword item)
      (number? item)))

(defn focus-row
  "Causes this row to get both focus and select.  If one argument,
  pass a cell.  If three arguments, pass the tableview, row index, and
  tablecolumn"
  ([tv row-index tablecolumn]
   (.select (.getSelectionModel tv) row-index tablecolumn)
   (.focus (.getFocusModel tv) row-index tablecolumn))
  ([cell]
   (focus-row (.getTableView cell)
              (.getIndex cell)
              (.getTableColumn cell))))

(defn make-textfield-editor
  "Implements changing text fields and listeners, etc, upon entering
  editablle state, for cells which use a textfield for user editing"
  [cell] ;; 'cell' is the fancy custom table cell in question
  (let [[var full-accesspath] (calc-full-accesspath cell)
        tf (jfxnew TextField (.getText cell))]  ;; tf used for editing text
    (add-cell-handlers! tf cell #(.getText tf))
    tf))

(def FANCY-TABLECELL-FACTORY
  "The main callback interface which constructs the actual each cell
  for arbitrary types.  Assumes an editable cell for text representations."
  (callback [column]
            (proxy [TableCell] []
              (updateItem [item empty]
                (proxy-super updateItem item empty)
                (when (not empty) 
                  (render-cell-with-item! this item)))
              (startEdit []
                (proxy-super startEdit)
                ;; Change to appropriate graphic when editing
                (println "In proxy's startEdit.")
                (focus-row this)
                (try
                  (let [item (apply db/read-db (calc-full-accesspath this))
                        options (get-field-options this)] ;; could be nil ...
                    (cond (:combo-items options)
                          ;; Show combobox with given items
                          (let [cmb (make-combobox this item (:combo-items options))]
                            (set-graphic-text! this cmb nil)
                            (.requestFocus cmb)
                            (.show cmb)) ;; This makes drop-down appear without clicking twice.

                          ;; Show JFX-supplied ColorPicker
                          (:color-picker options)
                          (let [cp (ColorPicker.)]
                            (.setValue cp item)
                            (add-cell-handlers! cp this #(.getValue cp)) ;; needs custom handler 
                            (set-graphic-text! this cp nil)
                            (.requestFocus cp)
                            (.show cp))
                          
                          ;; otherwise, show text edit or something else
                          :else
                          (cond (textish? item)
                                (let [tf (make-textfield-editor this)]
                                  (println "setting textish")
                                  (set-graphic-text! this tf nil) ;; just set tf as graphic; leave existing text alone
                                  (.requestFocus tf)
                                  (.selectAll tf))
                                ;; trying to make sub-control focused when attempting to edit non-editable cell
                                ;; But this doesn't appear to work

                                (or (instance? Boolean item));;(instance? Node item) ;; really shouldn't allow Nodes in database
                                (.setFocused (.getGraphic this) true))))
                  (catch Exception e
                    (println "Exception in startEdit\n" e))))
              (cancelEdit []
                ;; CancelEdit gets called either by defocus or by ESC.
                ;; In any case, use the item currently in the database
                ;; for this cell and just render as in updateItem
                ;;(println "in cell's cancelEdit, before super")
                ;;(showstack)
                (proxy-super cancelEdit)
                ;;(println "in cell's cancelEdit, after super")
                (let [item (apply db/read-db (calc-full-accesspath this))]
                  (render-cell-with-item! this item)))
              (commitEdit [value]
                ;; Nothing to do here.  All commits happen either in
                ;; the textField callback or in the column edit
                ;; callback
                (proxy-super commitEdit value)))))



(def CELL-VALUE-FACTORY
  "Returns a reified callback interface which extracts list item values.
  When called, receives TableColumn instance, TableView instance, and
  the item.  Extracts and retuns the value of interest from the item
  associated with a given cell.  The value returned is the actual
  value, not just a String, so it could be a Button or any other
  object, etc.  This becomes the value for the cell, which is rendered
  by the cell produced by cell-factory"
  (callback [features]
            ;; For inspector-view, list-item is a map entry (key-value
            ;; pair). For table-view, list-item is is an entire map.
            ;; Column's userdata contains a map which holds accessfn,
            ;; which is a function that gets the value of interest
            ;; from the list-item (ie the cell-value).  For
            ;; inspector-view, the accessor is either key or val, as
            ;; the item is a map entry.  For table-view it is probably
            ;; a keyword, as the item is a map.  If a value is not
            ;; available, returns nil.
            (try
             (let [list-item (.getValue features)
                   column (.getTableColumn features)
                   {:keys [accessfn]} (.getUserData column)]
               ;; For inspector-view without alt-name, returns the key of the map entry as keyword
               ;; For inspector-view with alt-name, returns the alt-name as string
               ;; For table-view, returns the value
               (jfxnew SimpleObjectProperty (accessfn list-item)))
             (catch Exception e
               (println "Exception in CELL-VALUE-FACTORY\n" e)))))


(defn var-snapshot
  "Returns section of var for use in Observablelist.  Return value
  must be converted by caller using to-array."
  [var accesspath]
  (let [var-or-map (if (db/mutable? var) @var var)]
    (get-in var-or-map accesspath)))


(def global-iv (atom nil))
(def global-tv (atom nil))
(def global-col0 (atom nil))
(def global-col1 (atom nil))
(def global-col2 (atom nil))
(def global-cb (atom nil))


(defn make-table-watcher
  "Creates a watch function which is set on the var for each TableView
  instance"
  [tv var accesspath]
  (fn [k r o n] ;; Key Ref Old New
    ;; Here we watch the value of var and update the items in the table to match.
    ;; Problem is the sort order gets reset when items are added
    ;; Therefore we must capture the existing sort order before changing data
    ;; and replace sort ordering after the change.
    ;; Taken from http://stackoverflow.com/questions/11096353/javafx-re-sorting-a-column-in-a-tableview
    (try
      ;; Capture sort order and current cell selection
      (let [sorted-cols (vec (.getSortOrder tv)) ;; vec makes local copy
            sort-types (map #(.getSortType %) sorted-cols)
            sortables (map #(.isSortable %) sorted-cols)

            focusmodel (.getFocusModel ^TableView tv)
            tableposition (.getFocusedCell ^TableCell focusmodel)
            this-col (.getTableColumn tableposition)
            this-index (.getRow tableposition)]

        ;; Here we actually put the items into the tableview after the change
        (.setItems tv (observable (var-snapshot var accesspath))) 
        
        ;; Sort order is now empty so we put back what was in it
        (let [new-sort-order (.getSortOrder tv)] ;; gets list of TableColumns in their sort order
          (.setAll new-sort-order (into-array sorted-cols)) ;; reset the sort order based on what was there before

          ;; Assign sorting to each column
          (doseq [col sorted-cols, sort-type sort-types, sortable sortables]
            (.setSortType col sort-type)
            (.setSortable col sortable))

          ;; Select previous cells here, not in commit
          (.focus focusmodel this-index this-col)
          (.select (.getSelectionModel tv) this-index this-col)))
      (catch Exception e
        (println "Exception in watch function\n" e)))))



(defn inner-table-view*
  "Make inner table view for use by inspector-view and table-view"
  [var accesspath columns]
  (let [obslist (observable (var-snapshot var accesspath))
        tv (jfxnew TableView
                   :user-data {:var var ;; the actual var... 
                               :accesspath accesspath } ;; ... and how to get to the displayed data
                   :items obslist
                   :columns columns
                   :editable (db/mutable? var))]
    (let [selectmodel (.getSelectionModel tv)          
          focusmodel (.getFocusModel tv)]
      (.setOnKeyPressed tv (event-handler [e] ;; selects and focuses next cell and row
                                         (condp = (.getCode e)
                                           KeyCode/TAB
                                           (try
                                             (let [dir (if (.isShiftDown e) dec inc)
                                                   numrows (count (.getItems tv))
                                                   tableposition (.getFocusedCell focusmodel)
                                                   this-col (.getTableColumn tableposition)
                                                   this-index (.getRow tableposition)
                                                   next-index (let [dni (dir this-index)]
                                                                (if (or (< dni 0) (>= dni numrows)) this-index dni))]
                                               (.focus focusmodel next-index this-col)
                                               (.select selectmodel next-index this-col))
                                             (catch Exception e
                                               (println "event exception\n" e)))
                                           nil))))
    tv))

(defn table-view
  "Creates new TableView instance which represents many maps and their
  contents.  Takes seq of maps or atom/var/ref/agent of seq and
  displays fields as column names, and values in rows."
  [var & {:keys [accesspath field-options]}]
  (let [ismutable (db/mutable? var)
        snapshot (var-snapshot var accesspath)
        allkeys (keys (apply merge snapshot))
        colfn (fn [kee]
                ;; The colmnn takes on the vale of the :alt-name option if it exists
                (jfxnew TableColumn (or (:alt-name (kee field-options)) (name kee)) 
                        :cell-value-factory CELL-VALUE-FACTORY
                        :cell-factory FANCY-TABLECELL-FACTORY
                        :user-data {:accessfn kee}
                        :on-edit-start (event-handler [e] #_(println "editing column " (.getOldValue e) (.getNewValue e)))
                        :on-edit-cancel (event-handler [e] #_(println "canceling column with event" e))
                        :on-edit-commit (event-handler [e] (do #_(println "column's on-edit-commit handler calling column-commit") (column-commit e)))
                        :editable ismutable
                        :sortable true))
        cols (map colfn allkeys)
        tv (inner-table-view* var accesspath cols)]
    (.setColumnResizePolicy tv (TableView/CONSTRAINED_RESIZE_POLICY))
    (.setUserData tv (conj (.getUserData tv) {:field-options field-options}))

    ;; Add watches, use tv instance as key so we can remove it later
    ;; This gets called each time db is changed.
    (if (db/mutable? var)
      (add-watch var tv (make-table-watcher tv var accesspath)))
    (reset! global-tv tv)
    tv))

(defn inspector-view
  "Creates new TableView instance which represents a single map and
  its contents.  Takes plain map or atom/var/ref/agent of map and
  displays fields and values in JFX TableView. Compound values (ie
  maps, vectors, etc., for now are just displayed as their string
  value.  If access is supplied, assumes m is var/ref/atom and assigns
  appropriate linkage between m and view contents.  The topmost
  available var or map is assigned to the TableView, and the accessor
  for each field is assigned to each column."
  [var & {:keys [accesspath field-options]}]
  (let [ismutable (db/mutable? var)
        accessfn (fn [list-item] ;; Allows for alt-text for each field
                   (let [kee (key list-item)]
                     (or (:alt-name (kee field-options)) kee)))
        field-col (jfxnew TableColumn "Field"
                          :cell-value-factory CELL-VALUE-FACTORY
                          :cell-factory SIMPLE-TABLECELL-FACTORY
                          :user-data {:accessfn accessfn}
                          :editable false
                          :sortable true)
        value-col (jfxnew TableColumn "Value"
                          :cell-value-factory CELL-VALUE-FACTORY
                          :cell-factory FANCY-TABLECELL-FACTORY
                          :user-data {:accessfn val} ;; val is fn for accessing cell values from data item
                          :on-edit-start (event-handler [e] #_(println "editing column " (.getOldValue e) (.getNewValue e)))
                          :on-edit-cancel (event-handler [e] #_(println "canceling column with event" e))
                          :on-edit-commit (event-handler [e] (do #_(println "column's on-edit-commit handler calling column-commit") (column-commit e)))
                          :editable ismutable)
        #_type-col #_(jfxnew TableColumn "Type"
                             :cell-value-factory CELL-VALUE-FACTORY
                             :cell-factory SIMPLE-TABLECELL-FACTORY
                             :user-data {:accessfn #(type (val %))}
                             :editable false
                             :sortable true)
        cols [field-col value-col #_type-col]
        tv (inner-table-view* var accesspath cols)]
    ;; Add options to table's userData.  This is for inspector-view
    ;; not table-view, so we don't put this in inner-table-view
    ;; function
    (.setColumnResizePolicy tv (TableView/CONSTRAINED_RESIZE_POLICY))
    (.setUserData tv (conj (.getUserData tv) {:field-options field-options}))
   
    ;; Add watches, use tv instance as key so we can remove it later
    ;; This gets called each time db is changed.
    (if (db/mutable? var)
      (add-watch var tv (make-table-watcher tv var accesspath)))
    (reset! global-iv tv)
    (reset! global-col1 value-col)
    tv))

