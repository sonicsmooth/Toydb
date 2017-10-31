(ns toydb.event
  (:require [jfxutils.core :refer :all :except [main]]))


" How does this work?  

Currently, each node in the network (a JavaFX Node or a var) maintains
its own reaction.  The var has a watch which calls .setWhatever on a
number of Nodes.  Each Node has a ChangeListener which forces a change
on the var.  In both cases, the event gets passed around until one of
the nodes determines the values are 'equal'.  This may cause
instability or ping-ponging of the values.

A better way would be for each change to only 'see' each node once.
Each node can then participate in a generic directed graph.  If a node
needs to change the value, eg the var needs to force something to be
an integer, the node can prevent propagating the value and instead
emit a new value.  If a node has 'seen' the event already, then the
event is ignored.  By 'event' here we don't mean a JavaFX Event, but
some other Clojurish thing we are making up.

How to implement this?  Either each node remembers what events it has
seen, or the event remembers what nodes it has visited.  The first one
implies each node accumulates a list of events, and each event needs a
unique identifier.  The second one suggests recognizing the event is
transient and can accumulate the list of nodes it has visited, then
disappear.

So the list of nodes visited is associated with the event.  How about
the network?  We already know that bidirectional graphs in immutable
language is not possible.  So what about a table showing the
connections?  Routing table could be a source: targets kind of thing,
eg {node1 [node2 node3],...} shows node1 as source and node2 and node3
as targets.  Later in the table node2 and node 3 can be sources.  If
they point back to node1, that is okay, since the event will know it
has already visited node 1.  

If a node is intended only as a sink, such as a read-only text field,
then it does not appear as a key, only as a target.  If a node, such
as a text field, must wait until defocus to get updated, then it
appears as both a source and a target, but gets flagged as being
updated only on a certain event, such as losing focus.

If there needs to be hierarchical or segmented networks, then two event
tables can be tied together.  For example if within a dialog box you
have a var and a bunch of gui elements, then outside the dialog box,
back in the regular application, you only care about the var.  So the
application routing table can include reference to the var in the ui
routing table.  This allows the UI to work independently, but still
participate in the bigger application.  When the UI is created, it
places its routing table in the userData, so the app can retrieve it.
Now the app has a map of some sort.  The map has a value which is just
the var, which itself is a map.  So you could do something like

(let [ui-var (:the-var (.getUserData my-ui-panel))
      routing-table {[ui-var inner-get1 inner-get2] [target1 target2],
                      source1 [target1 [ui-var inner-get3 inner-get4]]}])

How does the event get propagated?  The event is a pair of [value
visited-targets], where visited-targets grows each time a target is
visited.

Or is it?  That would make sense if the network were unknown.  But in
fact the network for each signal has the var at the center and targets
all around.

So the first declaration is better -- a var and a list of targets,
where it is assumed each target can source the signal, unless
otherwise noted.  

So we're back at a map with :var var and :targets [target1, ...],
except we shouldn't call these targets since they can also be sources.
Instead we should think of this as a net with multiple sinks and
sources, but the var is where the value is really stored.

When the var needs to change first, use either swap! or a helper fn.
The var will have a validator to make sure the value is acceptable.
If not acceptable, an exception will be thrown.  If it may be
necessary to modify the value before it reaches the var, such as
rounding to the nearest x, then use the helper fn.  The helper fn will
correct the value, then call swap!  In any case, because the validator
is in place, the body of the watcher fn is guaranteed to have a valid
value.  In the watcher fn, this gets propagated to each node by
temporarily removing the node's change listener, changing the value,
then replacing the change listener.  This is not at all elegant, but
apparently it's the way you're supposed to do things in JavaFX.

When a property in a node changes, it's easy to change the var from
the ChangeListener, but how to make sure the var skips updating the
source?  Maybe we don't bother.  JavaFX makes sure the ChangeListener
is not called when the .setWhatever sets the same value, and in many
cases the value coming back from the var is 'cleaned up', typically
rounded to the nearest whatever for numerical stuff.  So the
ChangeListener can calmly call the var fn and expect something to come back.

"



(def xxx {:var thevar
          :keyvec [:one :two]
          :property :the-prop
          :targets [t1 t2 t3]
          :vartoprop f1
          :proptovar f2})

{t1 {:var thevar
     :keyvec [:one :two]
     :property :the-prop
     :vartoprop f1
     :proptovar f2}
 t2 {:var thevar
     :keyvec [:one :two]
     :property :the-prop
     :vartoprop f1
     :proptovar f2}
 }


(def bp (javafx.beans.property.SimpleBooleanProperty. true))

(def block (atom false))

(def il (invalidation-listener [] (printexp (.get observable))))

(.addListener bp il)
(.addListener bp il)
(.addListener bp il)

(def *depth*)





















