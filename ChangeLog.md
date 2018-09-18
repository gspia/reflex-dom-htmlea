# Revision history for reflex-dom-htmlea


## 0.1.1.6 -- 2018-09-18

* Removed data-default -package and use explicit defConstructors.
* Removed e-prefix from html- and attribute-functions and data-structures.
  (As it is possible to import qualified there is no need for prefixes.)
* Add tree-component infra and first examples (listener's and activations 
  so that sub-trees and paths can be activated and de-activated).
  First set of tree-component examples.
* Changed the structures used to declare components (e.g.  TableConf) so
  that combining functions use CommonAde-functions.  This way, several listeners
  and activation functions work as such for several components (tables, trees,
  and the forthcoming ones).
* Added activation functions to allow one active region that is de-activable.
  (It still has things to be done.  Example with trees.)
* Update all examples to work with the above changes.
* Functions that help to give initial values for tables and trees.
* New table-example to handle external events ("select all" and "de-select all").
  This example also shows how to give initial values.
* New listeners (with examples).
* Most of the drawing methods into a separate module.
* Some renamings.
 

## 0.1.1.5 -- 2018-02-08

* Function mkRow as a parameter to the mkTableV (inside tableConf).
* Some function renamings.
* Row attribute setting (tr-elements).
* A function listenRow.
* Examples on row selection (that is, using listenRow).
* An example on column selection.
* Documentation updates in many places.
* Move vector-sized related things to a separate package.


## 0.1.1.4 -- 2018-01-26

* Changed the cabal-file position of the lib.
* Small updates to the texts of introduction of the table examples.


## 0.1.1.3 -- 2018-01-25

* Added some Semigroup instances
* Added functionality to build tables with varying "activity" functions
  to activate cells in different ways. Both vector and vector-sized can 
  be used as input.
* exampleTbl gives examples on how to use the table-component.
* Started to use the new "project"-setup.
* todo-list into a separate file.


## 0.1.1.2 -- Where's this one?


## 0.1.1.1 -- 2017-10-08

* Fix on Start -attribute.


## 0.1.1.0 -- 2017-10-07

* Added some Monoid instances.
* Changed to use several packages (cabal new-stuff).


## 0.1.0.0  -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
