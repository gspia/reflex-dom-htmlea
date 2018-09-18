
# TODO-list


- [ ] Table: start using reflex collection utilities.
- [ ] Tree: start using reflex collection utilities.
- [ ] Table: row-header usage with example.
- [ ] Table: col- and rowspans on vector-sized with example.
- [ ] Table: multi-cell selects have some extra updates (events) that 
      probably could be ffiltered/gated away.
- [ ] Table: multi-cell selects could return information about all cells selected
      (with examples).
- [ ] Table: example with some other elements than div's and text.
- [ ] Table: collapsing columns and rows (with sumfun hooks??)
- [ ] Table: reordering columns and rows (just the view)
- [ ] Table: module interface cleaning?
- [x] Table: move vector-sized to another package as it needs singletons
      and it doesn't compile to android, yet 
- [x] Table: a selection method like actMU but such that when the select
      item is clicked, it get's deselected. (Almost done.)

- [x] Tree-widget the with user suppliable methods for tree-level based 
      behavior (and look and feel). 
- [ ] Other methods to build trees (that is, not just ul-li -consturcts).

- [ ] A selection method like actMU but such that when the select
      item is clicked, it get's deselected. And such that when one item
      or group is selected, we can select a new one with one click that
      at them same time deselects the old one. (Now requires two clicks.)

- [ ] New component: a flow-component.


## Would be nice to have / general

Larger ones:
- provide MathMl-support
- provide svg-support  (Note that there is interesting work done elsewhere wrt svg)

Other things include 
- support for data- * (there are some common functions)
- support for aria- * (some of the most common are provided but there
  is no support for the recommended usage at the moment)
- add more constants so that compiler can help detect typos (in many places)
- use types from other packages where appropriate (e.g. change URLs to URIs,
  network-uri-package, and start using datetime- or chronos-package,
  and possibly some other packages, too)
- add network-uri-static and functions using that interface 

Further ideas
- implement "id-static" so that only defined id's could be referred
  (in addition to the current way of giving id's)
- provide smart constructors for typical usage so that elements will be
  correctly put relative to each other
- provide combinators to achieve similar effect as in the previous item 
- mime types?
- BCP 47?
- there are several other possible ideas or things to be done mentioned 
  in the code

Other
- ~~reorganise lib and example app cabal -structure (so that lib is not
  build twice)~~ (Now using the "project"-setup.)
- ~~make checkable todo-list into the repo~~
- add documentation - some haddock to re-exporting modules etc
- implement proper test cases
- extend current examples and add more examples
- links to w3c or mdn documentation from elements and attributes?


## Other thoughts

The interactive elements can be used but probably it is better idea to use
widgets from 
[Reflex.Dom.Widget.Input](https://github.com/reflex-frp/reflex-dom/tree/develop/reflex-dom-core/src/Reflex/Dom/Widget) 
or 
[Reflex.Dom.Contrib](https://github.com/reflex-frp/reflex-dom-contrib). 

Anyhow, maybe it is worth to extend/add
the functions in the Interactive-module into that direction - and at
the same time trying to avoid repeating things that are introduced in
the above libs. 

Similarly, some of the elements in Embedded-module act like interactive elements. 
Some of those elements probably should use event-mechanism that can return
several different events from the same widget. (E.g. maybe something like
the touch-events do? How to? Or like some of the widgets defined in the
above libs do?) Again, only if not repeating too badly the things defined
in the above libs.

