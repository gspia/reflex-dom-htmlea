
# Package reflex-dom-htmlea

## Introduction

The aim of the lib is to provide a set of functions that allow 
```
 eDiv (className "myWarning" $ id_ "mydiv" $ def) $ do
```
in place of 
```
 elAttr "div" ("class" =: "myWarning" <> "id" =: "mydiv") $ do
```
and prevents writing e.g. `eP ( href (URL "..") $ def) $ do ..`
as p-tag doesn't have href-attribute.

Another aim is to have components with pre-defined functionality.

The lib uses [Reflex.Dom](https://github.com/reflex-frp/reflex-dom) and
similar design choices apply when using it. 

As of the beginning of 2018, lib started to use new 
[project](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)-stuff. 

There are examples:
- example1: basic use
- example2: table-component


### About the naming conventions

- e in front of tags is for "element"
- attributes don't have that kind of prefix
- \' at the end of a tag name is similar as in elAttr' (it calls elAttr' internally)
- D at the end of a tag name means dynamic (it uses elDynAttr internally)
- D\' at the end of a tag name is similar as in elDynAttr'
- N at the end of a tag name means default attributes
- N' is like el'.
- \_ at the end of a tag name means default attributes (that is, empty) 
  and blank content (only a few elements have this defined). 
- "class" attribute function is className in this lib


### Other comments

The interactive elements are listed on a separate module. While 
example1 uses some of those elements, you most likely want to use the 
widgets from [Reflex.Dom](https://github.com/reflex-frp/reflex-dom) 
or [Reflex.Dom.Contrib](https://github.com/reflex-frp/reflex-dom-contrib). 


Lib is probably going to see some re-organization and re-naming etc. Suggestions 
are more than wellcome!


## Usage

First, get the repo with `git clone` and `cd` into the directory, and after that make sure that the reflex-platform is in place:

```
git submodule update --init --recursive
```

To build with GHC, use the nix-shell command to enter the sandbox shell and use cabal (which is supplied by the sandbox):

```
nix-shell -A shells.ghc
cabal new-build all
```

To build with GHCJS:

```
nix-shell -A shells.ghcjs
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

You can also build examples separately by replacing all with exe:name, e.g.

```
cabal new-build exe:tableEx
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build exe:exampleTbl
```

If you want to compile the examples to android, the singletons-package is a
bit problematic at the moment. (This lib may be split because of this.)
If you comment the singletons-package from cabal-files and corresponding 
modules (VS-ending and the examples), then the examples compile to android.


For further information, see the following
- [project-development documentation](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
- [blanket project derivation (default.nix)](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix)
- [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)

Note that if you have already obtained examples but want to update the reflex-platform, you can try, e.g.,

```
git submodule foreach "(git checkout develop; git pull --recurse-submodules)&"
```

(Note that the above command gets the develop-branch of the platform.)



## TODOs

The wish list is quite long at the moment. The list can be seen
at [todo.md](./todo.md).
It includes larger ones that probably take longer time and things that are 
on to-be-done-next list and on would-be-nice-to-have list.



## Caveats

As this lib is in it's infancy, maybe it is better to be open to naming 
convention and re-structuring suggestions etc. At the moment, some of the 
chosen names for attribute combinators are not consistent with regards to 
each other.

Bugs are more than likely to be found, as this is early work
and there are no test cases (except example-programs).



## Related work

See github, there are other libs possibly doing the same or almost the
same thing. (TODO, check the available libs.)

Possible:
- [reflex-html](https://github.com/Saulzar/reflex-html)
- [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui)
- [reflex-material](https://github.com/alasconnect/reflex-material)


