
# Package reflex-dom-htmlea

## Introduction

This library provides fucntions that help to avoid non-conforming
htlm and writing typos leading to runtime errors. Library provides
a reasonable coverage of elements and attributes.

So you can write

```
 eDiv (className "myWarning" $ id_ "mydiv" $ def) $ do
```
in place of 
```
 elAttr "div" ("class" =: "myWarning" <> "id" =: "mydiv") $ do
```

You can also mix both styles. See examples.


Library prevents writing e.g. `eP ( href (URL "..") $ def) $ do ..`
as p-tag doesn't have href-attribute.

Further, "type"- and "value"-attribute overloading (in html) is 
splitted up to individual data-types. Hopefully this leads to less typos
and runtime debugging.

This library/package is easy to use and the structure is simple.


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
  Only br has this atm. Is there need for other elements?
- "class" attribute function is className in this lib


### Other comments

The interactive elements are listed on a separate module. While the 
example uses some of those elements, you most likely want to use the 
widgets from [Reflex.Dom](https://github.com/reflex-frp/reflex-dom) 
or [Reflex.Dom.Contrib](https://github.com/reflex-frp/reflex-dom-contrib). 


Lib is probably going to see some re-organization and re-naming etc. Suggestions 
are more than wellcome!


## Usage

To build this library locally use reflex-platform's work-on script as follows:

```
<path-to-platform>/reflex-platform/work-on ghc ./reflex-dom-htmlea.nix
```

This puts you into a nix shell that has GHC and the environment it needs. Then you can build with:

```
cabal new-configure 
cabal new-build all
```

In this shell, ghcid can be used:
```
ghcid examples/app-wai/Main.hs
```
or even better, the auto-reloading can be used
```
./dev-server.sh
```
and then you can point your browser at localhost:8000.
If you want to copy the executables to, say mydir, you can
```
find . -path "./dist*" -executable -type f -exec cp '{}' mydir/ \;
```
If you do copying and if you want to see the fig, copy the figs-dir too.
The kit-executable is using webkit2gtk and wai-executable start warp-server 
(at localhost:8000).

At the moment (Oct 2017) this library cannot initialize work-on-shell with 
ghcjs. (This is probably due to some minor thing.)

You can go to nix-shell directly, then start editor. Tools like hlint 
are available (see the tool list in default.nix).

You can go to nix-shell also with
```
nix-shell --argstr "compiler" "ghcjs"
```
so that you can type
```
cabal new-configure --ghcjs
cabal new-build all
```
for a semi-fast build-cycle. The build results can be found in
`./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/htmlLeaExs-0.1.1.1/c/htmlLeaEx/build/htmlLeaEx/htmlLeaEx.jsexe` 
-directory. 
Note that in order to see the fig in the example, you have to manually copy 
it into the `...htmlLeaEx.jsexe/figs`-directory.


To build in NixOs, just type nix-build. It assumes ghcjs and builds accordingly.
(At the moment this doesn't build the example app. You have to do it in one of the above ways.)

If you want to use this in your own modules, you have to add this package
to the list of packages so that nix can find it. Assuming git cloning, 
the following
```
haskellPackageOverrides = self: super: {
  # nix-env -f "<nixpkgs>" -iA haskellPackages.reflex-dom-htmlea
  ### reflex-dom-htmlea = self.callPackage <absolute path>/reflex-dom-htmlea {};
  reflex-dom-htmlea = self.callPackage ~/git/reflex-dom-htmlea {};
};
```
could find its way into `~/.config/nixpkgs/config.nix`. 
My nix-setup is somewhat vaguely described in 
[nixos-adventure](https://github.com/gspia/half-baked/tree/master/hb8-nixos-adventure) 
notes. For another example, see [7guis-relfex-nix]().


## About nix-files and their organization

The default.nix assumes ghcjs and is used by nix-build.

The shell.nix assumes ghc and is used by nix-shell. Hlint works here so
that editor usage is fluent.

The reflex-dom-htmlea.nix sets compiler-default to ghc so that
if we start work-on-script from reflex-platform and point it to
reflex-dom-htmlea.nix, then the started environment can be used to
build the webkit-application. Also, ghicd works in this environment.

(This is not yet ideal.)


## TODOs

The wish list is quite long at the moment.

Larger ones:
- provide MathMl-support
- provide svg-support

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
- reorganise lib and example app cabal -structure (so that lib is not
  build twice)
- make checkable todo-list into the repo
- add documentation - some haddock to re-exporting modules etc
- implement proper test cases
- extend current example
- or add more examples
- links to w3c or mdn documentation from elements and attributes?

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


## Caveats

As this lib is in it's infancy, maybe it is better to be open to naming 
convention and re-structuring suggestions etc. At the moment, some of the 
chosen names for attribute combinators are not consistent with regards to 
each other.

Bugs are more than likely to be found, as this is the initial version
and there is no test cases.




## Related work

See github, there are other libs possibly doing the same or almost the
same thing. (TODO, check the available libs.)

Possible:
- [reflex-html](https://github.com/Saulzar/reflex-html)
- [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui)
- [reflex-material](https://github.com/alasconnect/reflex-material)

Even though the two latter are probably contain much more something else.

