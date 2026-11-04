Prerequisites to build
----------------------

The OCaml compiler

[https://ocaml.org](https://ocaml.org)

The camlpdf library

[https://github.com/johnwhitington/camlpdf](https://www.github.com/johnwhitington/camlpdf)

The ocamlfind library manager

[https://projects.camlcity.org/projects/findlib.html](https://projects.camlcity.org/projects/findlib.html)

If you are using the OPAM package manager:

[https://opam.ocaml.org](http://opam.ocaml.org)

these can be installed by:

```
opam install camlpdf
```

To build
--------

If downloading from Github, obtain the correct source. This means choosing the
tag for a particular version, such as "v2.9.3". The head of the master branch
is unstable.

1. Type `make` to make the cpdf executable and the library

2. Type `make install` to install the OCaml cpdf library.

The `cpdf` executable should be manually placed somewhere suitable.

Alternatively, the library and command-line binary can be installed by

```
opam install cpdf
```
