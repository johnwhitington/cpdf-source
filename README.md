cpdf-source
===========

Free for non-commercial use. See LICENSE for details. Copyright Coherent
Graphics Ltd 2013.

This code builds two things:

a) The CPDF PDF Command Line Tools, which are a popular commercial command line
tool for editing PDF files.

b) cpdf as an OCaml library.

Prerequisites to build
----------------------

The OCaml compiler

[http://ocaml.org](http://ocaml.org)

The camlpdf library

[http://github.com/johnwhitington/camlpdf](http://www.github.com/johnwhitington/camlpdf)

The ocamlfind library manager

[http://projects.camlcity.org/projects/findlib.html](http://projects.camlcity.org/projects/findlib.html)

If you are using the OPAM package manager:

[http://opam.ocamlpro.com](http://opam.ocamlpro.com)

these can be installed by:

```
opam install camlpdf
```

To build
--------

If downloading from Github, obtain the correct source. This means choosing the tag for a
particaular version, such as "v2.1.1". The head of the master branch is
unstable.

1. Type `make` to make the cpdf executable and the library

2. Type `make install` to install the OCaml cpdf library.

The `cpdf` executable should be manually placed somewhere suitable.

Alternatively, the library and command-line binary can be installed by

```
opam install cpdf
```


Documentation
-------------

The file `cpdfmanual.pdf` in this folder is the documentation for the command
line tools. The documentation for the cpdf OCaml library is in `doc/html/cpdf/`
in this folder once the library has been built.

Contact
-------

[contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk)
