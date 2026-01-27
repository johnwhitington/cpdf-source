Coherent PDF Command Line Tools
===============================

(If you are looking for binaries, there are here: https://www.github.com/coherentgraphics/cpdf-binaries )

CPDF is distributed under the AGPL - see LICENSE.md. If you are unable to abide
by the terms of the AGPL, you will need a commercial license.

See also original copyright and/or license statements in cpdfxmlm.ml*,
cpdfyojson.ml*, cpdfua.ml, and cpdfunicodedata.*

For commercial licenses, email
[contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk) or
visit [https://www.coherentpdf.com/](https://www.coherentpdf.com/)

Copyright Coherent Graphics Ltd 2025.

Keep up to date by joining the very low volume cpdf-announce mailin list:

https://groups.google.com/g/cpdf-announce/

This code builds two things:

a) The CPDF PDF command line tool for editing PDF files.

b) cpdf as an OCaml library, for which documentation is here:

[https://coherentpdf.com/cpdf/](https://coherentpdf.com/cpdf/)

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

If downloading from Github, obtain the correct source. This means choosing the
tag for a particular version, such as "v2.8.1". The head of the master branch
is unstable.

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

C API
-----

A C interface to cpdf is available, in source and binary form:

[https://github.com/johnwhitington/cpdflib-source](https://github.com/johnwhitington/cpdflib-source)

[https://github.com/coherentgraphics/cpdflib-binary](https://github.com/coherentgraphics/cpdflib-binary)

Python API
----------

A Python interface to cpdf is available:

[https://pypi.org/project/pycpdflib](https://pypi.org/project/pycpdflib)

Java API
--------

A Java interface to cpdf is available:

[https://github.com/coherentgraphics/jcpdf](https://github.com/coherentgraphics/jcpdf)

.NET API
--------

A .NET interface to cpdf is available:

[https://github.com/coherentgraphics/dotnet-libcpdf](https://github.com/coherentgraphics/dotnet-libcpdf)

JavaScript API
--------------

A JavaScript version of cpdf, for server and client side is available:

Distribution: [https://www.npmjs.com/package/coherentpdf](https://www.npmjs.com/package/coherentpdf)

Source: [https://github.com/coherentgraphics/coherentpdf.js](https://github.com/coherentgraphics/coherentpdf.js)

Acknowledgments
---------------

The file [cpdfxmlm.ml](cpdfxmlm.ml) was written by Daniel Bünzli.

The file [cpdfyojson.ml](cpdfyojson.ml) was written by Martin Jambon and others.

Unicode Data Files are under the Unicode Inc. License agreement.

The error descriptions for -verify "PDF/UA-1(matterhorn)" come from the PDF
Association, and are licensed under the Creative Commons Attribution 4.0
International license.

PDF/UA support in Cpdf was implemented under NLnet grant 2023-12-031.

Contact
-------

[contact@coherentgraphics.co.uk](mailto:contact@coherentgraphics.co.uk)
