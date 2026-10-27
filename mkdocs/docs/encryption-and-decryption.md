# Encryption and Decryption

    cpdf -encrypt <method> [-pw=]<owner> [-pw=]<user>  
    [-no-encrypt-metadata] <permissions> in.pdf -o out.pdf

    cpdf -decrypt [-decrypt-force] in.pdf owner=<owner password> -o out.pdf

## Introduction

PDF files can be encrypted using various types of encryption and
attaching various permissions describing what someone can do with a
particular document (for instance, printing it or extracting content).
There are two types of person:

The **User** can do to the document what is allowed in the permissions.

The **Owner** can do anything, including altering the permissions or
removing encryption entirely.

There are five kinds of encryption:

- 40-bit encryption (method `40bit`) in Acrobat 3 (PDF 1.1) and above --
  *this is insecure -- do not use for new documents*

- 128-bit encryption (method `128bit`) in Acrobat 5 (PDF 1.4) and above
  -- *this is insecure -- do not use for new documents*

- 128-bit AES encryption (method `AES`) in Acrobat 7 (PDF 1.6) and above

- 256-bit AES encryption (method `AES256`) in Acrobat 9 (PDF 1.7) --
  *this is deprecated -- do not use for new documents*

- 256-bit AES encryption (method `AES256ISO`) in Acrobat 9 (PDF 1.7) and
  later. Also suitable for PDF 2.0.

All encryption supports these kinds of permissions:

    -no-edit    Cannot change the document
    -no-print   Cannot print the document
    -no-copy    Cannot select or copy text or graphics
    -no-annot   Cannot add or change form fields or annotations

In addition, 128-bit encryption (Acrobat 5 and above) and AES encryption
supports these:

    -no-forms      Cannot edit form fields           
    -no-extract    Cannot extract text or graphics   
    -no-assemble   Cannot merge files etc.           
    -no-hq-print   Cannot print high-quality         

Add these options to the command line to prevent each operation.

*Note: Adobe Acrobat and Adobe Reader may show slightly different
permissions in info dialogues -- this is a result of policy changes and
not a bug in Cpdf. You may need to experiment.*

## Encrypting a Document

To encrypt a document, the owner and user passwords must be given (here,
`fred` and `charles` respectively):

`cpdf -encrypt AES fred charles -no-print in.pdf -o out.pdf`

`cpdf -encrypt AES fred charles -no-extract in.pdf -o out.pdf`

`cpdf -encrypt AES256ISO fred "" -no-edit -no-copy in.pdf -o out.pdf`

A blank user password is common. In this event, PDF viewers will
typically not prompt for a password for when opening the file or for
operations allowable with the user password.

`cpdf -encrypt AES256ISO fred "" -no-forms in.pdf -o out.pdf`

In addition, the usual method can be used to give the existing owner
password, if the document is already encrypted.

The optional `-pw=` preface may be given where a password might begin
with a `-` and thus be confused with a command line option.

When using AES encryption, the option is available to refrain from
encrypting the metadata. Add `-no-encrypt-metadata` to the command line.

## Decrypting a Document

To decrypt a document, the owner password is provided.

`cpdf -decrypt in.pdf owner=fred -o out.pdf`

The user password cannot decrypt a file.

When appropriate passwords are not available, the option
`-decrypt-force` may be added to the command line to process the file
regardless.
