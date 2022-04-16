# RST-Unparser

[![REUSE status](https://api.reuse.software/badge/github.com/reznikmm/rst-unparser)](https://api.reuse.software/info/github.com/reznikmm/rst-unparser)

> A tool to write a tree of RST elements as a ReStructuredText document.

This project aims for helping rewrite/construct RST documents.
Suppose you need to fix a lot of RST documents in a simple way.
You can parse it and get RST elements corresponding to chapters,
paragraphs, inlines, etc. After changing some of them you need to
write these element as a RST document (unparse). This code does
this for you.

## Install

Get the code and compile it with the command:

    make all install PREFIX=/path/to/install

### Dependencies
It depends on [Matreshka](https://forge.ada-ru.org/matreshka) library.

## Usage

For RST parsing you can use `sphinx-build` tool, that generates XML:

    sphinx-build -M xml rst-directory dist -v -c "sphinx"

RST unparser is a XML `SAX_Content_Handler`, so you just setup it as
a handler for the XML parser. You provide destination of unparsing as
a discriminant. See `examples/unparse/` for details.

Beside this you need a map from internal representation of references
to originals. This is a file contaiing pairs of lines. For example:

```
strongly_typed_language#whatisatype
WhatIsAType
```

This means that RST markup ``:ref:`discrete types <WhatIsAType>` `` is
translated by `sphinx-build` to `strongly_typed_language#whatisatype`
value.

This project provides two binaries: unparse and replace

### Unparse

This executables takes XML and uri maps as arguments and reconstruct
RST document on `stdout`. You can use it to check if unparsing works
correctly. Ideally the result should match the original RST file.

### Replace

This executables takes XML, an uri maps and LibreOffice "flat" ODT file
(`.fodt`). It replaces all text of paragraphs, titles, etc with
corresponding text of ODT document, except code samples.
To recognise end of code samples ODT should contain `###` marker
at the end of each code sample.

## Maintainer
     
[@MaximReznik](https://github.com/reznikmm).
     
## Contribute

[Open an issue](https://github.com/reznikmm/rst-unparser/issues/new)
or submit PRs.

## License

[Apache 2.0](LICENSES/Apache-2.0.txt) © Maxim Reznik
