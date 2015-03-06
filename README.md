# Syntax Annotations

[![releases](http://img.shields.io/github/release/jkuczm/MathematicaSyntaxAnnotations.svg)](https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases)
[![SemVer 2.0.0](http://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)
[![license MIT](http://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaSyntaxAnnotations/blob/master/LICENSE)
[![Mathematica 8.0 9.0 10.0](http://img.shields.io/badge/Mathematica-8.0_9.0_10.0-brightgreen.svg)](#compatibility)


Annotate syntax elements at box level.


* [Usage examples](#usage-examples)
* [Installation](#installation)
    * [Automatic installation](#automatic-installation)
    * [Manual installation](#manual-installation)
    * [No installation](#no-installation)
* [Compatibility](#compatibility)
* [Bugs and requests](#bugs-and-requests)
* [Contributing](#contributing)
* [License](#license)
* [Versioning](#versioning)



## Usage examples

You can find usage examples in
[answer to "How to convert a notebook cell to a string retaining all formatting, colorization of identifiers etc?" question](http://mathematica.stackexchange.com/a/74105/14303)
on Mathematica Stack Exchange.



## Installation


### Automatic installation

To install SyntaxAnnotations package evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaSyntaxAnnotations/master/BootstrapInstall.m"]
```

Note that this will also install
[ProjectInstaller](https://github.com/lshifr/ProjectInstaller)
package, if you don't have it already installed.

To load SyntaxAnnotations package evaluate: ``Needs["SyntaxAnnotations`"]``.


### Manual installation

1. Download latest released
   [SyntaxAnnotations.zip](https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases/download/v0.1.1/SyntaxAnnotations.zip)
   file.

2. Extract downloaded `SyntaxAnnotations.zip` to any directory which is on
   Mathematica `$Path`, e.g. to one obtained by evaluating
   `FileNameJoin[{$UserBaseDirectory,"Applications"}]`.

3. To load the package evaluate: ``Needs["SyntaxAnnotations`"]``


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import["https://raw.githubusercontent.com/jkuczm/MathematicaSyntaxAnnotations/master/SyntaxAnnotations/SyntaxAnnotations.m"]
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaSyntaxAnnotations/tree/master/SyntaxAnnotations/Tests).
Package is being tested with Mathematica versions 8.0, 9.0 and 10.0 on Linux.
Since it doesn't contain any OS specific code it should work with above
versions on all operating systems.

There's also no obvious reason for package not to work on earlier (6.0+)
versions of Mathematica.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaSyntaxAnnotations/issues).



## Contributing

Feel free to fork and send pull requests.

If you want to use Ant scripts from this repository you will also need to
install [WWBCommon](https://github.com/jkuczm/WWBCommon) project.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaSyntaxAnnotations/blob/master/LICENSE).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
