# Syntax Annotations

[![releases](https://img.shields.io/github/release/jkuczm/MathematicaSyntaxAnnotations.svg)](https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases)
[![Mathematica 8.0 - 11.0](https://img.shields.io/badge/Mathematica-8.0_--_11.0-brightgreen.svg)](#compatibility)
[![license MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jkuczm/MathematicaSyntaxAnnotations/blob/master/LICENSE)
[![SemVer 2.0.0](https://img.shields.io/badge/SemVer-2.0.0-brightgreen.svg)](http://semver.org/spec/v2.0.0.html)


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

To install newest version of SyntaxAnnotations package,
in *Mathematica* version 10 or newer, evaluate following code:
```Mathematica
PacletInstall@"http://github.com/jkuczm/MathematicaSyntaxAnnotations/releases/download/v0.2.2/SyntaxAnnotations-0.2.2.paclet"
```

Note that above requires allowing *Mathematica* to use the Internet.

To load SyntaxAnnotations package evaluate:
```Mathematica
Needs@"SyntaxAnnotations`"
```

To uninstall SyntaxAnnotations package evaluate:
```Mathematica
PacletUninstall@"SyntaxAnnotations"
```


### Manual installation

If in your setup *Mathematica* doesn't have Internet access,
or you're using version older than 10, download
[SyntaxAnnotations.0.2.2.paclet](https://github.com/jkuczm/MathematicaSyntaxAnnotations/releases/download/v0.2.2/SyntaxAnnotations-0.2.2.paclet)
file and evaluate `PacletInstall` with path to downloaded file:
```Mathematica
PacletInstall@"path/to/downloaded/SyntaxAnnotations.0.2.2.paclet"
```

To load SyntaxAnnotations package evaluate:
```Mathematica
Needs@"SyntaxAnnotations`"
```

To uninstall SyntaxAnnotations package evaluate:
```Mathematica
PacletUninstall@"SyntaxAnnotations"
```


### No installation

To use package directly from the Web, without installation, evaluate:
```Mathematica
Import@"https://raw.githubusercontent.com/jkuczm/MathematicaSyntaxAnnotations/master/SyntaxAnnotations/SyntaxAnnotations.m"
```



## Compatibility

This package contains extensive
[automatic test suite](https://github.com/jkuczm/MathematicaSyntaxAnnotations/tree/master/SyntaxAnnotations/Tests).
Package is tested with all *Mathematica* major and minor versions from 8.0 to
11.0 on Linux. Since it doesn't contain any OS specific code it should work
with above versions on all operating systems.

There's also no obvious reason for package not to work on older (6.0+)
and newer (11.1+) versions of *Mathematica*,
but it was not tested with these versions.



## Bugs and requests

If you find any bugs or have feature request please create an
[issue on GitHub](https://github.com/jkuczm/MathematicaSyntaxAnnotations/issues).



## Contributing

Feel free to fork and send pull requests.

All contributions are welcome!



## License

This package is released under
[The MIT License](https://github.com/jkuczm/MathematicaSyntaxAnnotations/blob/master/LICENSE).



## Versioning

Releases of this package will be numbered using
[Semantic Versioning guidelines](http://semver.org/).
