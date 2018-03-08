# Getting Started with Unison

Unison is a simple, flexible, and potentially optimal software tool that
performs register allocation and instruction scheduling in integration using
combinatorial optimization.

## Prerequisites

Unison has the following dependencies:
[Haskell platform](http://hackage.haskell.org/platform/),
[Qt](https://www.qt.io/) (version 4.x),
[Graphviz library](http://www.graphviz.org/), and
[Gecode](http://www.gecode.org/) (version 6.0.0).
To get the first three dependencies in Debian-based distributions, just run:

```
apt-get install haskell-platform libqt4-dev libgraphviz-dev
```

The source of Gecode can be fetched with:

```
wget https://github.com/Gecode/gecode/archive/release-6.0.0.tar.gz
```

## Building

Just go to the `src` directory and run:

```
make build
```

## Testing

Unison contains a test suite with a few functions where different targets and
optimization goals are exercised. To execute the tests just run:

```
make test
```

## Installing

The building process generates three binaries. The installation process consists
in copying the binaries into the appropriate system directory. To install the
binaries under the default directory `usr/local` just run:

```
make install
```

The installation directory is specified by the Makefile variable `PREFIX`. To
install the binaries under an alternative directory `$DIR` just run:

```
make install PREFIX=$DIR
```

## Running

Unison can be run as a standalone tool but is only really useful as a complement
to a full-fledged compiler such as [LLVM](http://llvm.org/). [Our LLVM
fork](https://github.com/unison-code/llvm) includes a Unison driver built on top
of LLVM's `llc` code generator. To try it out, just clone the LLVM fork and
follow the instructions in the `README.md` file from any of the branches with a
`-unison` suffix.

## Contact

[Roberto Castañeda Lozano](https://www.sics.se/~rcas/) [<rcas@sics.se>]

## License

Unison is licensed under the BSD3 license, see the [LICENSE.md](LICENSE.md) file
for details.

## Further Reading

Check [the Unison website](https://unison-code.github.io/).