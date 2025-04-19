# Advent of Code 2015-2024, in Scala

![aoc-logo](aoc.png)

Using

- [Scala-cli](https://scala-cli.virtuslab.org/) with
- [Visual Studio Code](https://code.visualstudio.com/) and
- [Metals](https://marketplace.visualstudio.com/items?itemName=scalameta.metals) extension.

From Scala 3.5.0 onward, `scala` is the same as `scala-cli`.

## Setup

To setup, install Scala, VS Code, Metals, and `git`. Then

```bash
git clone https://github.com/spamegg1/aoc
cd aoc
scala setup-ide .
code .
```

## Libraries

I setup this to be a Scala project using the
[Scala Toolkit](https://docs.scala-lang.org/toolkit/introduction.html).

For example, for the puzzles that require file input / output, I use
[`os-lib`](https://docs.scala-lang.org/toolkit/os-read-file.html)
that comes with the Toolkit.
