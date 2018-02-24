# OpraDB

OpraDB is graph database that uses [OPRA query language](https://arxiv.org/abs/1710.04419) and is written in [F#](http://fsharp.org/) with [dotnet core](https://www.microsoft.com/net/learn/get-started/linuxubuntu).
Main purpose of this project is to show expressive power of [OPRA](https://arxiv.org/pdf/1710.04419.pdf) queries.

This is a work in progress.

## How to build (Linux)

* Install [dotnet core](https://www.microsoft.com/net/learn/get-started/linuxubuntu)
* `git clone https://github.com/mateuszlewko/OpraDB.git`
* `cd OpraDB/src`
* `dotnet build`

    Run main project with (assuming you're in `src` folder):
    `dotnet run --no-build`

    Run tests with (assuming you're in `test` folder):
    `dotnet run --no-build`

## How to edit in [Visual Studio Code](https://code.visualstudio.com/)

* Install [mono](http://www.mono-project.com/download/)
* Install [F#](http://fsharp.org/use/linux/)
* Add [Ionide](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) plugin to VS Code:  
  * launch VS Code Quick Open (Ctrl+P)
  * paste: `ext install Ionide.Ionide-fsharp`
