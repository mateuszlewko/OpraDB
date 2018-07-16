# OpraDB

OpraDB is graph database that uses [OPRA query language](https://arxiv.org/abs/1710.04419) and is written in [F#](http://fsharp.org/) with [dotnet core](https://www.microsoft.com/net/learn/get-started/linuxubuntu).
Main purpose of this project is to show expressive power of [OPRA](https://arxiv.org/pdf/1710.04419.pdf) queries.

This is a work in progress.

## Quick start with Docker (Recommended)

1. Install [docker](TODO: link to docker)
2. Get container image:

   * (Option A) Download latest image

       ```bash
       TODO: Download container image
       ```

   * (Option B) You can also build container image from local sources

       * First, clone repository:

           ```bash
           git clone ... TODO: Clone repo
           ```

3. Run container with OpraDB client and loaded database:
    ```bash
    TODO: docker run
    ```

## How to build from source (Linux)

TODO: Build section

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

## Examples

### Basics

#### Graph

Let's start by defining OpraDB graph. Graph consists of directed edges and nodes.
Each node and edge can have a set of properties. Property has a:

* key `string`
* value of one of the following types: `string`, `int`, `bool`, `float`.

Undirected graphs can be represented by adding two directed edges for each undirected edge,
in both directions.

Example graph in `json` format:

```json
{
    "nodes": [
        { "_id": 1, "crowd": 2, "start": 1 }
      , { "_id": 2, "crowd": 10 }
    ],
    "directed_edges": [
        { "_from": 1, "_to": 2, "dist": 3 }
      , { "_from": 2, "_to": 1, "dist": 3 }
    ]
}
```

**Note:** All nodes must have field `_id` with *positive* integer value. Edges
must have `_from` and `_to` properties, which point to node ids *already defined*
in `nodes` object (see example above).

#### Syntax

Query has following syntax:

```cypher
MATCH NODES <list of matched (returned) nodes or node properties>  
      PATHS <list of matched paths>
      SUCH THAT <path constraints>
      WHERE <regular constraints>
      HAVING <arithmetic constraints>
```

`MATCH`, `NODES`, `PATHS`, `SUCH THAT`, `WHERE` and `HAVING` are most used
keywords and they all must be uppercase. When creating a query you should always
specify at least one matched node or node property.

**Note:** Returning matched paths is currently **not supported**.

### Constraints

#### Node and regular constraints

#### Arithmetic constraints

### Handling cycles

### Comparison with  Gremlin (Apache TinkerPop)
