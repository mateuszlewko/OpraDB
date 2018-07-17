# OpraDB

OpraDB is graph database that uses [OPRA query language](https://arxiv.org/abs/1710.04419) and is written in [F#](http://fsharp.org/).
Main purpose of this project is to show the expressive power of [OPRA](https://arxiv.org/pdf/1710.04419.pdf) queries.

Ability to express complex path properties in a modular
way and with a minimum effort, is where this query language really excels.

**Note:** This is a work in progress, although usable version is already available.

## Table of Contents

- [OpraDB](#opradb)
    - [Table of Contents](#table-of-contents)
    - [Quick start with Docker (Recommended)](#quick-start-with-docker-recommended)
    - [How to build from source (Linux)](#how-to-build-from-source-linux)
    - [How to edit in Visual Studio Code](#how-to-edit-in-visual-studio-code)
    - [Examples](#examples)
        - [Basics](#basics)
            - [Graph](#graph)
            - [Syntax](#syntax)
        - [Constraints](#constraints)
            - [Path constraints](#path-constraints)
            - [Node and regular constraints](#node-and-regular-constraints)
            - [Arithmetic constraints](#arithmetic-constraints)
        - [Let expression (Ontologies)](#let-expression-ontologies)
        - [Handling cycles](#handling-cycles)
        - [Comparison with Gremlin (Apache TinkerPop)](#comparison-with-gremlin-apache-tinkerpop)
        - [Future work](#future-work)
            - [TODO list](#todo-list)

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

- Install [dotnet core](https://www.microsoft.com/net/learn/get-started/linuxubuntu)
- `git clone https://github.com/mateuszlewko/OpraDB.git`
- `cd OpraDB/src`
- `dotnet build`

    Run main project with (assuming you're in `src` folder):
    `dotnet run --no-build`

    Run tests with (assuming you're in `test` folder):
    `dotnet run --no-build`

## How to edit in [Visual Studio Code](https://code.visualstudio.com/)

- Install [mono](http://www.mono-project.com/download/)
- Install [F#](http://fsharp.org/use/linux/)
- Add [Ionide](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp) plugin to VS Code:  
  - launch VS Code Quick Open (Ctrl+P)
  - paste: `ext install Ionide.Ionide-fsharp`

## Examples

Read this sections to get a brief understanding of how to write queries in [Opra QL](https://arxiv.org/pdf/1710.04419.pdf).

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

* List of matched nodes (`MATCH NODES`):

    Example:

    ```cypher
    MATCH NODES a, b, someNode, property(someNode), name(someNode)
    ...
    ```
    By specifying node or node's property in matched nodes, we're essentially 
    defining an alias to this node, which we can use in latter constraints.

    Usually we want to return node's property like `name(x)`. Providing just an
    alias `x` will return ids of all nodes that matched `x`'s constraints.

* `MATCH ... PATHS`:
    Behaviour for matching paths is similar to nodes, however current versions
    doesn't return paths.

### Constraints

#### Path constraints

```cypher
path1 : beginNode -> endNode
```

Path constraints are here to specify that there is path called `path1` from node
`beginNode` to `endNode`. Both nodes could be bound earlier in *matched nodes list*,
but they don't have to be. In case a node wasn't bound earlier, it will be existentially quantified (meaning that is just needs to exists). This rule applies for all occurrences of nodes or paths not bound in `MATCH ...` list.

Multiple path constraints can be specified in query by separating them with comma.

Example:

```cypher
MATCH NODES name(a), name(b), name(c)
SUCH THAT p: a -> b, q: a -> c
```

#### Node and regular constraints

Regular constraint specifies how correct paths should look like. It consists of 
node constraints and operators: `|`, `*`. Each node constraint describes one of the nodes on a given path.

Example node constrains:

- `(access_level(people) > 3 OR position(people) = "CTO")` this constraint will be     true if *current* node on path `people` has property `access_level` that is 
  greater
  than 3 or property `position` is "CTO"

*Current node* is currently checked node when traversing a path.

- `(distance(cities, 'cities) < 10)` Tick before node name (`'node`) is current node's 
  successor when traversing a path. Node will satisfy this constraint, if outgoing edge (to successor) has property `distance` and its values is less than 10.

Regular constraint can just be a sequence of node constraints, like this:

```cypher
(name(friends) = "Bol")(name(friends) = "Alice")(name(friends) = "Mateusz")
```

Path `friends` satisfies this constraint as long as it contains three nodes which 
are connected by some edges, and `name` of first node is *Bob*, second *Alice* and 
third *Mateusz*.

However, regular constraints can act as regular expression on paths, where instead 
of letters and digits we have *node constraints*. This is where we can make use of 
union `|` and Kleene-star `*` operators.

#### Arithmetic constraints

### Let expression (Ontologies)

### Handling cycles

### Comparison with  Gremlin (Apache TinkerPop)

### Future work

#### TODO list

- Returning paths 
- Finding shortest paths
- Time and memory optimizations

If you have any thoughts or request, feel free to create an issue or add a pull request.