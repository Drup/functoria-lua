Functoria-lua
--------------

[Lua-ml](https://github.com/lindig/lua-ml) is a modular Lua interpreter written
in OCaml. "Modular" here means that every part of the interpreter is expressed
as a functor parametrized over its dependencies. It allows users to
add new primitives and replace parts as desired.

[Functoria](https://github.com/mirage/functoria) is a library to create DSL
manipulating such functor-based libraries. It was originally created for
[mirageOS](https://mirage.io/) (See [this blog post](https://mirage.io/blog/introducing-functoria) for more details).

functoria-lua uses functoria to define a set of combinators to build
customized lua intepretors with lua-ml. See the [example](example) directory.

## Install

You will need the following things:
- A version of functoria that support projections: https://github.com/mirage/functoria/pull/150
- An opamified version of lua-ml: https://github.com/lindig/lua-ml/pull/3

Then you can pin this repository:

```
opam pin add functoria-lua "https://github.com/Drup/functoria-lua.git"
```

To try out the example:

```
cd example/
functoria-lua config
opam install --deps .
functoria-lua build
```

---------

![Graph of the example](https://i.imgur.com/HmPyofZ.png)
