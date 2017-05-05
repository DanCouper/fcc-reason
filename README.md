# FCC JS Crap in ReasonML

Attempted translating of the JS stuff on FCC to Reason. Given this
is running parallel to learning OCaml, expect extremely bad code.

## Installation

```
yarn
yarn start
```

The initial call should install dependencies - `bs-platform` is the
key, as that is going to compile the code etc.

The`.bsconfig` file specifies the Bucklescript config for the project. See
http://bloomberg.github.io/bucklescript/docson/#build-schema.json for
an overview of options.

The call to `yarn start` will kick off a watch server.

## Dev env

[JS workflow is here](http://facebook.github.io/reason/jsWorkflow.html) -
I use Atom and have a Spacemacs-like setup, and Nuclide screwed that up,
so I installed everything seperately which is a pain, but need to
just keep following links for each package & installing relevant
opam dependencies for each one and it seems to work in the end.

## Useful links

- [Comparison of OCaml and Reason, helpful when translating OCaml guides to work with Reason](https://facebook.github.io/reason/mlCompared.html#reason-and-ocaml-various-differences)
- [The Bucklescript Js module API](http://bloomberg.github.io/bucklescript/api/Js.html)
