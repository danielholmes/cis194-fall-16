# CIS194 Exercises and examples

See [Course Website for CIS194 (Fall 16 Version)](http://www.seas.upenn.edu/~cis194/fall16/)


## Dependencies

 - [Stack](https://www.haskellstack.org/)


## Setup

Each week is managed separately. Before using that week's code run:

```
cd w01
stack setup
```


## Running exercises

Within a week directory after setup:

```
stack build --pedantic
stack exec w01-exe
```

or via the REPL:

```
stack repl
> greenLightMain
> redLightMain
```

If you've changed the source files, then run `:reload` in the repl to get access to updated functions


## Running tests

Within a week directory after setup:

```
stack test
```

Note that the return code from the execution will be success even if there are failures. TODO: Find a solution