# CIS194 Exercises and examples

See [Course Website for CIS194 (Fall 16 Version)](http://www.seas.upenn.edu/~cis194/fall16/)


## Dependencies

 - [Stack](https://www.haskellstack.org/)


## Running exercises

```
cd w01
stack setup
stack build --pedantic
stack exec w01-exe
```

or via the REPL:

```
cd w01
stack repl
> greenLightMain
> redLightMain
```

If you've changed the source files, then run `:reload` in the repl to get access to updated functions