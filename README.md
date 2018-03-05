# vincenty
[Vincenty's formulae](https://en.wikipedia.org/wiki/Vincenty's_formulae) implementation in Erlang

# usage
$ make

# test
$ make test

# typecheck
$ make typecheck

# example
$ ./rebar3 shell
```Erlang
1> vincenty:distance({0.0, 0.0}, {0.0, 0.0}).
0.0
2> vincenty:distance({42.3541165, -71.0693514}, {40.7791472, -73.9680804}).
298.396186
```

