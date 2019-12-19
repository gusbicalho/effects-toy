# effects-toy

This repo includes implementations of a "Hello, world!" HTTP server in several
extensible effects libraries. Currently, there are implementations using:

* fused-effects
* eff

The task itself was based in [this tutorial](https://blog.sumtypeofway.com/posts/serving-http-content-with-fused-effects.html).

# Running

You can run both implementations using:

```bash
stack run toy-eff # to run the server
./ghci toy-eff # start a nice ghci repl

# or

stack run toy-fused-effects # to run the server
./ghci toy-fused-effects # start a nice ghci repl
```

Because `eff` and `fused-effects` use the same module names, we cannot load
both at the same time :(
