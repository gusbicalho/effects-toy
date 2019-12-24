# effects-toy

This repo includes implementations of a "Hello, world!" HTTP server in several
extensible effects libraries. Currently, there are implementations using:

* fused-effects
* eff
* polysemy

The task itself was based in [this tutorial](https://blog.sumtypeofway.com/posts/serving-http-content-with-fused-effects.html).

# Running

You can run any implementation using:

```bash
stack run <package-name> # to run the server
./ghci <package-name> # start a nice ghci repl

# for example

stack run toy-fused-effects
stack run toy-eff
stack run toy-polysemy
```

Because `eff` and `fused-effects` use the same module names, we cannot load
both at the same time :(

# Development

This repo has a nice setup fot developing with VSCode and the
[ghcide extension](https://marketplace.visualstudio.com/items?itemName=DigitalAssetHoldingsLLC.ghcide).

After installing the extension above, you need to build `ghcide`:

```bash
stack build ghcide --copy-compiler-tool
```
