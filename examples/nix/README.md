# Nix Based Example

This example shows you how you can use Nix to create closures that contain
all the required dependencies.

The trick to using Shh with Nix, is that you need to load your functions in
using `Absolute` `ExecReferencing`. This ensures the store path appears in
the resulting binary, and means that the dependencies are part of the closure.

This makes deploying scripts incredibly easy!

## Usage

To get the dependencies injected, simply add your dependencies to the
`build-tools` section in your cabal file. Use the attribute name as would
be found in `nixpkgs`, e.g. `perl`, or `vim`.

Next, load your executables into your program

```
$(load Absolute ["xxd", "ls"])
```

In most cases, this will just work, and you can call `cabal2nix .` on your
project, or use the `nixpgks` `callCabal2nix` function to build your project.

The one issue that can occur is if your system dependencies have the same name
as a package on hackage. In this case, you will need to specify a `default.nix`
which passes in the correct thing.
