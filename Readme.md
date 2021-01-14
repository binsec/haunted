# Binsec/Haunted: a binary-level analyzer to detect Spectre attacks.
Binsec/Haunted is a binary-analysis tool for speculative
constant-time, that can find Spectre-PHT (a.k.a. Spectre-v1) and
Spectre-STL (a.k.a. Spectre-v4) vulnerabilities.

It is an extension of the binary analysis plateform
[Binsec](https://github.com/binsec/binsec), and in particular, it
builds on its module for relational symbolic execution,
[Binsec/Rel](https://github.com/binsec/Rel).

If you are interested, you can read the
[paper](https://binsec.github.io/assets/publications/papers/2021-ndss.pdf),
published at NDSS 2021.

Benchmarks to test Binsec/Haunted: https://github.com/binsec/haunted_bench

## Installation
### Docker
Coming soon.
<!-- ### Docker -->
<!-- The docker contains necessary files for running Binsec/Haunted and the benchmarks to test it. -->

<!-- 1. Download the [image](todo). -->

<!-- 2. Import the image: -->
<!-- ``` -->
<!-- docker load < binsec-haunted.tar -->
<!-- ``` -->

<!-- 3. Run the container: -->
<!-- ``` -->
<!-- docker run -it binsec-haunted /bin/bash -->
<!-- ``` -->

<!-- 4. Run `./update.sh` to get the latest version of Binsec/Haunted. -->

<!-- You are ready to go ! (Read https://github.com/binsec/haunted_bench for examples on how to use Binsec/Haunted). -->

### From sources
**Requirements**: boolector (recommended boolector-3.2.0), z3, yices or cvc4.

``` bash
# Install Ocaml and prerequisite packages for BINSEC via OPAM
sudo apt update
sudo apt install ocaml ocaml-native-compilers camlp4-extra opam protobuf-compiler libgmp-dev libzmq3-dev llvm-6.0-dev cmake pkg-config
opam init
opam switch 4.05.0
opam install menhir ocamlgraph piqi zarith zmq.5.0.0 llvm.6.0.0 oUnit

# Additional packages (optional)
# opam install merlin ocp-indent caml-mode tuareg ocamlfind

# Checkout source code
git clone https://github.com/binsec/haunted.git binsec-haunted

# Compile source code
cd binsec-haunted
autoconf
./configure
cd src
make depend
make binsec
```

Print the help:
``` bash
$ binsec --help
```

## Source code
Useful source code is located under `src/relse/`.
