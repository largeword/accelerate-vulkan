# Accelerate Vulkan
This is a Vulkan backend for the new pipeline of [Accelerate](https://github.com/ivogabe/accelerate/tree/new-pipeline).

## Requirements
This project requires specific software to run:
- GHC: 9.2
- LLVM: 15.0
- CUDA: 11.8
- Vulkan: 1.3
- Cabal: 3.10

They are supposed to be installed and configured before running this project.

## Execution
[app/Main.hs](https://github.com/largeword/accelerate-vulkan/blob/main/app/Main.hs) shows how to use the Vulkan backend to write the [histogram example](https://hackage.haskell.org/package/accelerate/docs/Data-Array-Accelerate.html#g:28).

One can use `cabal build` and `cabal run` to compile and run the example.

## Evaluation
Benchmarks and original results are putted in the forlder [benchmark](https://github.com/largeword/accelerate-vulkan/tree/main/benchmark). Each test can be run by `cabal build` and `cabal run`.

Errors related to the MVar operation might show up during the execution, user can re-run the program until having an error-free run.

For the PTX backend, benchmarks are stored in the forlder [benchmark/PTX_backend](https://github.com/largeword/accelerate-vulkan/tree/main/benchmark/PTX_backend). Note that before compiling the benchmark MG, one should delete the local cache folder `~/.cache/accelerate` for Accelerate.
