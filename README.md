# Ÿauhau

Ÿauhau is a compiler plugin for the parallelizing compiler
[ohua](https://github.com/ohua-dev/ohua-core) which improves programs
by automatically batching I/O calls and running them concurrently.

> Concise code and efficient IO, get it for free with data flow.

## Repository structure 


- `src` contains the sources for the plugin itself
- `jvm` contains JVM specific runtime operators and helper data
  structures
- `examples` contains examples of how yauhau can be used on the
  particular target platforms of ohua. See the [README](/examples/README.md) in the
  folder for more details.
