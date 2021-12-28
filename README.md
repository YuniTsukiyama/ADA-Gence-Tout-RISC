# Ada-Gence Tout RISC (AGTR)

ADA implementation of a 16bit RISC processor emulator.

## Requirements

- gcc-ada
- gprbuild

## Build process

Release:

```
mkdir obj_release
gprbuild -XBuild=Release
```

Debug:

```
mkdir obj_debug
gprbuild -XBuild=Debug
```

## Project content

doc/        Documentation
src/        Source Code
tests/      Testsuite