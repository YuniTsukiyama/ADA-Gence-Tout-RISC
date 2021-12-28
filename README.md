# Ada-gence tout RISC

ADA implementation of a RISC Processor

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

src/        Source Code
tests/      Testsuite