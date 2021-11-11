# OnyxC
A compiler for the Onyx language, written in C with no external dependencies.

See [syntax.md](syntax.md) for example snippets.

Use ```make all``` to build and ```make test``` to run the (very hacked together and temporary) tests.

## Planned features
- [X] C-like syntax
- [X] 'Simulation mode' that executes the intermediate code directly
- [ ] Compilation to x86 and x64 machine code
- [ ] Support for 16-bit x86
- [ ] Inline assembly
- [ ] Floating point math
- [ ] AVX intrinsics
- [ ] A standard library that is as independent from libc as possible

GPL-2.0+ License.
