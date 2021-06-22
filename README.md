# OnyxC
A compiler for the Onyx language, written in C with no external dependencies.

OnyxC will be able to output 16, 32 and 64 bit ELF (16 bit will be real mode only for use in bootloaders and such).

It will also support outputting and running its intermediate code for platform independence.

Use ```make all``` to build.

Use ```make sync``` to generate ```onyxc.files``` for use with QTCreator.

GPL-2.0+ License.
