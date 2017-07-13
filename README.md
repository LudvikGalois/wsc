# wsc
An optimising whitespace compiler using LLVM.
Currently implements everything from the reference implementation
but shuffle (with a few limitations)
# installation
If you've got LLVM4 installed and stack,
`stack build` et al should just work, or if you don't have stack
`cabal build` will probably also work. It does also use a C compiler
for building a small run time system and for linking (since we depend
on a few things in libc), but it's unlikely you're trying to install
this and don't have a C compiler.
# limitations
We don't use arbitrary sized ints, it's
not particularly hard to change to using
a bunch of calls to gmp, but I wanted this
to be fast, and currently we don't support
negative heap addresses. In the future we
might just move our heap pointer to the middle
of the allocated heap to make this work, but I'm not sure
how much whitespace code in the wild uses negative
heap addresses. We might add int size as an argument
which whilst not as good as arbitrary size ints, is
probably sufficient to get most things working.
