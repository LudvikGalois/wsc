# wsc
An optimising whitespace compiler using LLVM.
Currently implements everything but shuffle
# installation
If you've got LLVM4 installed and stack,
`stack build` et al should just work
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
probably sufficient to get most things working
