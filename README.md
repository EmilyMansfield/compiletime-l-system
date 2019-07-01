# Compile-time L-Systems in C++

Evolve tuples of types according to the rules of an L-system. For example,

```cpp
struct A : Symbol<'a'> {};
struct B : Symbol<'b'> {};

auto produce(A) -> em::tuple<A, B>;
auto produce(B) -> em::tuple<A>;

static_assert(std::is_same_v<decltype(produce<3>(A{})),
              em::tuple<A, B, A, A, B>>);
```

Parametric L-systems are also supported, as are stochastic L-systems.

An [*L-system*](https://en.wikipedia.org/wiki/L-system) is a parallel rewriting
system that takes an initial string of symbols, called a word, and
simultaneously applies a set of production rules that replace each symbol with
another word.

For example, repeatedly applying the production rules `a -> ab` and `b -> a` to
the initial word `a` gives the sequence
```
a
ab
aba
abaab
abaababa
...
```

The above C++ code produces the same output as this L-System if the symbols 'a'
and 'b' are interpreted as types.

### Requirements

The examples as written should compile on anything that supports C++17.
Suitably modern versions of GCC, Clang, and MSVC have all been tested.

If you increase the number of generations in the examples or write anything
considerably more complex, then be prepared to have a lot of memory handy! This
is pretty taxing on the compiler, though Clang copes the best due to some
additional optimizations that can be made using intrinsics.

This project is of course in no way serious and is horrendously impractical, but
it's been fun to write!
