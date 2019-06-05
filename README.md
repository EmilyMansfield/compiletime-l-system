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

An [*l-system*](https://en.wikipedia.org/wiki/L-system) is a parellel rewriting
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

This was a little project that was put together in an evening. It's probably
inefficient, and not very useful, but it was fun to write.

