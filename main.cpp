#include "fmt.hpp"
#include "tuple.hpp"
#include <array>
#include <cstdio>
#include <type_traits>

//===---------------------------------------------------------------------===//
// Silly and simple compile-time L-systems. Jump to the bottom for an example!
//===---------------------------------------------------------------------===//

// 'Tests' for em::tuple
namespace {

using R = em::tuple<int, float, char, float>;
static_assert(std::is_same_v<em::tuple_element_t<2, R>, char>);
static_assert(em::tuple_size_v<R> == 4);

} // namespace

//===----------------------------------------------------------------------===//
// Implementation functions.
// Since the contents of the tuples are irrelevant, most of these functions
// don't actually need to be defined. It's assumed that the Symbol types have a
// definition and are default constructible though, so we can avoid declval.
//===----------------------------------------------------------------------===//
template <char Name> struct Symbol {
  constexpr static inline char name = Name;
};

// Really simple substitute for em::tuple_cat that only cares about types.
template <class... Is, class... Js>
auto join(em::tuple<Is...>, em::tuple<Js...>) -> em::tuple<Is..., Js...>;

// Base cases for applying productions to tuples.
template <class I>
auto produce(em::tuple<I>) -> decltype(produce(std::declval<I>()));
auto produce(em::tuple<>) -> em::tuple<>;

// Apply production rules in parallel to each element of the tuple.
template <class I, class J, class... Is>
auto produce(em::tuple<I, J, Is...> T) -> decltype(
    join(decltype(produce(I{})){},
         decltype(join(decltype(produce(J{})){},
                       decltype(produce(em::tuple<Is...>{})){})){}));

// Base template for implementation of a repeated call to `produce`.
// Naively writing this as one function and trying to put the result in the
// return type, such as with
// template <int N, class Arg>
// auto produce(Arg) -> std::conditional_t<
//    N == 0, Arg, decltype(produce(decltype(produce<N - 1, Arg>(Arg{})){}))>;
// causes an infinite descent of template instantiation.
// Partially specializing for the N == 0 case and calling this via a function
// template avoids that.
// Of course none of this machinery is necessary if we just wrote an ordinary
// function with a definition, but where's the fun in that?
template <int N, class A> struct produceImpl {
  auto operator()() -> decltype(produce(decltype(produceImpl<N - 1, A>{}()){}));
};

template <class A> struct produceImpl<0, A> { auto operator()() -> A; };

//===----------------------------------------------------------------------===//
// Interface functions, go ahead and use these.
// `produce` isn't designed to actually be called; wrap it in a decltype.
//===----------------------------------------------------------------------===//

// Run the produce rule N times.
template <int N, class Arg>
auto produce(Arg) -> decltype(produceImpl<N, Arg>{}());

//===----------------------------------------------------------------------===//
// L-System definition.
//
// Define the symbols by inheriting from `Symbol` and supplying a name. The
// symbols should be default constructible, and an empty definition is
// sufficient.
//
// For each symbol, define the production rule by declaring a function
// `produce` that takes that symbol and returns a word, represented as a tuple
// of symbols. The production functions don't need to have a definition, since
// they are never actually called. This makes them look more like the
// mathematical notation for a production rule, which I think is nice.
//
// Several examples are presented below.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// 1. Lindenmayer's system for modelling algae growth.
//
// This is the canonical example of an L-system (see basically any book on
// L-systems or https://en.wikipedia.org/wiki/L-system#Example_1:_Algae) and
// consists of the production rules:
// I : A
// p0: A -> AB
// p1: -> A
// The function `printExample` shows how to print the result.
//===----------------------------------------------------------------------===//
struct A : Symbol<'A'> {};
struct B : Symbol<'B'> {};

auto produce(A) -> em::tuple<A, B>;
auto produce(B) -> em::tuple<A>;

static_assert(std::is_same_v<decltype(produce<1>(A{})), em::tuple<A, B>>);
static_assert(std::is_same_v<decltype(produce<2>(A{})), em::tuple<A, B, A>>);
static_assert(
    std::is_same_v<decltype(produce<3>(A{})), em::tuple<A, B, A, A, B>>);

void printExample1() {
  using R = decltype(produce<10>(A{}));
  auto chars = em::formatWord(R{});
  printf("%s\n", chars.data());
}

//===----------------------------------------------------------------------===//
// 2. A parametric L-System
//
// This example is taken from (1.7) in 'The Algorithmic Beauty of Plants', and
// illustrates how to implement a parametric L-system using class templates for
// symbols and SFINAE. Parametric L-systems allow the symbols to have finitely
// many parameters and each production rule is associated with a predicate that
// must be satisfied by the paramters for the rule to be selected. The L-system
// here is:
// I : V(2) U(4, 4)
// p1: U(X, Y) : Y <= 3 -> U(2 * X, X + Y)
// p2: U(X, Y) : Y > 3  -> V(X) U(X / Y, 0)
// p3: V(X)    : X < 1  -> W
// p4: W -> W
//
// This is supported using the exact same machinery used to implement
// nonparametric L-systems if now the symbols are replaced by class templates
// with non-type template parameters and the production rules replaced by
// function templates that SFINAE away when the predicate is false.
//
// The definition of a parametric L-system requires the parameters be real,
// but non-type template parameters may not be of floating-point type and so the
// parameters here are restricted to integers instead.
// TODO: Try out `std::ratio` as a replacement of `int`.
//===----------------------------------------------------------------------===//
template <int X, int Y> struct U : Symbol<'U'> {};
template <int X> struct V : Symbol<'V'> {};
struct W : Symbol<'W'> {};

template <int X, int Y, class = std::enable_if_t<(Y <= 3)>>
auto produce(U<X, Y>) -> em::tuple<U<2 * X, X + Y>>;

template <int X, int Y, class = std::enable_if_t<(Y > 3)>>
auto produce(U<X, Y>) -> em::tuple<V<X>, U<X / Y, 0>>;

template <int X, class = std::enable_if_t<(X < 1)>>
auto produce(V<X>) -> em::tuple<W>;

template <int X, class = std::enable_if_t<(X >= 1)>>
auto produce(V<X>) -> em::tuple<V<X - 1>>;

auto produce(W) -> em::tuple<W>;

static_assert(std::is_same_v<decltype(produce<1>(em::tuple<V<2>, U<4, 4>>{})),
                             em::tuple<V<1>, V<4>, U<1, 0>>>);
static_assert(std::is_same_v<decltype(produce<2>(em::tuple<V<2>, U<4, 4>>{})),
                             em::tuple<V<0>, V<3>, U<2, 1>>>);
static_assert(std::is_same_v<decltype(produce<3>(em::tuple<V<2>, U<4, 4>>{})),
                             em::tuple<W, V<2>, U<4, 3>>>);

void printExample2() {
  using R = decltype(produce<10>(em::tuple<V<2>, U<4, 4>>{}));
  auto chars = em::formatWord(R{});
  printf("%s\n", chars.data());
}

int main() {
  printExample1();
  printExample2();
}
