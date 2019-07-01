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

//===   The following functions are useful for stochastic L-systems only   ===//

// Wrapper around a symbol with a single parameter to allow applying
// transformations of the form T(S) -> T(K*S + L) to entire tuples without
// having to match each of the T and S.
template <int S, template <int> class T> struct AffineWrapper {
  static constexpr inline int value = S;
  template <int S1> using type = T<S1>;
};

// Take a tuple of parameterised symbols and put each in an AffineWrapper.
template <int S, template <int> class... Ts>
auto affineWrap(em::tuple<Ts<S>...>) -> em::tuple<AffineWrapper<S, Ts>...>;

// Take a tuple of AffineWrappers and unwrap them into parameterised symbols.
template <class... Wrappers>
auto affineUnwrap(em::tuple<Wrappers...>)
    -> em::tuple<typename Wrappers::template type<Wrappers::value>...>;

// Apply the affine transformation S -> N * S + K.
template <int N, int K, int S, template <int> class T>
auto affineTransform(AffineWrapper<S, T>) -> AffineWrapper<N * S + K, T>;

// Implementation of affineTransform(em::tuple<Wrappers...>)
template <int N, class Tup, int... Ks>
auto affineTransform(std::integer_sequence<int, Ks...>, Tup) -> em::tuple<
    decltype(affineTransform<N, Ks>(em::tuple_element_t<Ks, Tup>{}))...>;

// Apply the affine transformations N*S, N*S+1, ..., N*S+(N-1) to each of the
// elements in a tuple of AffineWrappers.
template <int N, class... Wrappers>
auto affineTransform(em::tuple<Wrappers...> wrappers)
    -> decltype(affineTransform<N>(
        std::make_integer_sequence<int, sizeof...(Wrappers)>{}, wrappers));

// Transform a word T1(S) T2(S) ... TK(S) -> T1(N*S) T2(N*S+1) ...  TK(N*S+K-1)
// Used in stochastic L-systems to ensure that the PRNG state is propagated
// reasonably.
template <int N, int S, template <int> class... Ts>
auto stochastic(em::tuple<Ts<S>...> ts) -> decltype(
    affineUnwrap(decltype(affineTransform<N>(decltype(affineWrap(ts)){})){}));

// Simple linear congruential generator, similar to the
// UniformRandomBitGenerator interface but adapted for compile time.
template <uint32_t Seed> struct minstd_rand {

  constexpr uint32_t operator()(int state) {
    uint32_t s = Seed;
    for (int i = 1; i < state; ++i)
      s = (16807u * s) % (max() + 1u);
    return s;
  }

  constexpr static uint32_t min() { return 0u; }
  constexpr static uint32_t max() { return 2147483646u; }
};

//===----------------------------------------------------------------------===//
// Interface functions, go ahead and use these.
// `produce` isn't designed to actually be called; wrap it in a decltype.
//===----------------------------------------------------------------------===//

// Run the produce rule N times.
template <int N, class Arg>
auto produce(Arg) -> decltype(produceImpl<N, Arg>{}());

template <int N, class T> using stochastic_t = decltype(stochastic<N>(T{}));

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

///===---------------------------------------------------------------------===//
// 3. A Stochastic L-System
//
// In a stochastic L-system there may be more than one production rule for each
// symbol; which one to take is chosen probabilistically. The example below
// shows how to implement a stochastic L-system by modelling it as a parametric
// L-system whose parameter represents the internal state of a pseudorandom
// number generator, then annotating the return type to ensure that the state
// propagates semi-reasonably.
//
// The example below is an approximation of the L-system
// I : A
// p1: A -[0.5]-> AA
// p2: A -[0.5]-> BA
// p3: B -[1.0] B
//
// The first template parameter to stochastic_t should be the maximum number of
// terms on the right-hand side of any production rule for the system. As a user
// you need to define the probability distribution yourself, but an example PRNG
// following a similar interface to a UniformRandomBitGenerator is provided as a
// source of entropy.
//
// The rest of this explanation centres around the implementation itself, and
// what the stochastic_t wrapper around the return type is for. PRNGs are
// fundamentally stateful, but when messing with types we're limited to
// manipulating constant objects with pure functions. The state therefore needs
// to be maintained outside of the PRNG itself, inside of the symbols.
//
// The problem is that the state can only be propagated downwards through the
// production rules, whereas the optimal behaviour would have it propagating
// sideways within each generation. Specifically, each call to the PRNG should
// advance the state by one iteration, ensuring that (so long as we don't reach
// the period of the PRNG) no two symbols ever occur with the same state, and
// that we don't miss any. If the same symbol/state pair did occur in two
// different places, then all the descendants of those symbols would be
// identical for the rest of the simulation, which isn't great.
//
// For example, we would like the state to evolve as
//   A(1)
//   A(gen(1)) A(gen^2(1))
//   A(gen^3(1)) A(gen^4(1)) A(gen^5(1)) A(gen^6(1))
// but this would require that each production know some kind of global
// information about the system, which it cannot (in general) because only the
// symbol/state pair that it's acting on is available. Were each production rule
// to update the state as if it were the only production running, such as with
//   template<int S, class = /* enable with probability 1, for exposition */>
//   auto produce(A<S>) -> em::tuple<A<gen(S)>, A<gen(gen(S))>;
// we would quickly run into duplicate pairs:
//   A(1)
//   A(gen(1)) A(gen^2(1))
//   A(gen^2(1)) A(gen^3(1)) A(gen^3(1)) A(gen^4(1))
//
// A better strategy can be developed as follows. Suppose that the maximum
// number of symbols on the right-hand side of any production rule is d. We can
// pretend that every production rule has d symbols on its right-hand side by
// inserting enough 'empty' symbols to the right. Applying the unique production
// rule to a symbol A(s), say
//   A(s) -> A0(f0(s)) A1(f1(s)) ... Ad(fd(s))
// replaces A(s) with a new set of symbols A0, A1, ..., Ad (some of which may be
// empty) whose states are determined by a collection of functions
// f0, f1, ..., fd acting on s. Each symbol is indexed by an integer, which can
// be thought of as a digit in base d. If these digits are tracked for each
// symbol as the system evolves, then at each generation every symbol in the
// word is described by a unique number in base d.  Choosing the functions
//   fi(s) := d * s + i
// store this number in the state itself.
//
// This cannot be the actual internal state of the PRNG, since that is
// controlled by the PRNG itself. For the linear congruential generator used in
// this example, the internal state should transform by s_{n+1} = gen(s_n), or
// with initial seed 1 (for example) s_n = gen^n(1). We can therefore interpret
// the above state not as s_n, but rather as n; whenever a random number is
// needed, the PRNG can just be run as many times as the state of the symbol
// that the production is acting on, and then the state can be advanced
// according to the fi above.
//
// The stochastic_t wrapper hides the fi by taking the return type of a
// production rule and returning a new return type that has the fi applied to
// each state. In other words,
//   stochastic_t : A0(s) A1(s) ... Ab(s) -> A0(f0(s)) A1(f1(s)) ... Ab(fb(s))
// where 0 <= b < d and d is the first template parameter of stochastic_t.
///===---------------------------------------------------------------------===//

using PRNG = minstd_rand</*seed=*/32>;

constexpr int bernoulli(uint32_t s) {
  return PRNG()(s) > (PRNG::max() / 2) ? 1 : 0;
}

template <int S> struct R1 : Symbol<'A'> {};
template <int S> struct R2 : Symbol<'B'> {};

template <int S, class = std::enable_if_t<(bernoulli(S) == 0)>>
auto produce(R1<S>) -> stochastic_t<2, em::tuple<R1<S>, R1<S>>>;

template <int S, class = std::enable_if_t<(bernoulli(S) == 1)>>
auto produce(R1<S>) -> stochastic_t<2, em::tuple<R2<S>>>;

template <int S> auto produce(R2<S>) -> stochastic_t<2, em::tuple<R2<S>>>;

void printExample3() {
  using R = decltype(produce<10>(em::tuple<R1<1>>{}));
  auto chars = em::formatWord(R{});
  printf("%s\n", chars.data());
}

int main() {
  printExample1();
  printExample2();
  printExample3();
}
