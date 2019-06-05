#include <array>
#include <cstdio>
#include <type_traits>

//===---------------------------------------------------------------------===//
// Silly and simple compile-time L-systems. Jump to the bottom for an example!
//===---------------------------------------------------------------------===//

//===---------------------------------------------------------------------===//
// Replacement for std::tuple.
// Everything works with std::tuple, it's just *very* memory hungry. We just
// need an ordered type container and a few helper functions surrounding it,
// not all of std::tuple's magic or even the ability to store data.
//===---------------------------------------------------------------------===//
namespace em {

template <class... Ts> struct tuple {};

// std::tuple_element replacement
template <std::size_t I, class T, class... Ts> struct tuple_element_impl {
  using type = typename tuple_element_impl<I - 1, Ts...>::type;
};

template <class T, class... Ts> struct tuple_element_impl<0, T, Ts...> {
  using type = T;
};

template <std::size_t I, class... Ts>
auto tuple_element_f(em::tuple<Ts...>) -> tuple_element_impl<I, Ts...>;

template <std::size_t I, class T> struct tuple_element {
  using type = typename decltype(tuple_element_f<I>(std::declval<T>()))::type;
};

template <std::size_t I, class T>
using tuple_element_t = typename tuple_element<I, T>::type;

// std::tuple_size replacement
template <class T> struct tuple_size;
template <class... Ts>
struct tuple_size<em::tuple<Ts...>>
    : std::integral_constant<std::size_t, sizeof...(Ts)> {};

template <class T>
static constexpr inline std::size_t tuple_size_v = tuple_size<T>::value;

// 'Tests'
namespace {
using R = em::tuple<int, float, char, float>;
static_assert(std::is_same_v<em::tuple_element_t<2, R>, char>);
static_assert(em::tuple_size_v<R> == 4);
} // namespace

} // namespace em

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

// Set the I-th character in the array to the name of the I-th symbol.
// T should be a tuple of symbols and Arr an array of characters at least as
// long as the tuple.
template <std::size_t I, class T, class Arr> void setChar(Arr &arr) {
  arr[I] = em::tuple_element_t<I, T>::name;
}

// Copy the names of each symbol in the word into the array.
// T should be a tuple of symbols and Arr an array of characters at least as
// long as the tuple. The length of the index sequence should equal the length
// of the tuple.
template <class T, class Arr, std::size_t... Is>
void getNameImpl(Arr &arr, std::index_sequence<Is...> is) {
  std::array a{(setChar<Is, T>(arr), 0)...};
}

//===----------------------------------------------------------------------===//
// Interface functions, go ahead and use these.
// `produce` isn't designed to actually be called; wrap it in a decltype.
//===----------------------------------------------------------------------===//

// Run the produce rule N times.
template <int N, class Arg>
auto produce(Arg) -> decltype(produceImpl<N, Arg>{}());

// Copy the names of each symbol in the word into the array.
// T should be a tuple of symbols and Arr an array of characters at least as
// long as the tuple.
template <class T, class Arr> void getName(Arr &arr) {
  getNameImpl<T>(arr, std::make_index_sequence<em::tuple_size<T>::value>{});
}

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
// The example below is Lindenmayer's system for modelling algae growth:
// https://en.wikipedia.org/wiki/L-system#Example_1:_Algae
//===----------------------------------------------------------------------===//
struct A : Symbol<'A'> {};
struct B : Symbol<'B'> {};

auto produce(A) -> em::tuple<A, B>;
auto produce(B) -> em::tuple<A>;

static_assert(std::is_same_v<decltype(produce<1>(A{})), em::tuple<A, B>>);
static_assert(std::is_same_v<decltype(produce<2>(A{})), em::tuple<A, B, A>>);
static_assert(
    std::is_same_v<decltype(produce<3>(A{})), em::tuple<A, B, A, A, B>>);

int main() {
  using R = decltype(produce<10>(A{}));
  std::array<char, em::tuple_size<R>::value + 1> chars{};
  getName<R>(chars);
  printf("%s\n", chars.data());
}
