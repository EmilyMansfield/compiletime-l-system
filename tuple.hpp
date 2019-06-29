#ifndef COMPILETIME_L_SYSTEM_TUPLE_HPP
#define COMPILETIME_L_SYSTEM_TUPLE_HPP

#include <array>

//===---------------------------------------------------------------------===//
// Replacement for std::tuple.
// Everything works with std::tuple, it's just *very* memory hungry. We just
// need an ordered type container and a few helper functions surrounding it,
// not all of std::tuple's magic or even the ability to store data.
//===---------------------------------------------------------------------===//

namespace em {

template <class... Ts> struct tuple {};

// std::tuple_element replacement
// The naive recursive approach used below is slow and uses a lot of memory,
// but it's still a lot better than std::tuple. On Clang we can use a builtin
// function to do much better.

// Workaround for compilers other than Clang.
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif

#if defined(__clang__) && __has_builtin(__type_pack_element)
template <std::size_t I, class... Ts>
auto tuple_element_f(em::tuple<Ts...>) -> __type_pack_element<I, Ts...>;
#else
template <std::size_t I, class T, class... Ts> struct tuple_element_impl {
  using type = typename tuple_element_impl<I - 1, Ts...>::type;
};

template <class T, class... Ts> struct tuple_element_impl<0, T, Ts...> {
  using type = T;
};

template <std::size_t I, class... Ts>
auto tuple_element_f(em::tuple<Ts...>) ->
    typename tuple_element_impl<I, Ts...>::type;
#endif

template <std::size_t I, class T> struct tuple_element {
  using type = decltype(tuple_element_f<I>(std::declval<T>()));
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

// Evaluate a function on a default constructed value of each type of a tuple.
template <class F, class... Ts> void tuple_for_each(em::tuple<Ts...>, F &&f) {
  [[maybe_unused]] std::array dummy{(std::forward<F>(f)(Ts{}), 0)...};
}

} // namespace em

#endif
