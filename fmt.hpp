#ifndef COMPILETIME_L_SYSTEM_FMT_HPP
#define COMPILETIME_L_SYSTEM_FMT_HPP

#include "tuple.hpp"

namespace em {

namespace internal {

//===----------------------------------------------------------------------===//
// fmtLength overloads return the length of the formatted version of their
// argument.
//===----------------------------------------------------------------------===//

// Number of characters needed to represent i, formatted as a base-10 integer.
constexpr std::size_t fmtLength(int i) {
  std::size_t digits{i < 0 ? 1u : 0u};
  while (i != 0) {
    i /= 10;
    ++digits;
  }
  return digits;
}

// Number of characters needed to represent the parametric symbol Sym, formatted
// in prefix notation with its arguments written as base-10 integers.
// For example, X<10, 5> would appear as (X 10 5) in this notation and so would
// have length 8.
template <template <int...> class Sym, int... Params>
constexpr auto fmtLength(Sym<Params...>) -> std::enable_if_t<
    std::is_same_v<decltype(Sym<Params...>::name), const char>, std::size_t> {
  std::size_t len{3u};
  ((len += 1u + internal::fmtLength(Params), (void)0), ...);
  return len;
}

// Number of characters needed to represent the nonparametric symbol Sym.
// This is always 1, as symbols may only have single character names.
template <class Sym>
constexpr auto fmtLength(Sym)
    -> std::enable_if_t<std::is_same_v<decltype(Sym::name), const char>,
                        std::size_t> {
  return 1u;
}

// Number of characters needed to represent the given word as a formatted
// string. Symbols are concatenated with no spaces, such as in AB or X(Y 10)X.
template <class... Ts> constexpr std::size_t fmtLength(em::tuple<Ts...>) {
  std::size_t len{};
  ((len += internal::fmtLength(Ts{})), ...);
  return len;
}

//===----------------------------------------------------------------------===//
// Actual formatting functions.
//===----------------------------------------------------------------------===//

// Format an integer into an bidirectional iterator range starting at `it`,
// returning an iterator to one past the end of the written subrange.
template <class It> It fmt(int i, It it) {
  It end = it + internal::fmtLength(i);
  int absI = i < 0 ? -i : i;
  for (It jt = end; jt != it + (i < 0);) {
    *--jt = static_cast<char>('0' + (absI % 10));
    absI /= 10;
  }
  if (i < 0)
    *it = '-';
  return end;
}

// Format a nonparametric symbol into an input iterator `it`, returning the
// iterator one past `it`.
template <class It, class Sym> It fmt(Sym, It it) {
  *it++ = Sym::name;
  return it;
}

// Format a parametric symbol into a bidirectional iterator range starting at
// `it`, returning one past the end of the written range.
template <class It, template <int...> class Sym, int... Params>
It fmt(Sym<Params...>, It it) {
  *it++ = '(';
  *it++ = Sym<Params...>::name;
  [[maybe_unused]] std::array dummy{
      ((void)std::array{(*it++ = ' ', 0), (it = internal::fmt(Params, it), 0)},
       0)...};
  *it++ = ')';
  return it;
}

} // namespace internal

//===---------------------------------------------------------------------===//
// Interface
//===---------------------------------------------------------------------===//

template <class... Ts>
auto formatWord(em::tuple<Ts...>)
    -> std::array<char, 1u + internal::fmtLength(em::tuple<Ts...>{})> {
  std::array<char, 1u + internal::fmtLength(em::tuple<Ts...>{})> chars{};
  auto it = chars.begin();
  em::tuple_for_each(em::tuple<Ts...>{},
                     [&it](auto t) { it = internal::fmt(t, it); });
  return chars;
}

} // namespace em

#endif
