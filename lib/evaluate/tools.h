// Copyright (c) 2018, NVIDIA CORPORATION.  All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef FORTRAN_EVALUATE_TOOLS_H_
#define FORTRAN_EVALUATE_TOOLS_H_

#include "expression.h"
#include "../common/idioms.h"
#include "../common/template.h"
#include "../parser/message.h"
#include "../semantics/symbol.h"
#include <array>
#include <optional>
#include <utility>

namespace Fortran::evaluate {

// Some expression predicates and extractors.

// When an Expr holds something that is a Variable (i.e., a Designator
// or pointer-valued FunctionRef), return a copy of its contents in
// a Variable.
template<typename A>
std::optional<Variable<A>> AsVariable(const Expr<A> &expr) {
  using Variant = decltype(Variable<A>::u);
  return std::visit(
      [](const auto &x) -> std::optional<Variable<A>> {
        if constexpr (common::HasMember<std::decay_t<decltype(x)>, Variant>) {
          return std::make_optional<Variable<A>>(x);
        }
        return std::nullopt;
      },
      expr.u);
}

template<typename A>
std::optional<Variable<A>> AsVariable(const std::optional<Expr<A>> &expr) {
  if (expr.has_value()) {
    return AsVariable(*expr);
  } else {
    return std::nullopt;
  }
}

// Predicate: true when an expression is a variable reference
template<typename A> bool IsVariable(const A &) { return false; }
template<typename A> bool IsVariable(const Designator<A> &designator) {
  if constexpr (common::HasMember<Substring, decltype(Designator<A>::u)>) {
    if (const auto *substring{std::get_if<Substring>(&designator.u)}) {
      return substring->GetLastSymbol() != nullptr;
    }
  }
  return true;
}
template<typename A> bool IsVariable(const FunctionRef<A> &funcRef) {
  if (const semantics::Symbol * symbol{funcRef.proc().GetSymbol()}) {
    return symbol->attrs().test(semantics::Attr::POINTER);
  } else {
    return false;
  }
}
template<typename A> bool IsVariable(const Expr<A> &expr) {
  return std::visit([](const auto &x) { return IsVariable(x); }, expr.u);
}

// Predicate: true when an expression is a constant value
template<typename A> bool IsConstant(const A &) { return false; }
template<typename A> bool IsConstant(const Constant<A> &) { return true; }
template<typename A> bool IsConstant(const Parentheses<A> &p) {
  return IsConstant(p.left());
}
template<typename A> bool IsConstant(const Expr<A> &expr) {
  return std::visit([](const auto &x) { return IsConstant(x); }, expr.u);
}

// Predicate: true when an expression is assumed-rank
template<typename A> bool IsAssumedRank(const A &) { return false; }
template<typename A> bool IsAssumedRank(const Designator<A> &designator) {
  if (const auto *symbolPtr{
          std::get_if<const semantics::Symbol *>(&designator.u)}) {
    if (const auto *details{
            (*symbolPtr)
                ->template detailsIf<semantics::ObjectEntityDetails>()}) {
      return details->IsAssumedRank();
    }
  }
  return false;
}
template<typename A> bool IsAssumedRank(const Expr<A> &expr) {
  return std::visit([](const auto &x) { return IsAssumedRank(x); }, expr.u);
}

// When an expression is a constant integer, extract its value.
template<typename A> std::optional<std::int64_t> ToInt64(const A &) {
  return std::nullopt;
}
template<int KIND>
std::optional<std::int64_t> ToInt64(
    const Constant<Type<TypeCategory::Integer, KIND>> &c) {
  return {c.value.ToInt64()};
}
template<int KIND>
std::optional<std::int64_t> ToInt64(
    const Parentheses<Type<TypeCategory::Integer, KIND>> &p) {
  return ToInt64(p.left());
}
template<typename A> std::optional<std::int64_t> ToInt64(const Expr<A> &expr) {
  return std::visit([](const auto &x) { return ToInt64(x); }, expr.u);
}
template<typename A>
std::optional<std::int64_t> ToInt64(const std::optional<A> &x) {
  if (x.has_value()) {
    return ToInt64(*x);
  } else {
    return std::nullopt;
  }
}

// Generalizing packagers: these take operations and expressions of more
// specific types and wrap them in Expr<> containers of more abstract types.

template<typename A> Expr<ResultType<A>> AsExpr(A &&x) {
  return Expr<ResultType<A>>{std::move(x)};
}

template<typename T> Expr<T> AsExpr(Expr<T> &&x) {
  static_assert(T::isSpecificIntrinsicType);
  return std::move(x);
}

template<typename A>
Expr<SomeKind<ResultType<A>::category>> AsCategoryExpr(A &&x) {
  return Expr<SomeKind<ResultType<A>::category>>{AsExpr(std::move(x))};
}

template<TypeCategory CATEGORY>
Expr<SomeKind<CATEGORY>> AsCategoryExpr(Expr<SomeKind<CATEGORY>> &&x) {
  return std::move(x);
}

template<typename A> Expr<SomeType> AsGenericExpr(A &&x) {
  return Expr<SomeType>{AsCategoryExpr(std::move(x))};
}

template<> inline Expr<SomeType> AsGenericExpr(Expr<SomeType> &&x) {
  return std::move(x);
}

template<> inline Expr<SomeType> AsGenericExpr(BOZLiteralConstant &&x) {
  return Expr<SomeType>{std::move(x)};
}

Expr<SomeReal> GetComplexPart(
    const Expr<SomeComplex> &, bool isImaginary = false);

template<int KIND>
Expr<SomeComplex> MakeComplex(Expr<Type<TypeCategory::Real, KIND>> &&re,
    Expr<Type<TypeCategory::Real, KIND>> &&im) {
  return AsCategoryExpr(ComplexConstructor<KIND>{std::move(re), std::move(im)});
}

// Creation of conversion expressions can be done to either a known
// specific intrinsic type with ConvertToType<T>(x) or by converting
// one arbitrary expression to the type of another with ConvertTo(to, from).

template<typename TO, TypeCategory FROMCAT>
Expr<TO> ConvertToType(Expr<SomeKind<FROMCAT>> &&x) {
  static_assert(TO::isSpecificIntrinsicType);
  if constexpr (FROMCAT != TO::category) {
    if constexpr (TO::category == TypeCategory::Complex) {
      using Part = typename TO::Part;
      Scalar<Part> zero;
      return Expr<TO>{ComplexConstructor<TO::kind>{
          ConvertToType<Part>(std::move(x)), Expr<Part>{Constant<Part>{zero}}}};
    } else {
      return Expr<TO>{Convert<TO, FROMCAT>{std::move(x)}};
    }
  } else {
    // Same type category
    if (auto *already{std::get_if<Expr<TO>>(&x.u)}) {
      return std::move(*already);
    }
    if constexpr (TO::category == TypeCategory::Complex) {
      // Extract, convert, and recombine the components.
      return Expr<TO>{std::visit(
          [](auto &z) {
            using FromType = ResultType<decltype(z)>;
            using FromPart = typename FromType::Part;
            using FromGeneric = SomeKind<TypeCategory::Real>;
            using ToPart = typename TO::Part;
            Convert<ToPart, TypeCategory::Real> re{Expr<FromGeneric>{
                Expr<FromPart>{ComplexComponent<FromType::kind>{false, z}}}};
            Convert<ToPart, TypeCategory::Real> im{Expr<FromGeneric>{
                Expr<FromPart>{ComplexComponent<FromType::kind>{true, z}}}};
            return ComplexConstructor<TO::kind>{
                AsExpr(std::move(re)), AsExpr(std::move(im))};
          },
          x.u)};
    } else {
      return Expr<TO>{Convert<TO, TO::category>{std::move(x)}};
    }
  }
}

template<typename TO> Expr<TO> ConvertToType(BOZLiteralConstant &&x) {
  static_assert(TO::isSpecificIntrinsicType);
  using Value = typename Constant<TO>::Value;
  if constexpr (TO::category == TypeCategory::Integer) {
    return Expr<TO>{Constant<TO>{Value::ConvertUnsigned(std::move(x)).value}};
  } else {
    static_assert(TO::category == TypeCategory::Real);
    using Word = typename Value::Word;
    return Expr<TO>{Constant<TO>{Word::ConvertUnsigned(std::move(x)).value}};
  }
}

template<TypeCategory TC, int TK, TypeCategory FC>
Expr<Type<TC, TK>> ConvertTo(
    const Expr<Type<TC, TK>> &, Expr<SomeKind<FC>> &&x) {
  return ConvertToType<Type<TC, TK>>(std::move(x));
}

template<TypeCategory TC, int TK, TypeCategory FC, int FK>
Expr<Type<TC, TK>> ConvertTo(
    const Expr<Type<TC, TK>> &, Expr<Type<FC, FK>> &&x) {
  return AsExpr(ConvertToType<Type<TC, TK>>(AsCategoryExpr(std::move(x))));
}

template<TypeCategory TC, TypeCategory FC>
Expr<SomeKind<TC>> ConvertTo(
    const Expr<SomeKind<TC>> &to, Expr<SomeKind<FC>> &&from) {
  return std::visit(
      [&](const auto &toKindExpr) {
        using KindExpr = std::decay_t<decltype(toKindExpr)>;
        return AsCategoryExpr(
            ConvertToType<ResultType<KindExpr>>(std::move(from)));
      },
      to.u);
}

template<TypeCategory TC, TypeCategory FC, int FK>
Expr<SomeKind<TC>> ConvertTo(
    const Expr<SomeKind<TC>> &to, Expr<Type<FC, FK>> &&from) {
  return ConvertTo(to, AsCategoryExpr(std::move(from)));
}

template<typename FT>
Expr<SomeType> ConvertTo(const Expr<SomeType> &to, Expr<FT> &&from) {
  return std::visit(
      [&](const auto &toCatExpr) {
        return AsGenericExpr(ConvertTo(toCatExpr, std::move(from)));
      },
      to.u);
}

template<TypeCategory CAT>
Expr<SomeKind<CAT>> ConvertTo(
    const Expr<SomeKind<CAT>> &to, BOZLiteralConstant &&from) {
  return std::visit(
      [&](const auto &tok) {
        using Ty = ResultType<decltype(tok)>;
        return AsCategoryExpr(ConvertToType<Ty>(std::move(from)));
      },
      to.u);
}

// Convert an expression of some known category to a dynamically chosen
// kind of some category (usually but not necessarily distinct).
template<TypeCategory TOCAT, typename VALUE> struct ConvertToKindHelper {
  using Result = std::optional<Expr<SomeKind<TOCAT>>>;
  static constexpr std::size_t Types{std::tuple_size_v<CategoryTypes<TOCAT>>};
  ConvertToKindHelper(int k, VALUE &&x) : kind{k}, value{std::move(x)} {}
  template<std::size_t J> Result Test() {
    using Ty = std::tuple_element_t<J, CategoryTypes<TOCAT>>;
    if (kind == Ty::kind) {
      return std::make_optional(
          AsCategoryExpr(ConvertToType<Ty>(std::move(value))));
    }
    return std::nullopt;
  }
  int kind;
  VALUE value;
};

template<TypeCategory TOCAT, typename VALUE>
Expr<SomeKind<TOCAT>> ConvertToKind(int kind, VALUE &&x) {
  return *common::SearchDynamicTypes(
      ConvertToKindHelper<TOCAT, VALUE>{kind, std::move(x)});
}

// Given a type category CAT, SameKindExprs<CAT, N> is a variant that
// holds an arrays of expressions of the same supported kind in that
// category.
template<typename A, int N = 2> using SameExprs = std::array<Expr<A>, N>;
template<int N = 2> struct SameKindExprsHelper {
  template<typename A> using SameExprs = std::array<Expr<A>, N>;
};
template<TypeCategory CAT, int N = 2>
using SameKindExprs =
    common::MapTemplate<SameKindExprsHelper<N>::template SameExprs,
        CategoryTypes<CAT>>;

// Given references to two expressions of arbitrary kind in the same type
// category, convert one to the kind of the other when it has the smaller kind,
// then return them in a type-safe package.
template<TypeCategory CAT>
SameKindExprs<CAT, 2> AsSameKindExprs(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return std::visit(
      [&](auto &&kx, auto &&ky) -> SameKindExprs<CAT, 2> {
        using XTy = ResultType<decltype(kx)>;
        using YTy = ResultType<decltype(ky)>;
        if constexpr (std::is_same_v<XTy, YTy>) {
          return {SameExprs<XTy>{std::move(kx), std::move(ky)}};
        } else if constexpr (XTy::kind < YTy::kind) {
          return {SameExprs<YTy>{ConvertTo(ky, std::move(kx)), std::move(ky)}};
        } else {
          return {SameExprs<XTy>{std::move(kx), ConvertTo(kx, std::move(ky))}};
        }
#if !__clang__ && 100 * __GNUC__ + __GNUC_MINOR__ == 801
        // Silence a bogus warning about a missing return with G++ 8.1.0.
        // Doesn't execute, but must be correctly typed.
        CHECK(!"can't happen");
        return {SameExprs<XTy>{std::move(kx), std::move(kx)}};
#endif
      },
      std::move(x.u), std::move(y.u));
}

// Ensure that both operands of an intrinsic REAL operation (or CMPLX()
// constructor) are INTEGER or REAL, then convert them as necessary to the
// same kind of REAL.
using ConvertRealOperandsResult =
    std::optional<SameKindExprs<TypeCategory::Real, 2>>;
ConvertRealOperandsResult ConvertRealOperands(parser::ContextualMessages &,
    Expr<SomeType> &&, Expr<SomeType> &&, int defaultRealKind);

// Per F'2018 R718, if both components are INTEGER, they are both converted
// to default REAL and the result is default COMPLEX.  Otherwise, the
// kind of the result is the kind of most precise REAL component, and the other
// component is converted if necessary to its type.
std::optional<Expr<SomeComplex>> ConstructComplex(parser::ContextualMessages &,
    Expr<SomeType> &&, Expr<SomeType> &&, int defaultRealKind);
std::optional<Expr<SomeComplex>> ConstructComplex(parser::ContextualMessages &,
    std::optional<Expr<SomeType>> &&, std::optional<Expr<SomeType>> &&,
    int defaultRealKind);

template<typename A> Expr<TypeOf<A>> ScalarConstantToExpr(const A &x) {
  using Ty = TypeOf<A>;
  static_assert(
      std::is_same_v<Scalar<Ty>, std::decay_t<A>> || !"TypeOf<> is broken");
  return {Constant<Ty>{x}};
}

// Combine two expressions of the same specific numeric type with an operation
// to produce a new expression.  Implements piecewise addition and subtraction
// for COMPLEX.
template<template<typename> class OPR, typename SPECIFIC>
Expr<SPECIFIC> Combine(Expr<SPECIFIC> &&x, Expr<SPECIFIC> &&y) {
  static_assert(SPECIFIC::isSpecificIntrinsicType);
  if constexpr (SPECIFIC::category == TypeCategory::Complex &&
      (std::is_same_v<OPR<LargestReal>, Add<LargestReal>> ||
          std::is_same_v<OPR<LargestReal>, Subtract<LargestReal>>)) {
    static constexpr int kind{SPECIFIC::kind};
    using Part = Type<TypeCategory::Real, kind>;
    return AsExpr(ComplexConstructor<kind>{
        AsExpr(OPR<Part>{AsExpr(ComplexComponent<kind>{false, x}),
            AsExpr(ComplexComponent<kind>{false, y})}),
        AsExpr(OPR<Part>{AsExpr(ComplexComponent<kind>{true, x}),
            AsExpr(ComplexComponent<kind>{true, y})})});
  } else {
    return AsExpr(OPR<SPECIFIC>{std::move(x), std::move(y)});
  }
}

// Given two expressions of arbitrary kind in the same intrinsic type
// category, convert one of them if necessary to the larger kind of the
// other, then combine the resulting homogenized operands with a given
// operation, returning a new expression in the same type category.
template<template<typename> class OPR, TypeCategory CAT>
Expr<SomeKind<CAT>> PromoteAndCombine(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return std::visit(
      [](auto &&xy) {
        using Ty = ResultType<decltype(xy[0])>;
        return AsCategoryExpr(
            Combine<OPR, Ty>(std::move(xy[0]), std::move(xy[1])));
      },
      AsSameKindExprs(std::move(x), std::move(y)));
}

// Given two expressions of arbitrary type, try to combine them with a
// binary numeric operation (e.g., Add), possibly with data type conversion of
// one of the operands to the type of the other.  Handles special cases with
// typeless literal operands and with REAL/COMPLEX exponentiation to INTEGER
// powers.
template<template<typename> class OPR>
std::optional<Expr<SomeType>> NumericOperation(parser::ContextualMessages &,
    Expr<SomeType> &&, Expr<SomeType> &&, int defaultRealKind);

extern template std::optional<Expr<SomeType>> NumericOperation<Power>(
    parser::ContextualMessages &, Expr<SomeType> &&, Expr<SomeType> &&,
    int defaultRealKind);
extern template std::optional<Expr<SomeType>> NumericOperation<Multiply>(
    parser::ContextualMessages &, Expr<SomeType> &&, Expr<SomeType> &&,
    int defaultRealKind);
extern template std::optional<Expr<SomeType>> NumericOperation<Divide>(
    parser::ContextualMessages &, Expr<SomeType> &&, Expr<SomeType> &&,
    int defaultRealKind);
extern template std::optional<Expr<SomeType>> NumericOperation<Add>(
    parser::ContextualMessages &, Expr<SomeType> &&, Expr<SomeType> &&,
    int defaultRealKind);
extern template std::optional<Expr<SomeType>> NumericOperation<Subtract>(
    parser::ContextualMessages &, Expr<SomeType> &&, Expr<SomeType> &&,
    int defaultRealKind);

std::optional<Expr<SomeType>> Negation(
    parser::ContextualMessages &, Expr<SomeType> &&);

// Given two expressions of arbitrary type, try to combine them with a
// relational operator (e.g., .LT.), possibly with data type conversion.
std::optional<Expr<LogicalResult>> Relate(parser::ContextualMessages &,
    RelationalOperator, Expr<SomeType> &&, Expr<SomeType> &&);

Expr<SomeLogical> LogicalNegation(Expr<SomeLogical> &&);
Expr<SomeLogical> BinaryLogicalOperation(
    LogicalOperator, Expr<SomeLogical> &&, Expr<SomeLogical> &&);

// Convenience functions and operator overloadings for expression construction.
// These interfaces are defined only for those situations that can never
// emit any message.  Use the more general templates (above) in other
// situations.

template<TypeCategory C, int K>
Expr<Type<C, K>> operator-(Expr<Type<C, K>> &&x) {
  return AsExpr(Negate<Type<C, K>>{std::move(x)});
}

template<int K>
Expr<Type<TypeCategory::Complex, K>> operator-(
    Expr<Type<TypeCategory::Complex, K>> &&x) {
  using Part = Type<TypeCategory::Real, K>;
  return AsExpr(ComplexConstructor<K>{
      AsExpr(Negate<Part>{AsExpr(ComplexComponent<K>{false, x})}),
      AsExpr(Negate<Part>{AsExpr(ComplexComponent<K>{true, x})})});
}

template<TypeCategory C, int K>
Expr<Type<C, K>> operator+(Expr<Type<C, K>> &&x, Expr<Type<C, K>> &&y) {
  return AsExpr(Combine<Add, Type<C, K>>(std::move(x), std::move(y)));
}

template<TypeCategory C, int K>
Expr<Type<C, K>> operator-(Expr<Type<C, K>> &&x, Expr<Type<C, K>> &&y) {
  return AsExpr(Combine<Subtract, Type<C, K>>(std::move(x), std::move(y)));
}

template<TypeCategory C, int K>
Expr<Type<C, K>> operator*(Expr<Type<C, K>> &&x, Expr<Type<C, K>> &&y) {
  return AsExpr(Combine<Multiply, Type<C, K>>(std::move(x), std::move(y)));
}

template<TypeCategory C, int K>
Expr<Type<C, K>> operator/(Expr<Type<C, K>> &&x, Expr<Type<C, K>> &&y) {
  return AsExpr(Combine<Divide, Type<C, K>>(std::move(x), std::move(y)));
}

template<TypeCategory C> Expr<SomeKind<C>> operator-(Expr<SomeKind<C>> &&x) {
  return std::visit(
      [](auto &xk) { return Expr<SomeKind<C>>{-std::move(xk)}; }, x.u);
}

template<TypeCategory CAT>
Expr<SomeKind<CAT>> operator+(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return PromoteAndCombine<Add, CAT>(std::move(x), std::move(y));
}

template<TypeCategory CAT>
Expr<SomeKind<CAT>> operator-(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return PromoteAndCombine<Subtract, CAT>(std::move(x), std::move(y));
}

template<TypeCategory CAT>
Expr<SomeKind<CAT>> operator*(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return PromoteAndCombine<Multiply, CAT>(std::move(x), std::move(y));
}

template<TypeCategory CAT>
Expr<SomeKind<CAT>> operator/(
    Expr<SomeKind<CAT>> &&x, Expr<SomeKind<CAT>> &&y) {
  return PromoteAndCombine<Divide, CAT>(std::move(x), std::move(y));
}

// A utility for use with common::SearchDynamicTypes to create generic
// expressions when an intrinsic type category for (say) a variable is known
// but the kind parameter value is not.
template<TypeCategory CAT, template<typename> class TEMPLATE, typename VALUE>
struct TypeKindVisitor {
  using Result = std::optional<Expr<SomeType>>;
  static constexpr std::size_t Types{std::tuple_size_v<CategoryTypes<CAT>>};

  TypeKindVisitor(int k, VALUE &&x) : kind{k}, value{std::move(x)} {}
  TypeKindVisitor(int k, const VALUE &x) : kind{k}, value{x} {}

  template<std::size_t J> Result Test() {
    using Ty = std::tuple_element_t<J, CategoryTypes<CAT>>;
    if (kind == Ty::kind) {
      return AsGenericExpr(TEMPLATE<Ty>{std::move(value)});
    }
    return std::nullopt;
  }

  int kind;
  VALUE value;
};
}
#endif  // FORTRAN_EVALUATE_TOOLS_H_
