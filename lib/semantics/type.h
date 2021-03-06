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

#ifndef FORTRAN_SEMANTICS_TYPE_H_
#define FORTRAN_SEMANTICS_TYPE_H_

#include "attr.h"
#include "../common/fortran.h"
#include "../common/idioms.h"
#include "../common/indirection.h"
#include "../evaluate/expression.h"
#include "../parser/char-block.h"
#include <list>
#include <memory>
#include <optional>
#include <ostream>
#include <string>
#include <unordered_map>
#include <variant>

namespace Fortran::parser {
struct Expr;
}

namespace Fortran::semantics {

class Scope;
class Symbol;
class SemanticsContext;
class ExprResolver;

/// A SourceName is a name in the cooked character stream,
/// i.e. a range of lower-case characters with provenance.
using SourceName = parser::CharBlock;
using TypeCategory = common::TypeCategory;
using SomeExpr = evaluate::Expr<evaluate::SomeType>;
using MaybeExpr = std::optional<SomeExpr>;

// An expression that starts out as a parser::Expr and gets resolved to
// a MaybeExpr. Resolve should not be called until after names are resolved.
// An unresolved LazyExpr should not be used after the parse tree is deleted.
class LazyExpr {
public:
  LazyExpr() : u_{nullptr} {}
  LazyExpr(const parser::Expr &expr) : u_{&expr} {}
  LazyExpr(SomeExpr &&);
  LazyExpr(LazyExpr &&) = default;
  LazyExpr &operator=(LazyExpr &&) = default;
  LazyExpr Clone() const { return LazyExpr(*this); }
  const MaybeExpr Get() const;
  MaybeExpr Get();
  bool Resolve(SemanticsContext &);

private:
  using CopyableExprPtr = common::Indirection<SomeExpr, true>;
  struct ErrorInExpr {};  // marks an expr with an error in evaluation
  std::variant<const parser::Expr *, CopyableExprPtr, ErrorInExpr> u_;

  LazyExpr(const LazyExpr &) = default;
  friend std::ostream &operator<<(std::ostream &, const LazyExpr &);
};

// An array spec bound: an explicit integer expression or ASSUMED or DEFERRED
class Bound {
public:
  static Bound Assumed() { return Bound(Category::Assumed); }
  static Bound Deferred() { return Bound(Category::Deferred); }
  Bound(const parser::Expr &expr)
    : category_{Category::Explicit}, expr_{expr} {}
  Bound(int bound);
  Bound(Bound &&) = default;
  Bound &operator=(Bound &&) = default;
  Bound Clone() const { return Bound(category_, expr_.Clone()); }
  bool isExplicit() const { return category_ == Category::Explicit; }
  bool isAssumed() const { return category_ == Category::Assumed; }
  bool isDeferred() const { return category_ == Category::Deferred; }
  const LazyExpr &GetExplicit() const { return expr_; }
  void Resolve(SemanticsContext &);

private:
  enum class Category { Explicit, Deferred, Assumed };
  Bound(Category category) : category_{category} {}
  Bound(Category category, LazyExpr &&expr)
    : category_{category}, expr_{std::move(expr)} {}
  Category category_;
  LazyExpr expr_;
  friend std::ostream &operator<<(std::ostream &, const Bound &);
};

class IntrinsicTypeSpec {
public:
  IntrinsicTypeSpec(TypeCategory, int kind);
  const TypeCategory category() const { return category_; }
  const int kind() const { return kind_; }
  bool operator==(const IntrinsicTypeSpec &x) const {
    return category_ == x.category_ && kind_ == x.kind_;
  }
  bool operator!=(const IntrinsicTypeSpec &x) const { return !operator==(x); }

private:
  TypeCategory category_;
  int kind_;
  friend std::ostream &operator<<(std::ostream &os, const IntrinsicTypeSpec &x);
  // TODO: Character and len
};

class ShapeSpec {
public:
  // lb:ub
  static ShapeSpec MakeExplicit(Bound &&lb, Bound &&ub) {
    return ShapeSpec(std::move(lb), std::move(ub));
  }
  // 1:ub
  static const ShapeSpec MakeExplicit(Bound &&ub) {
    return MakeExplicit(Bound{1}, std::move(ub));
  }
  // 1:
  static ShapeSpec MakeAssumed() {
    return ShapeSpec(Bound{1}, Bound::Deferred());
  }
  // lb:
  static ShapeSpec MakeAssumed(Bound &&lb) {
    return ShapeSpec(std::move(lb), Bound::Deferred());
  }
  // :
  static ShapeSpec MakeDeferred() {
    return ShapeSpec(Bound::Deferred(), Bound::Deferred());
  }
  // 1:*
  static ShapeSpec MakeImplied() {
    return ShapeSpec(Bound{1}, Bound::Assumed());
  }
  // lb:*
  static ShapeSpec MakeImplied(Bound &&lb) {
    return ShapeSpec(std::move(lb), Bound::Assumed());
  }
  // ..
  static ShapeSpec MakeAssumedRank() {
    return ShapeSpec(Bound::Assumed(), Bound::Assumed());
  }

  ShapeSpec(ShapeSpec &&) = default;
  ShapeSpec &operator=(ShapeSpec &&) = default;
  ShapeSpec Clone() const { return ShapeSpec{lb_.Clone(), ub_.Clone()}; }

  bool isExplicit() const { return ub_.isExplicit(); }
  bool isDeferred() const { return lb_.isDeferred(); }
  const Bound &lbound() const { return lb_; }
  const Bound &ubound() const { return ub_; }

private:
  ShapeSpec(Bound &&lb, Bound &&ub) : lb_{std::move(lb)}, ub_{std::move(ub)} {}
  Bound lb_;
  Bound ub_;
  friend ExprResolver;
  friend std::ostream &operator<<(std::ostream &, const ShapeSpec &);
};

using ArraySpec = std::list<ShapeSpec>;

class GenericSpec {
public:
  enum Kind {
    GENERIC_NAME,
    OP_DEFINED,
    ASSIGNMENT,
    READ_FORMATTED,
    READ_UNFORMATTED,
    WRITE_FORMATTED,
    WRITE_UNFORMATTED,
    OP_ADD,
    OP_AND,
    OP_CONCAT,
    OP_DIVIDE,
    OP_EQ,
    OP_EQV,
    OP_GE,
    OP_GT,
    OP_LE,
    OP_LT,
    OP_MULTIPLY,
    OP_NE,
    OP_NEQV,
    OP_NOT,
    OP_OR,
    OP_POWER,
    OP_SUBTRACT,
    OP_XOR,
  };
  static GenericSpec IntrinsicOp(Kind kind) {
    return GenericSpec(kind, nullptr);
  }
  static GenericSpec DefinedOp(const SourceName &name) {
    return GenericSpec(OP_DEFINED, &name);
  }
  static GenericSpec GenericName(const SourceName &name) {
    return GenericSpec(GENERIC_NAME, &name);
  }

  const Kind kind() const { return kind_; }
  const SourceName &genericName() const {
    CHECK(kind_ == GENERIC_NAME);
    return *name_;
  }
  const SourceName &definedOp() const {
    CHECK(kind_ == OP_DEFINED);
    return *name_;
  }

private:
  GenericSpec(Kind kind, const SourceName *name) : kind_{kind}, name_{name} {}
  const Kind kind_;
  const SourceName *const name_;  // only for GENERIC_NAME & OP_DEFINED
  friend std::ostream &operator<<(std::ostream &, const GenericSpec &);
};

// A type parameter value: integer expression or assumed or deferred.
class ParamValue {
public:
  static const ParamValue Assumed() { return Category::Assumed; }
  static const ParamValue Deferred() { return Category::Deferred; }
  ParamValue(const parser::Expr &);
  bool isExplicit() const { return category_ == Category::Explicit; }
  bool isAssumed() const { return category_ == Category::Assumed; }
  bool isDeferred() const { return category_ == Category::Deferred; }
  const LazyExpr &GetExplicit() const { return expr_; }
  void ResolveExplicit(SemanticsContext &);

private:
  enum class Category { Explicit, Deferred, Assumed };
  ParamValue(Category category) : category_{category} {}
  Category category_;
  LazyExpr expr_;
  friend std::ostream &operator<<(std::ostream &, const ParamValue &);
};

class DerivedTypeSpec {
public:
  using listType = std::list<std::pair<std::optional<SourceName>, ParamValue>>;
  explicit DerivedTypeSpec(const SourceName &name) : name_{&name} {}
  DerivedTypeSpec() = delete;
  const SourceName &name() const { return *name_; }
  const Scope *scope() const { return scope_; }
  void set_scope(const Scope &);
  listType &paramValues() { return paramValues_; }
  const listType &paramValues() const { return paramValues_; }
  void AddParamValue(ParamValue &&value) {
    paramValues_.emplace_back(std::nullopt, std::move(value));
  }
  void AddParamValue(const SourceName &name, ParamValue &&value) {
    paramValues_.emplace_back(name, std::move(value));
  }

private:
  const SourceName *name_;
  const Scope *scope_{nullptr};
  listType paramValues_;
  friend std::ostream &operator<<(std::ostream &, const DerivedTypeSpec &);
};

class DeclTypeSpec {
public:
  enum Category { Intrinsic, TypeDerived, ClassDerived, TypeStar, ClassStar };

  // intrinsic-type-spec or TYPE(intrinsic-type-spec)
  DeclTypeSpec(const IntrinsicTypeSpec &);
  // TYPE(derived-type-spec) or CLASS(derived-type-spec)
  DeclTypeSpec(Category, DerivedTypeSpec &);
  // TYPE(*) or CLASS(*)
  DeclTypeSpec(Category);
  DeclTypeSpec() = delete;

  bool operator==(const DeclTypeSpec &) const;
  bool operator!=(const DeclTypeSpec &that) const { return !operator==(that); }

  Category category() const { return category_; }
  const IntrinsicTypeSpec &intrinsicTypeSpec() const;
  DerivedTypeSpec &derivedTypeSpec();
  const DerivedTypeSpec &derivedTypeSpec() const;

private:
  Category category_;
  union TypeSpec {
    TypeSpec() : derived{nullptr} {}
    TypeSpec(IntrinsicTypeSpec intrinsic) : intrinsic{intrinsic} {}
    TypeSpec(DerivedTypeSpec *derived) : derived{derived} {}
    IntrinsicTypeSpec intrinsic;
    DerivedTypeSpec *derived;
  } typeSpec_;
};
std::ostream &operator<<(std::ostream &, const DeclTypeSpec &);

// This represents a proc-interface in the declaration of a procedure or
// procedure component. It comprises a symbol (representing the specific
// interface), a decl-type-spec (representing the function return type),
// or neither.
class ProcInterface {
public:
  const Symbol *symbol() const { return symbol_; }
  const DeclTypeSpec *type() const { return type_ ? &*type_ : nullptr; }
  void set_symbol(const Symbol &symbol);
  void set_type(const DeclTypeSpec &type);

private:
  const Symbol *symbol_{nullptr};
  std::optional<DeclTypeSpec> type_;
};

// Resolve expressions in symbols.
void ResolveSymbolExprs(SemanticsContext &);
}

#endif  // FORTRAN_SEMANTICS_TYPE_H_
