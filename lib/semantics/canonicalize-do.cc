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

#include "canonicalize-do.h"
#include "../common/idioms.h"
#include "../parser/parse-tree-visitor.h"
#include <variant>
#include <vector>

namespace Fortran::parser {

struct CanonicalizationOfDoLoops {
  template<typename T> bool Pre(T &) { return true; }
  template<typename T> void Post(T &) {}
  void Post(Block &);
};

// When an execution part construct is a label DO statement, return its label,
// else zero.
static Label GetLabelDoLoopLabel(const ExecutionPartConstruct &construct) {
  if (const auto *executable{std::get_if<ExecutableConstruct>(&construct.u)}) {
    if (const auto *labelDo{
            std::get_if<Statement<common::Indirection<LabelDoStmt>>>(
                &executable->u)}) {
      return std::get<Label>(labelDo->statement->t);
    }
  }
  return 0;
}

// When an executable construct in a block is a "bare" END DO statement, it's
// present there in the block because the parser found that its label was
// referenced on an earlier label DO statement.  Replace it with a CONTINUE
// statement so that the label remains defined.  Returns the label, or zero.
static Label ReplaceEndDoStmt(ExecutableConstruct &construct) {
  if (auto *endDo{std::get_if<Statement<common::Indirection<EndDoStmt>>>(
          &construct.u)}) {
    CHECK(endDo->label.has_value() || !"unlabeled END DO in block");
    Label label{*endDo->label};
    CHECK(label > 0);
    construct.u = Statement<ActionStmt>{label, ContinueStmt{}};
    return label;
  } else {
    return 0;
  }
}

// If an executable construct is an action statement with a label, or a
// DO construct whose END DO has a label, return the label, else zero.
static Label GetPossibleLoopEndLabel(const ExecutableConstruct &construct) {
  if (const auto *actionStmt{
          std::get_if<Statement<ActionStmt>>(&construct.u)}) {
    if (actionStmt->label.has_value()) {
      return *actionStmt->label;
    }
  } else if (const auto *doConstruct{
                 std::get_if<common::Indirection<DoConstruct>>(&construct.u)}) {
    const auto &endDo{std::get<Statement<EndDoStmt>>((*doConstruct)->t)};
    if (endDo.label.has_value()) {
      return *endDo.label;
    }
  }
  return 0;
}

// Extracts a sequence of ExecutionPartConstructs, which constitutes the
// body of a DO loop, from a block, and returns it as a new block.
static Block ExtractDoLoopBody(
    Block &block, Block::iterator doStmt, Block::iterator nextAfterLoop) {
  Block body;
  body.splice(body.begin(), block, ++doStmt, nextAfterLoop);
  return body;
}

// Given an original label DO statement and its extracted loop body, repackage
// their contents in a new block DO/END DO construct with the same construct
// name, loop control, and body.
static DoConstruct MakeBlockDoLoop(
    ExecutionPartConstruct &&labelDo, Block &&body) {
  auto &doConstruct{*std::get_if<ExecutableConstruct>(&labelDo.u)};
  auto &doStmt{
      *std::get_if<Statement<common::Indirection<LabelDoStmt>>>(&doConstruct.u)
           ->statement};
  return DoConstruct{
      Statement<NonLabelDoStmt>{std::nullopt,
          NonLabelDoStmt{std::move(std::get<std::optional<Name>>(doStmt.t)),
              std::move(std::get<std::optional<LoopControl>>(doStmt.t))}},
      std::move(body),
      Statement<EndDoStmt>{std::nullopt, EndDoStmt{std::nullopt}}};
}

static void ConvertToBlockDoConstruct(
    Block &block, Block::iterator labelDo, Block::iterator nextAfter) {
  auto &construct{*std::get_if<ExecutableConstruct>(&labelDo->u)};
  construct.u = common::Indirection<DoConstruct>{MakeBlockDoLoop(
      std::move(*labelDo), ExtractDoLoopBody(block, labelDo, nextAfter))};
}

// Convert labeled DO loops in a block to DO/END DO block-structured
// loops in place.  All nested execution parts have already been so converted.
void CanonicalizationOfDoLoops::Post(Block &block) {
  std::vector<Block::iterator> pendingDoLoopStack;
  Block::iterator blockEnd{block.end()};
  Block::iterator next;
  for (Block::iterator iter{block.begin()}; iter != blockEnd; iter = next) {
    // Capture the next ExecutionPartConstruct to process now before doing
    // anything that might invalidate "iter" as an iterator within the block.
    next = iter;
    ++next;
    if (GetLabelDoLoopLabel(*iter) > 0) {
      // At a label DO statement: push its position upon the stack so that its
      // body can be extracted later and repackaged when its terminal label
      // is found.
      pendingDoLoopStack.push_back(iter);
    } else if (auto *construct{std::get_if<ExecutableConstruct>(&iter->u)}) {
      if (Label endDoLabel{ReplaceEndDoStmt(*construct)}) {
        CHECK(!pendingDoLoopStack.empty());
        CHECK(endDoLabel == GetLabelDoLoopLabel(*pendingDoLoopStack.back()));
      }
      if (Label label{GetPossibleLoopEndLabel(*construct)}) {
        // There can be multiple label DO loops that are terminated by this
        // label.  Rewrite them from innermost to outermost in place by
        // extracting their bodies and repackaging them as DoConstructs.
        for (; !pendingDoLoopStack.empty(); pendingDoLoopStack.pop_back()) {
          Block::iterator doIter{pendingDoLoopStack.back()};
          if (GetLabelDoLoopLabel(*doIter) == label) {
            ConvertToBlockDoConstruct(block, doIter, next);
            CHECK(++doIter == next);
          } else {
            break;
          }
        }
      }
    }
  }
  if (!pendingDoLoopStack.empty()) {
    common::die("INTERNAL: CanonicalizationOfDoLoops: %zd loops remain stacked "
                "at end of block; topmost label is %d",
        pendingDoLoopStack.size(),
        GetLabelDoLoopLabel(*pendingDoLoopStack.back()));
  }
}

void CanonicalizeDo(Program &program) {
  CanonicalizationOfDoLoops mutator;
  Walk(program, mutator);
}
}
