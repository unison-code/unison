/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, RISE SICS AB
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *  1. Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright notice,
 *     this list of conditions and the following disclaimer in the documentation
 *     and/or other materials provided with the distribution.
 *  3. Neither the name of the copyright holder nor the names of its
 *     contributors may be used to endorse or promote products derived from this
 *     software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 */


#include "selectioninspector.hpp"

SelectionInspector::SelectionInspector() :
  ConsoleInspector("Temporary and instruction selection") {}

void SelectionInspector::printb(const Model& m, ostream & pOs, block b) {
  for (operation o : m.input->ops[b]) {
    vector<QString> dps, ups;
    for (operand p : m.input->operands[o]) {
      QString op = showOperand(m, p);
      if (m.input->use[p]) ups.push_back(op);
      else dps.push_back(op);
    }
    pOs << "o" << o << ":\t"
        << showOperands(dps).toUtf8().constData() << " <- "
        << showInstructions(o, m.i(o), m.input) << " "
        << showOperands(ups).toUtf8().constData() << endl;
  }
}

QString SelectionInspector::showOperand(const Model& m, operand p) {
  vector<QString> ts;
  for (IntVarValues yi(m.y(p)); yi(); ++yi) {
    temporary t = m.input->temps[p][yi.val()];
    ts.push_back(t == NULL_TEMPORARY ? "-" : "t" + QString::number(t));
  }
  QString
    tInfo("p" + QString::number(p) + "{" + sepBy(ts, QString(", ")) + "}");
  QString uInfo("");
  if (m.input->use[p]) {
    uInfo += "<";
    for (temporary t : m.input->temps[p]) {
      if (m.u(p, t).assigned()) uInfo += m.u(p, t).val() ? "1" : "0";
      else uInfo += "?";
    }
    uInfo += ">";
  }
  return tInfo + (m.input->use[p] ? (" " + uInfo) : "");
}

QString SelectionInspector::showOperands(vector<QString> ops) {
  return "[" + sepBy(ops, QString(", ")) + "]";
}

void GlobalSelectionInspector::print(const Model& s, ostream & pOs) {

  const Model& m = static_cast<const Model&>(s);

  for (block b : m.input->B) {
    pOs << "b" << b << ":";
    pOs << endl << endl;
    printb(m, pOs, b);
    pOs << endl;
  }

}

void LocalSelectionInspector::print(const Model& s, ostream & pOs) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  printb(m, pOs, m.b);
}
