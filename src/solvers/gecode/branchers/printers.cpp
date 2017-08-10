/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, SICS Swedish ICT AB
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


#include "printers.hpp"

void print_cluster_connection_decision(const Space &s, const Brancher&,
                                       unsigned int alternative,
                                       BoolVar, global_cluster gc, const int&,
                                       std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  vector<operand> ps = m.input->clusters[gc];
  if (alternative == 1) out << "!";
  out << "x(" << show(ps, ",", "p", "{}") << ")";
}

void print_cluster_disconnection_decision(const Space &s, const Brancher&,
                                          unsigned int alternative,
                                          BoolVar, global_cluster gc, const int&,
                                          std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  vector<operand> ps = m.input->clusters[gc];
  if (alternative == 0) out << "!";
  out << "x(" << show(ps, ",", "p", "{}") << ")";
}

void print_allocation_decision(const Space &s, const Brancher&,
                               unsigned int alternative,
                               SetVar, int g, const int& rs,
                               std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  congruence c = m.input->regular[g];
  out << "r(";
  operand fp = m.input->congr[c][0];
  if (m.input->congr[c].size() == 1)
    out << "p" << fp;
  else {
    operand lp = m.input->congr[c][m.input->congr[c].size() - 1];
    out << "{p" << fp << "..p" << lp << "}";
  }
  out << ") "
      << (alternative == 0 ? "->" : "-!>")
      << " " << m.input->spacename[rs];
}

void print_activation_decision(const Space &s, const Brancher&,
                               unsigned int alternative,
                               BoolVar, activation_class ac, const int&,
                               std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  set<operation> os = m.input->activation_class_operations[ac];
  if (alternative == 1) out << "!";
  out << "a(" << show(os, ",", "o", "{}") << ")";
}

void print_hinted_avoidance_decision(const Space &s, const Brancher&,
                                     unsigned int alternative,
                                     BoolVar, int hi, const int&,
                                     std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  operand p = get<0>(m.input->avoidhints[hi]);
  vector<register_atom> as = get<1>(m.input->avoidhints[hi]);
  out << "r(p" << p << ") ";
  out << (alternative == 0 ? "-!>" : "->");
  out << " {"
      << show_register(*as.begin(), m.input->operand_width[p], m.input)
      << ".."
      << show_register(*as.rbegin(), m.input->operand_width[p], m.input)
      << "}";
}

void print_hinted_assignment_decision(const Space &s, const Brancher&,
                                      unsigned int alternative,
                                      BoolVar, int hi, const int&,
                                      std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  operand p = m.input->assignhints[hi][0];
  register_atom ra = m.input->assignhints[hi][1];
  out << "r(p" << p << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << show_register(ra, m.input->operand_width[p], m.input);
}

void print_alignment_decision(const Space &s, const Brancher&,
                              unsigned int alternative,
                              BoolVar, int a, const int&,
                              std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  operand p = m.input->pairs[a].first, q = m.input->pairs[a].second;
  if (alternative == 1) out << "!";
  out << "oa(p" << p << ", p" << q  << ")";
}

void print_slack_assignment_decision(const Space &s, const Brancher&,
                                     unsigned int alternative,
                                     IntVar, int goi, const int& slack,
                                     std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  operand p = -1;
  for (operand q : m.input->P)
    if (m.input->global_operand[q] && m.input->global_index[q] == goi) {
      p = q;
      break;
    }
  out << "s(p" << p << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << slack;
}

void print_assignment_decision(const Space &s, const Brancher&,
                               unsigned int alternative,
                               IntVar, int g, const int& ra,
                               std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  congruence c = m.input->regular[g];
  out << "r(";
  operand fp = m.input->congr[c][0];
  if (m.input->congr[c].size() == 1)
    out << "p" << fp;
  else {
    operand lp = m.input->congr[c][m.input->congr[c].size() - 1];
    out << "{p" << fp << "..p" << lp << "}";
  }
  out << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << show_register(ra, m.input->operand_width[fp], m.input);
}

void print_inactive_decision(const Space &s, const Brancher&,
                             unsigned int alternative,
                             BoolVar, int oi, const int&,
                             std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  operation o = m.input->in[m.b] + oi;
  if (alternative == 0) out << "!";
  out << "a(o" << o << ")";
}

void print_active_decision(const Space &s, const Brancher& bh,
                           unsigned int alternative,
                           BoolVar b, int oi, const int& v,
                           std::ostream& out) {
  print_inactive_decision(s, bh, !alternative, b, oi, v, out);
}

void print_global_inactive_decision(const Space &, const Brancher&,
                                    unsigned int alternative,
                                    BoolVar, operation o, const int&,
                                    std::ostream& out) {
  if (alternative == 0) out << "!";
  out << "a(o" << o << ")";
}

void print_instruction_decision(const Space &s, const Brancher&,
                              unsigned int alternative,
                              IntVar, int oi, const int& ii,
                              std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  operation o = m.input->in[m.b] + oi;
  out << "i(o" << o << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << m.input->insname[m.input->instructions[o][ii]];
}

void print_global_instruction_decision(const Space &s, const Brancher&,
                                       unsigned int alternative,
                                       IntVar, operation o, const int& ii,
                                       std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  out << "i(o" << o << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << m.input->insname[m.input->instructions[o][ii]];
}

void print_temporary_decision(const Space &s, const Brancher&,
                              unsigned int alternative,
                              IntVar, int pi, const int& ti,
                              std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  operand p = m.input->groupcopyrel[m.b][pi];
  out << "y(p" << p << ") "
      << (alternative == 0 ? "=" : "!=")
      << "t" << m.input->temps[p][ti];
}

void print_global_temporary_decision(const Space &s, const Brancher&,
                                     unsigned int alternative,
                                     IntVar, operand p, const int& ti,
                                     std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  out << "y(p" << p << ") "
      << (alternative == 0 ? "=" : "!=")
      << "t" << m.input->temps[p][ti];
}

void print_register_decision(const Space &s, const Brancher&,
                             unsigned int alternative,
                             IntVar, int ti, const int& ra,
                             std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  temporary t = m.input->tmp[m.b][0] + ti;
  out << "r(t" << t << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << show_register(ra, m.input->width[t], m.input);
}

void print_global_register_decision(const Space &s, const Brancher&,
                                    unsigned int alternative,
                                    IntVar, temporary t, const int& ra,
                                    std::ostream& out) {
  const Model& m = static_cast<const Model&>(s);
  out << "r(t" << t << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << show_register(ra, m.input->width[t], m.input);
}

void print_cycle_decision(const Space &s, const Brancher&,
                          unsigned int alternative,
                          IntVar, int oi, const int& c,
                          std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  operation o = m.input->in[m.b] + oi;
  out << "c(o" << o << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << c;
}

void print_global_cycle_decision(const Space &, const Brancher&,
                                 unsigned int alternative,
                                 IntVar, operation o, const int& c,
                                 std::ostream& out) {
  out << "c(o" << o << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << c;
}

void print_cost_decision(const Space &s, const Brancher&,
                         unsigned int alternative,
                         IntVar, int, const int& c,
                         std::ostream& out) {
  const LocalModel& m = static_cast<const LocalModel&>(s);
  out << "f(b" << m.b << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << c;
}

void print_global_cost_decision(const Space &, const Brancher&,
                                unsigned int alternative,
                                IntVar, int n, const int& c,
                                std::ostream& out) {
  out << "cost(" << n << ") "
      << (alternative == 0 ? "=" : "!=")
      << " " << c;
}
