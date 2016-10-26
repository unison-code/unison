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


#ifndef __PRINTER_BRANCHER__
#define __PRINTER_BRANCHER__

#include <vector>

#include <gecode/int.hh>

#include "common/util.hpp"
#include "common/definitions.hpp"
#include "models/localmodel.hpp"

using namespace std;
using namespace Gecode;

void print_cluster_connection_decision(const Space &s, const Brancher& b,
                                       unsigned int alternative,
                                       BoolVar, global_cluster gc, const int&,
                                       std::ostream& out);

void print_cluster_disconnection_decision(const Space &s, const Brancher& b,
                                       unsigned int alternative,
                                       BoolVar, global_cluster gc, const int&,
                                       std::ostream& out);

void print_allocation_decision(const Space &s, const Brancher& b,
                               unsigned int alternative,
                               SetVar, int g, const int& rs,
                               std::ostream& out);

void print_activation_decision(const Space &s, const Brancher& b,
                               unsigned int alternative,
                               BoolVar, activation_class ac, const int&,
                               std::ostream& out);

void print_hinted_avoidance_decision(const Space &s, const Brancher& b,
                                      unsigned int alternative,
                                      BoolVar, int hi, const int&,
                                      std::ostream& out);

void print_hinted_assignment_decision(const Space &s, const Brancher& b,
                                      unsigned int alternative,
                                      BoolVar, int hi, const int&,
                                      std::ostream& out);

void print_alignment_decision(const Space &s, const Brancher& b,
                              unsigned int alternative,
                              BoolVar, int ai, const int&,
                              std::ostream& out);

void print_assignment_decision(const Space &s, const Brancher& b,
                               unsigned int alternative,
                               IntVar, int g, const int& ra,
                               std::ostream& out);

void print_inactive_decision(const Space &s, const Brancher& b,
                             unsigned int alternative,
                             BoolVar, int oi, const int&,
                             std::ostream& out);

void print_active_decision(const Space &s, const Brancher& b,
                           unsigned int alternative,
                           BoolVar, int oi, const int&,
                           std::ostream& out);

void print_global_inactive_decision(const Space &s, const Brancher& b,
                                    unsigned int alternative,
                                    BoolVar, operation o, const int&,
                                    std::ostream& out);

void print_instruction_decision(const Space &s, const Brancher& b,
                              unsigned int alternative,
                              IntVar, int oi, const int& ii,
                              std::ostream& out);

void print_global_instruction_decision(const Space &s, const Brancher& b,
                                       unsigned int alternative,
                                       IntVar, operation o, const int& ii,
                                       std::ostream& out);

void print_temporary_decision(const Space &s, const Brancher& b,
                              unsigned int alternative,
                              IntVar, int pi, const int& ti,
                              std::ostream& out);

void print_global_temporary_decision(const Space &s, const Brancher& b,
                                     unsigned int alternative,
                                     IntVar, operand p, const int& ti,
                                     std::ostream& out);

void print_register_decision(const Space &s, const Brancher& b,
                             unsigned int alternative,
                             IntVar, int ti, const int& ra,
                             std::ostream& out);

void print_global_register_decision(const Space &s, const Brancher& b,
                                    unsigned int alternative,
                                    IntVar, temporary t, const int& ra,
                                    std::ostream& out);

void print_cycle_decision(const Space &s, const Brancher& b,
                          unsigned int alternative,
                          IntVar, int oi, const int& c,
                          std::ostream& out);

void print_global_cycle_decision(const Space &s, const Brancher& b,
                                 unsigned int alternative,
                                 IntVar, operation o, const int& c,
                                 std::ostream& out);

void print_cost_decision(const Space &s, const Brancher& b,
                         unsigned int alternative,
                         IntVar, int oi, const int& c,
                         std::ostream& out);

void print_global_cost_decision(const Space &s, const Brancher& b,
                                unsigned int alternative,
                                IntVar, int oi, const int& c,
                                std::ostream& out);

#endif
