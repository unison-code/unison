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


#include "definitions.hpp"

Temporand::Temporand() {}
Temporand::Temporand(int id, TemporandType t) : _id(id), _type(t) {}
Temporand::Temporand(const Temporand& t) : _id(t.id()), _type(t.type()) {}

bool Temporand::operator==(const Temporand& t) const {
  return ((_type == t.type()) && (_id == t.id()));
}

bool Temporand::operator!=(const Temporand& t) const {
  return !(*this == t);
}

bool Temporand::operator<(const Temporand& t) const {
  if (type() < t.type()) return true;
  else if (type() == t.type()) return (id() < t.id());
  return false;
}

// friend function, for simpler debuging
std::ostream& operator<<(std::ostream& os, const Temporand& t){
  if(t.type() == TEMPORAND_NONE){
    os << t.id();
    return os;
  }

  // Tag id with type in output
  else if(t.is_operand()) os << "p";
  else if(t.is_temporary()) os << "t";
  else if(t.is_register()) os << "r";
  os << "(" << t.id() << ")";
  return os;
}

int Temporand::id() const {
  return _id;
}

TemporandType Temporand::type() const {
  return _type;
}

bool Temporand::is_operand() const {
  return (type() == TEMPORAND_OPERAND);
}

bool Temporand::is_temporary() const {
  return (type() == TEMPORAND_TEMPORARY);
}

bool Temporand::is_register() const {
  return (type() == TEMPORAND_REGISTER);
}

// Static member function
Temporand Temporand::p(int id) {
  return Temporand(id, TEMPORAND_OPERAND);
}

// Static member function
Temporand Temporand::t(int id) {
  return Temporand(id, TEMPORAND_TEMPORARY);
}

// Static member function
Temporand Temporand::r(int id){
  return Temporand(id, TEMPORAND_REGISTER);
}

// Static member function
Temporand Temporand::n(int id){
  return Temporand(id, TEMPORAND_NONE);
}
