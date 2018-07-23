/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#ifndef __PRESOLVER_OPTIONS__
#define __PRESOLVER_OPTIONS__

#include <gecode/driver.hh>

using namespace Gecode;
using namespace std;

class PresolverOptions : public InstanceOptions {

protected:

  // Interface

  Driver::StringValueOption _output_file; // Output file
  Driver::StringValueOption _dzn_file; // .dzn file
  Driver::DoubleOption _timeout; // Timeout
  Driver::BoolOption _verbose; // Verbose mode
  Driver::BoolOption _regions; // Find regions
  Driver::BoolOption _tabling; // Do tabling

  // Testing

  Driver::BoolOption _test; // Test the generated parameters against the input ones

public:

  PresolverOptions(void);

  string output_file(void) const {return _output_file.value();}
  string dzn_file(void) const {return _dzn_file.value();}
  double timeout(void) const {return _timeout.value();}
  bool verbose(void) const {return _verbose.value();}
  bool regions(void) const {return _regions.value();}
  bool tabling(void) const {return _tabling.value();}

  bool test(void) const {return _test.value();}

};

#endif
