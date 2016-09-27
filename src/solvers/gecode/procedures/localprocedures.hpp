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


#ifndef __LOCAL_PROCEDURES__
#define __LOCAL_PROCEDURES__

#include "models/localmodel.hpp"
#include "procedures/commonprocedures.hpp"

using namespace std;
using namespace Gecode;

// Creates a local problem for block b out of the global solution g1
Solution<LocalModel> local_problem(GlobalModel * g1, block b);

// Gives a local solution by running the local solver portfolio
Solution<LocalModel>
solve_local_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration);

// Gives a local solution by running the generic local solver portfolio
Solution<LocalModel>
solve_generic_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration);

// Gives a local solution by running the custom local solver portfolio
Solution<LocalModel>
solve_custom_portfolio(LocalModel * base, GIST_OPTIONS * lo, int iteration);

// Gives a local solution applying a certain search strategy
Solution<LocalModel>
solve_local(LocalModel * base, char search, GIST_OPTIONS * lo, int iteration);

// Prefix for debug output
string local(block b);

#endif
