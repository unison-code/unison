/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *    Noric Couderc <noric@sics.se>
 *    Erik Ekstrom <eeks@sics.se>
 *    Mats Carlsson <mats.carlsson@ri.se>
 *
 *  Contributing authors:
 *    Mikael Almgren <mialmg@kth.se>
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


#ifndef __PRESOLVE__
#define __PRESOLVE__

#include <algorithm>
#include "models/parameters.hpp"
#include "models/globalmodel.hpp"
#include "procedures/commonprocedures.hpp"
#include "presolver-options.hpp"
#include "across.hpp"
#include "auxiliary.hpp"
#include "before_presolver.hpp"
#include "congr.hpp"
#include "diff_temps.hpp"
#include "dominance.hpp"
#include "infeasible_presolver.hpp"
#include "last_use.hpp"
#include "precedences.hpp"
#include "presolver_asserts.hpp"
#include "quasi_adjacent.hpp"
#include "unsafe_temp.hpp"

using namespace std;

// Updates the input parameters after presolving the problem
void presolve(Parameters & input, PresolverOptions & options);

#endif
