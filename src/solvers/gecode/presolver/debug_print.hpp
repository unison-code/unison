/*
 *  Main authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Roberto Castaneda Lozano <rcas@acm.org>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2015-2016, Erik Ekstrom
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
 *
 * Set of helper functions to ease debugging and printing of complex data.
 */

#include <set>
#include <vector>
#include <iostream>

#ifndef __DEBUG_PRINT_H__
#define __DEBUG_PRINT_H__

using namespace std;

template<typename T>
void _debug_print(T e);
template<typename T>
void _debug_print(set<T> v);
template<typename T>
void _debug_print(vector<T> v);

template<typename T>
void _debug_print(T e){
  cout << e;
}

template<typename T>
void _debug_print(vector<T> v){
  cout << "[";
  for(auto it = v.begin(); it != v.end(); it++){
    _debug_print(*it);
    if( next(it) != v.end() ) cout << ",";
  }
  cout << "]";
}

template<typename T>
void _debug_print(set<T> v){
  cout << "[";
  for(auto it = v.begin(); it != v.end(); it++){
    _debug_print(*it);
    if( next(it) != v.end() ) cout << ",";
  }
  cout << "]";
}

template<typename T>
void debug_print(T e){
  _debug_print(e);
  cout << endl;
}

template<typename T>
void debug_print(T e, string prefix){
  cout << prefix;
  _debug_print(e);
  cout << endl;
}

#endif
