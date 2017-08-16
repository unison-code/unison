%
%  Main authors:
%    Mats Carlsson <mats.carlsson@ri.se>
%
%  This file is part of Unison, see http://unison-code.github.io
%
%  Copyright (c) 2016, RISE SICS AB
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%  1. Redistributions of source code must retain the above copyright notice,
%     this list of conditions and the following disclaimer.
%  2. Redistributions in binary form must reproduce the above copyright notice,
%     this list of conditions and the following disclaimer in the documentation
%     and/or other materials provided with the distribution.
%  3. Neither the name of the copyright holder nor the names of its contributors
%     may be used to endorse or promote products derived from this software
%     without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
:- use_module(library(lists)).

:- op(550, xfx, ..).

print_out_json :-
	bb(BBs),
	s(Ss),
	c(Cs),
	p(Ps),
	(   foreach(bb(Src..Sink,_),BBs),
	    foreach(Clusters,Clusterss),
	    param(Ss,Cs)
	do  (   for(J,Src,Sink),
		fromto(KL3,KL4,KL5,[]),
		fromto(0,Max1,Max2,Max3),
		param(Ss,Cs)
	    do  nth0(J, Ss, Sj),
		nth0(J, Cs, Cj),
		(Sj=:=0 -> KL4 = KL5 ; KL4 = [Cj-J|KL5]),
		Max2 is max(Max1,Cj)
	    ),
	    (   for(K,0,Max3),
		fromto(KL1,[K - -1|KL2],KL2,KL3),
		foreach(_-[-1|Cluster],KL7),
		foreach(Cluster,Clusters)
	    do  true
	    ),
	    keysort(KL1, KL6),
	    keyclumped(KL6, KL7)
	),
	format('{"issues": ~w,\n "placements": ~w}\n', [Clusterss,Ps]).

:- initialization
	consult(user),
	print_out_json,
	halt.
