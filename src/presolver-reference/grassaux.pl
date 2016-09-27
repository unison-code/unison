%
%  Main authors:
%    Mats Carlsson <matsc@sics.se>
%
%  This file is part of Unison, see http://unison-code.github.io
%
%  Copyright (c) 2016, SICS Swedish ICT AB
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
:- use_module(library(clpfd)).

cycle_order(XSs, XCs, First, Later) :-
	(   foreach(S,XSs),
	    foreach(C,XCs),
	    fromto(First,First1,First2,[]),
	    fromto(Later,Later4,Later5,[])
	do  (   integer(C) -> First1 = First2, Later4 = Later5
	    ;   S=:=1 -> First1 = [C|First2], Later4 = Later5
	    ;   First1 = First2, Later4 = [C|Later5]
	    )
	).

value_precede_chain(Ints, X) :-
	(   foreach(B,X),
	    foreach(Set,Sets)
	do  fd_set(B, Set)
	),
	fdset_union(Sets, Union),
	fdset_to_list(Union, Values),
	(   fromto(Ints,Ints1,Ints2,[]),
	    foreach(sink(State),Sinks),
	    fromto(Arcs,Arcs1,Arcs5,[]),
	    count(State,1,_),
	    param(Values)
	do  Ints1 = [Y|Ints2],
	    (   Ints2==[] -> Nono = [], Arcs1 = Arcs2
	    ;   Nono = Ints1,
		Arcs1 = [arc(State,Y,State1)|Arcs2],
		State1 is State+1
	    ),
	    (   foreach(V,Values),
		fromto(Arcs2,Arcs3,Arcs4,Arcs5),
		param(Nono,State)
	    do  (   member(V, Nono) -> Arcs3 = Arcs4
		;   Arcs3 = [arc(State,V,State)|Arcs4]
		)
	    )
	),
	automaton(X, [source(1)|Sinks], Arcs).