%
%  Main authors:
%    Mats Carlsson <mats.carlsson@ri.se>
%
%  Contributing authors:
%    Roberto Castaneda Lozano <rcas@acm.org>
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
:- use_module(library(codesio)).

objective([0|Source], F, I2DU, MaxC, NoInsn, scalar_product(F,Terms), BBs) :- % (8)
	append(Source, [NoInsn], Sink),
	(   foreach(c(S1),Terms),
	    foreach(M,MaxC),
	    foreach(Fi,F),
	    foreach(NM-bb(Src..S1,T1..T2),BBs1),
	    foreach(_-BB,BBs2),
	    foreach(BB,BBs),
	    foreach(S,Sink),
	    fromto(0,Src,S,_),
	    param(I2DU)
	do  S1 is S-1,
	    NM is -M*Fi,
	    (   for(I,Src,S1),
		fromto(Subsets,[Ut,Dt|Subsets1],Subsets1,[]),
		param(I2DU)
	    do  avl_fetch(I, I2DU, Dt-Ut)
	    ),
	    append(Subsets, Bag),
	    (   min_member(T1, Bag),
	        max_member(T2, Bag) -> true
	    ;   T1=1, T2=0
	    )
	),
	keysort(BBs1, BBs2).

get_crs(I, T, CRs, I2DU, I2Atom, LB..UB) :-
	memberchk([I|Tail], CRs),
	avl_fetch(I, I2DU, Di-Ui),
	append(Ui, Di, UDi),
	nth0(J, UDi, T), !,
	nth0(J, Tail, RS),
	avl_fetch(RS, I2Atom, [LB,UB]).
get_crs(_, _, _, _, _, {}).

precedence_ctr(E) -->		% (9)
	(   foreach([I,J,L],E)
	do  [c(I) #=< c(J)],
	    (   {L=:=0} -> []
	    ;   [c(I) + L*s(I) + L*s(J) - L #=< c(J)]
	    )
	).

s_sink_aux(E, I2Rus, [0|Source], NoInsn) -->
	{append(Source, [NoInsn], Sink)},
	(   foreach(Srcb,[0|Source]),
	    foreach(Sinkb,Sink),
	    param(E,I2Rus)
	do  {Sink1 is Sinkb-1},
	    {Sink2 is Sinkb-2},
	    {   memberchk([Sink2,Sink1,D], E) -> true
	    ;   D = 1
	    },
	    (   for(I,Srcb,Sink1),
		fromto(Ss1,Ss2,Ss3,[]),
		param(I2Rus)
	    do  {   avl_fetch(I, I2Rus, []) -> Ss2 = Ss3
		;   Ss2 = [s(I)|Ss3]
		}
	    ),
	    [sum(Ss1) + D #= c(Sink1)]
	).


resource_ctr(Cap, I2Rus, [0|Source], NoInsn) --> % (10)
	{append(Source, [NoInsn], Sink)},
	(   foreach(Capr,Cap),
	    count(R,0,_),
	    param(Source,Sink,I2Rus)
	do  (   foreach(Srcb,[0|Source]),
		foreach(Sinkb,Sink),
		param(R,I2Rus,Capr)
	    do  (   for(I,Srcb,Sinkb-1),
		    fromto(Tasks1,Tasks2,Tasks3,[]),
		    fromto(Us1,Us2,Us3,[]),
		    fromto(Capr,GCD1,GCD2,GCD),
		    param(R,I2Rus)
		do  (   {avl_fetch(I, I2Rus, Triples)},
			{member([R,Uir,Oir], Triples)} ->
			{Tasks2 = [task(c(I),Oir,U*s(I))|Tasks3]},
			{Us2 = [Uir-U|Us3]},
			{GCD2 is gcd(GCD1,Uir)}
		    ;   {Tasks2 = Tasks3},
			{Us2 = Us3},
			{GCD1 = GCD2}
		    )
		),
		(   foreach(X-Y,[Capr-Caps|Us1]),
		    param(GCD)
		do  {Y is X//GCD}
		),
		(   {Tasks1=[]} -> []
		;   {Tasks1=[_]} -> []
		;   [cumulative(Tasks1,Caps)]
		)
	    )
	).


json2avl(Filename, AVL) :-
	see(Filename),
	read_line(Line1),
	(   fromto(Line1,Line2,Line3,end_of_file),
	    fromto(Lines1,[[10],Line2|Lines2],Lines2,[".\n"])
	do  read_line(Line3)
	),
	seen,
	append(Lines1, LongLine),
	read_from_codes(LongLine, {Term}),
	term_to_keylist(Term, KL1, []),
	keysort(KL1, KL2),
	ord_list_to_avl(KL2, AVL).

avl2json(Filename, AVL) :-
	avl_to_list(AVL, KL1),
	length(KL1, Len),
	tell(Filename),
	(   foreach(Key-Val,KL1),
	    count(I,1,_),
	    param(Len)
	do  (I>1 -> true ; write('{\n')),
	    (I<Len -> Sep = ',' ; Sep = '}'),
	    format(' "~w": ~w~w\n', [Key,Val,Sep])
	),
	told.

term_to_keylist((T1,T2)) --> !,
	term_to_keylist(T1),
	term_to_keylist(T2).
term_to_keylist(Qname:T) --> [Name-T],
	{atom_codes(Name, Qname)}.

replace_file_extension(InputName, BeforeExt, AfterExt, OutputName) :-
    name(InputName, InputString),
    append([BaseString, BeforeExt], InputString),
    append([BaseString, AfterExt], OutputString),
    name(OutputName, OutputString).

test_cases([addnoise,db,fact,/*fft,*/gngauss,hack2b,ifthenelse,insertion,loop]).

end_of_file.

/***
../addnoise
../db - obj=762, sicstus=12
../fact - obj=109, sicstus=0
../gngauss
../hack2b - obj=84, sicstus=1
../ifthenelse - obj=14, sicstus=0
../insertion - obj=2322, sicstus=0
../loop - obj=68, sicstus=0
***/
