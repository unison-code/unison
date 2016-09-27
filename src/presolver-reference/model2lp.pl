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
:- ensure_loaded(grass).

lp_style(fd, no).			% "yes" is useless
lp_style(binc, yes).		% best so far
lp_style(bincs, no).		% best so far
lp_style(binp, yes).		% best so far
lp_style(binlive, no).		% best so far
lp_style(liverange, weak).		% best so far
lp_style(value_precede, strong).	% off | weak | strong

json2lp(BaseName) :-
	atom_concat(BaseName, '.ext.json', InputName),
	atom_concat(BaseName, '.dir', DirName),
	atom_concat(BaseName, '.opt', OptName),
	atom_concat(BaseName, '.lp', OutputName),
	json2avl(InputName, AVL),
	model(AVL, Vars, Objective, Constraints, []),
	tell(DirName),
	write(' PR s*                  0010\n'),
	told,
	tell(OptName),
	format('COVERCUTS=0\nGOMCUTS=0\nreadprob ~w\nreaddir ~w.dir\nminim -gp\n',
	       [BaseName,BaseName]),
	told,
	tell(OutputName),
	model2lp(Vars, Objective, Constraints),
	told.

model2lp(vars(Vars,_), scalar_product(Coeffs,Terms), Constraints) :-
	memberchk(c(0..MaxI) in 0..MaxC,Vars),
	memberchk(p(0..MaxT) in 0..MaxP,Vars),
	(   foreach(CA,Coeffs),
	    foreach(c(I),Terms),
	    fromto(0,Obj1,Obj1+j*CA*c(I,j),Obj)
	do  true
	),
	write('Minimize\n'),
	isolate(sum(j,0..MaxC,Obj), 1, 0, 0, LHS, []),
	write_lp_expr(LHS),
	nl,
	write('Subject To\n'),
	write_lp(forall(i,0..MaxI,[sum(j,0..MaxC,c(i,j))=1,
					'C'(i) : (sum(j,0..MaxC,(j+1)*c(i,j)) is sos1)
					% setmipdir('C(i)','XPRS_PR',1000)
				       ])),
	write_lp(forall(t,0..MaxT,[sum(r,0..MaxP,p(t,r))=1,
					'P'(t) : (sum(r,0..MaxP,(r+1)*p(t,r)) is sos1)
					% setmipdir('P(t)','XPRS_PR',1000)
				       ])),
	write_lp(forall(i,0..MaxI,[forall(j,0..MaxC,[cs(i,j) >= c(i,j) + s(i) - 1])])),
	%% necessary conditions
	write_lp(forall(t,0..MaxT,
			   [forall(j,0..MaxC,[sum(r,0..MaxP,live(t,r,j)) =< 1])])),
	write_lp(forall(t,0..MaxT,
			   [forall(r,0..MaxP,
				   [forall(j,0..MaxC,[live(t,r,j) =< p(t,r)])])])),
	(   foreach(Ctr,Constraints),
	    fromto(LR1,LR2,LR4,[]),
	    param(MaxC,MaxP)
	do  (   Ctr = (ls(T) #= c(J))
	    ->  LR2 = [T-def(J)|LR4]
	    ;   Ctr = (le(T) #= c(J))
	    ->  LR2 = [T-use(J)|LR4]
	    ;   Ctr = maximum(Cs,le(T))
	    ->  (   foreach(c(J),Cs),
		    fromto(LR2,[T-use(J)|LR3],LR3,LR4),
		    param(T)
		do  true
		)
	    ;   LR2 = LR4,
		lp_constraint(Ctr, MaxC, MaxP)
	    )
	),
	keysort(LR1, LR5),
	keyclumped(LR5, LR6),
	(   foreach(T1-[def(J1)|Js],LR6),
	    param(MaxC,MaxP)
	do  lp_liverange(T1, J1, Js, MaxC, MaxP)
	),
	write('Bounds\n'),
	(   lp_style(binlive, yes) -> true
	;   write_lp(forall(t,0..MaxT,
			       [forall(r,0..MaxP,
				       [forall(j,0..MaxC,[live(t,r,j) =< 1])])]))
	),
	(   lp_style(binc, yes) -> true
	;   write_lp(forall(i,0..MaxI,
			       [forall(j,0..MaxC, [c(i,j) =< 1])]))
	),
	(   lp_style(bincs, yes) -> true
	;   write_lp(forall(i,0..MaxI,
			       [forall(j,0..MaxC, [cs(i,j) =< 1])]))
	),
	(   lp_style(binp, yes) -> true
	;   write_lp(forall(t,0..MaxT,
			       [forall(r,0..MaxP, [p(t,r) =< 1])]))
	),
	write('Binaries\n'),
	write_lp(forall(i,0..MaxI,[s(i) is binary
					% setmipdir(s(i),'XPRS_PR',10) % WORKED WONDERS
					% setmipdir(s(i),'XPRS_UP')
				       ])),
	(   lp_style(binlive, no) -> true
	;   write_lp(forall(t,0..MaxT,
			       [forall(r,0..MaxP,
				       [forall(j,0..MaxC,[live(t,r,j) is binary
				                            % setmipdir(live(t,r,j),'XPRS_PR',1000),
							   ])])]))
	),
	(   lp_style(binc, no) -> true

	;   write_lp(forall(i,0..MaxI,
			       [forall(j,0..MaxC,
				       [c(i,j) is binary
					% setmipdir(c(i,j),'XPRS_PR',1000)
				       ])]))
	),
	(   lp_style(bincs, no) -> true
	;   write_lp(forall(i,0..MaxI,
			       [forall(j,0..MaxC,
				       [cs(i,j) is binary
					% setmipdir(cs(i,j),'XPRS_PR',1000)
				       ])]))
	),
	(   lp_style(binp, no) -> true
	;   write_lp(forall(t,0..MaxT,
			       [forall(r,0..MaxP,
				       [p(t,r) is binary
					% setmipdir(p(t,r),'XPRS_PR',1000)
				       ])]))
	),
	write('End\n').

lp_liverange(Temp, Def, Uss, MaxC, MaxP) :-
	lp_style(binlive, yes),
	lp_style(liverange, weak), !,
	length(Uss, N),
	(   foreach(use(U),Uss),
	    foreach(U,Us)
	do  true
	),
	write_lp(forall(j,0..(MaxC-1),
			[forall(r,0..MaxP,
				[N*live(Temp,r,j) >=
				sum(i,0..j,N*c(Def,i)) +
				sum(i,(j+1)..MaxC,sum(use,Us,c(use,i))) +
				N*p(Temp,r) - 2*N])])).
lp_liverange(Temp, Def, Uss, MaxC, MaxP) :-
	(   foreach(use(U),Uss),
	    foreach(U,Us)
	do  true
	),
	write_lp(forall(use,Us,
			[forall(j,0..(MaxC-1),
				[forall(r,0..MaxP,
					[live(Temp,r,j) >=
					sum(i,0..j,c(Def,i)) +
					sum(i,(j+1)..MaxC,c(use,i)) +
					p(Temp,r) - 2])])])).

lp_constraint(p(I) in Dom, _MaxC, _MaxP) :- !,
	range_to_fdset(Dom, Set),
	fdset_to_list(Set, List),
	(   foreach(J,List),
	    fromto(0,Expr1,Expr1+p(I,J),Expr),
	    param(I)
	do  true
	),
	write_lp(Expr = 1).
lp_constraint(s(K) #=> p(I) in Dom, _MaxC, _MaxP) :- !,
	range_to_fdset(Dom, Set),
	fdset_to_list(Set, List),
	(   foreach(J,List),
	    fromto(0,Expr1,Expr1+p(I,J),Expr),
	    param(I)
	do  true
	),
	write_lp(Expr >= s(K)).
lp_constraint(s(K) #=> c(I) #>= LB, _, _) :- !,
	(   for(J,0,LB-1),
	    fromto(s(K),Expr1,Expr1+c(I,J),Expr),
	    param(I)
	do  true
	),
	write_lp(Expr =< 1).
lp_constraint(s(I) #= s(J), _, _) :-
	write_lp(s(I) = s(J)).
lp_constraint(p(I) #= p(J), _MaxC, MaxP) :-
	lp_style(fd, yes), !,
	write_lp(sum(r,0..MaxP,r*p(I,r) = sum(r in 0..MaxP,r*p(J,r)))).
lp_constraint(p(I) #= p(J), _MaxC, MaxP) :- !,
	write_lp(forall(r,0..MaxP,[p(I,r) = p(J,r)])).
lp_constraint(p(I) #= J, _MaxC, _MaxP) :- !,
	write_lp(p(I,J) = 1).
lp_constraint(p(I) #\= p(J), _MaxC, MaxP) :- !,
	write_lp(forall(r,0..MaxP,[p(I,r) + p(J,r) =< 1])).
lp_constraint(s(K) #= 0 #=> p(I) #= p(J), _MaxC, MaxP) :- !,
	write_lp(forall(r,0..MaxP,[p(I,r) =< p(J,r) + s(K)])),
	write_lp(forall(r,0..MaxP,[p(J,r) =< p(I,r) + s(K)])).
lp_constraint(s(K) #= 0 #=> c(I) #= c(J), MaxC, _MaxP) :- !,
	write_lp(forall(j,0..MaxC,[c(I,j) =< c(J,j) + s(K)])),
	write_lp(forall(j,0..MaxC,[c(J,j) =< c(I,j) + s(K)])).
lp_constraint(s(K) #= 0 #=> c(I) #= min(c(L),C2), MaxC, _MaxP) :- !,
	(   fromto(C2,min(c(M),C4),C4,c(Last)),
	    fromto(c(L,j),CJ1,CJ1+c(M,j),CJ0),
	    fromto(c(L,k),CK1,CK1+c(M,k),CK0),
	    count(_,3,N)
	do  true
	),
	CJ = CJ0 + c(Last,j),
	CK = CK0 + c(Last,k),
	write_lp(forall(j,0..MaxC,[c(I,j) =< CJ + s(K)])),
	write_lp(forall(j,0..MaxC,[N*c(I,j) + N*s(K) >= CJ - sum(k,0..(j-1),(N-1)*CK)])).
lp_constraint(s(I) #= 1, _MaxC, _MaxP) :- !,
	write_lp(s(I) = 1).
lp_constraint(c(I) #= J, _MaxC, _MaxP) :- !,
	write_lp(c(I,J) = 1).
lp_constraint(c(I) #=< c(J), MaxC, _MaxP) :-
	lp_style(fd, yes), !,
	write_lp(sum(j,0..MaxC,j*c(I,j) =< sum(j,0..MaxC,j*c(J,j)))).
lp_constraint(c(I) #=< c(J), MaxC, _MaxP) :- !,
	write_lp(forall(i,0..MaxC,[sum(j,0..i,c(I,j)) >= sum(j,0..i,c(J,j))])).
lp_constraint(c(I) + D*s(I) + D*s(J) - D #=< c(J), MaxC, _MaxP) :-
	lp_style(fd, yes), !,
	write_lp(sum(j,0..MaxC,j*c(I,j) + D*s(I) + D*s(J) - D) =< sum(j,0..MaxC,j*c(J,j))).
lp_constraint(c(I) + D*s(I) + D*s(J) - D #=< c(J), MaxC, _MaxP) :- !,
	write_lp(forall(i,0..MaxC,[sum(j,0..i-D,c(I,j)) - s(I) - s(J) + 2 >= sum(j,0..i,c(J,j))])).
lp_constraint(c(I) #=< J, MaxC, _MaxP) :- !,
	write_lp(sum(j,J+1..MaxC,c(I,j)) = 0).
lp_constraint(c(I) #>= J, _MaxC, _MaxP) :- !,
	write_lp(sum(j,0..J-1,c(I,j)) = 0).
lp_constraint(c(I) in L..U, _MaxC, _MaxP) :- !,
	write_lp(sum(j,L..U,c(I,j)) = 1).
lp_constraint(cumulative(Tasks,Limit), MaxC, _MaxP) :- !, % TODO: dur>1
	(   foreach(task(c(I),1,Ri*s(I)),Tasks),
	    fromto(0,Expr1,Expr1+Ri*cs(I,j),Expr)
	do  true
	),
	write_lp(forall(j,0..MaxC,[Expr =< Limit])).
lp_constraint(disjoint2(Rects), MaxC, MaxP) :- !, % TODO: dur>1
	(   foreach(rect(p(I),p(I)+1,ls(I),le(I)),Rects), % TODO: width>1
	    fromto(0,Expr1,Expr1+live(I,r,j),Expr)
	do  true
	),
	write_lp(forall(j,0..MaxC,[forall(r,0..MaxP,[Expr =< 1])])).
lp_constraint(all_different(Ps), _MaxC, MaxP) :- !,
	(   foreach(p(I),Ps),
	    fromto(0,Expr1,Expr1+p(I,r),Expr)
	do  true
	),
	write_lp(forall(r,0..MaxP,[Expr =< 1])).
lp_constraint(value_precede_chain(_,_), _MaxC, _MaxP) :-
	lp_style(value_precede, off), !.
lp_constraint(value_precede_chain(Ints,Ps), _MaxC, _MaxP) :-
	lp_style(value_precede, strong), !,
	(   fromto(Ints,[A,B|Ints1],[B|Ints1],[_]),
	    param(Ps)
	do  (   foreach(p(I),Ps),
		fromto(0,Expr1,Expr1+p(I,A),_),
		param(A,B)
	    do  write_lp(Expr1 >= p(I,B))
	    )
	).
lp_constraint(value_precede_chain(Ints,Ps), _MaxC, _MaxP) :-
	lp_style(value_precede, weak), !,
	lp_value_precede_chain_weak(Ints, Ps). % domains only

lp_value_precede_chain_weak([_], _) :- !.
lp_value_precede_chain_weak(_, []) :- !.
lp_value_precede_chain_weak([_|Ints], [p(I)|Ps]) :-
	(   foreach(A,Ints),
	    fromto(0,Expr1,Expr1+p(I,A),Expr),
	    param(I)
	do  true
	),
	write_lp(Expr = 0),
	lp_value_precede_chain_weak(Ints, Ps).

write_lp(Ctr) :-
	write('  '),
	write_lp_ctr(Ctr),
	nl.

write_lp_ctr(L =< R) :- !,
	isolate(L-R, 1, 0, RHS, LHS, []),
	write_lp_expr(LHS),
	write(' <= '),
	write(RHS).
write_lp_ctr(L = R) :- !,
	isolate(L-R, 1, 0, RHS, LHS, []),
	write_lp_expr(LHS),
	write(' = '),
	write(RHS).
write_lp_ctr(L >= R) :- !,
	isolate(L-R, 1, 0, RHS, LHS, []),
	write_lp_expr(LHS),
	write(' >= '),
	write(RHS).
write_lp_ctr(forall(Q,L..U,Ctrs)) :- !,
	(   for(I,L,U),
	    foreach(I,List)
	do  true
	),
	(   foreach(Value,List),
	    param(Q,Ctrs)
	do  (   foreach(Ctr1,Ctrs),
		param(Q,Value)
	    do  substitute(Q, Value, Ctr1, Ctr2),
		write_lp(Ctr2)
	    )
	).
write_lp_ctr(forall(Q,List,Ctrs)) :- !,
	(   foreach(Value,List),
	    param(Q,Ctrs)
	do  (   foreach(Ctr1,Ctrs),
		param(Q,Value)
	    do  substitute(Q, Value, Ctr1, Ctr2),
		write_lp(Ctr2)
	    )
	).
write_lp_ctr(Name : Ctr) :- !,
	write(Name),
	write(' : '),
	write_lp_ctr(Ctr).
write_lp_ctr(Expr is binary) :- !,
	write(Expr).
write_lp_ctr(Ctr is sos1) :- !,
	isolate(Ctr, 1, 0, 0, LHS, []),
	write_lp_expr(LHS),
	write(' = S1').
write_lp_ctr(Ctr is sos2) :- !,
	isolate(Ctr, 1, 0, 0, LHS, []),
	write_lp_expr(LHS),
	write(' = S2').
write_lp_ctr(Ctr) :-
	write(Ctr).

write_lp_expr([1-V]) :- !,
	write(V).
write_lp_expr(LHS) :-
	write('0 '),
	(   foreach(C-V,LHS)
	do  (   C=:=1 -> format('+\n ~w', [V])
	    ;   C=:= -1 -> format('-\n ~w', [V])
	    ;   C>0 -> format('+\n ~d ~w', [C,V])
	    ;   C<0 -> format('-\n ~d ~w', [-C,V])
	    )
	).

isolate(_, Sign, R, R) -->
	{Sign=:=0}, !.
isolate(X+Y, S, R0, R) --> !,
	isolate(X, S, R0, R1),
	isolate(Y, S, R1, R).
isolate(X-Y, S, R0, R) --> !,
	isolate(X, S, R0, R1),
	isolate(Y, -S, R1, R).
isolate(N*X, S, R0, R) --> !,
	isolate(X, N*S, R0, R).
isolate(sum(Q,L..U,X), S, R0, R) --> !,
	(   for(N,L,U),
	    fromto(R0,R1,R2,R),
	    param(Q,X,S)
	do  {substitute(Q, N, X, Y)},
	    isolate(Y, S, R1, R2)
	).
isolate(X, Sign, R0, R) -->
	{integer(X)}, !,
	{R is R0-Sign*X}.
isolate(X, Sign, R, R) --> [V-X],
	{V is Sign}.

substitute(Old, New, Old, New) :- !.
substitute(_, _, Term, Term) :-
	simple(Term), !.
substitute(Old, New, Term1, Term2) :-
	Term1 =.. [F|L1],
	(   foreach(X,L1),
	    foreach(Y,L2),
	    param(Old,New)
	do  substitute(Old, New, X, Y)
	),
	Term2 =.. [F|L2].

