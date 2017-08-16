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
:- ensure_loaded('../../../presolver/grass').

:- op(550, xfx, :=).
:- op(550, xfx, ::).

style(c, binary).		% binary | default
style(p, binary).		% binary | default
style(cs, default).		% binary | default
style(live, default).		% binary | default | off
style(liverange, weak).		% weak | strong
style(value_precede, strong).	% off | weak | strong

json2mos(InputName) :-
    replace_file_extension(InputName, ".ext.json", ".mos", OutputName),
	json2avl(InputName, AVL),
	model(AVL, Vars, Objective, _, _, _, _, Constraints, []),
	tell(OutputName),
	model2mos(Vars, Objective, Constraints),
	told.

model2mos(vars(Vars,_), scalar_product(Coeffs,Terms), Constraints) :-
	retractall(mos_c_bounds(_,_,_)),
	memberchk(c(0..MaxI) in 0..MaxC,Vars),
	memberchk(p(0..MaxT) in 0..MaxP,Vars),
	length(Coeffs, Ncost),
	(   foreach(c(I),Terms),
	    foreach(I,Indexes)
	do  true
	),
	write('model "anonymous"\n'),
	write('  uses "mmxprs"\n'),
	write('  parameters\n'),
	format('    MAXC = ~d\n', [MaxC]),
	format('    MAXP = ~d\n', [MaxP]),
	format('    MAXI = ~d\n', [MaxI]),
	format('    MAXT = ~d\n', [MaxT]),
	format('    NCOST = ~d\n', [Ncost]),
	write('  end-parameters\n'),
	nl,
	write('  declarations\n'),
	write('    c: array(0..MAXI,0..MAXC) of mpvar\n'),
	write('    p: array(0..MAXT,0..MAXP) of mpvar\n'),
	write('    s: array(0..MAXI) of mpvar\n'),
	(   style(live, off) -> true
	;   write('    live: array(0..MAXT,0..MAXP,0..MAXC) of mpvar\n')
	),
	write('    cs: array(0..MAXI,0..MAXC) of mpvar\n'),
	write('    CA: array(1..NCOST) of integer\n'),
	write('    CI: array(1..NCOST) of integer\n'),
	write('  end-declarations\n'),
	nl,
	(   style(live, off) -> true
	;   style(live, binary),
	    style(liverange, weak) ->
	    write_mosel(procedure(liverange,[temp:integer,def:integer,uss:set(integer)],
				  [n := getsize(uss),
				   forall(j,0..('MAXC'-1),
					  [forall(r,0..'MAXP',
						  [n*live(temp,r,j) >=
						   sum(i,0..j,n*c(def,i)) +
						   sum(i,(j+1)..'MAXC',sum(use,uss,c(use,i))) +
						   n*p(temp,r) - 2*n])])]), 2)
	;   write_mosel(procedure(liverange,[temp:integer,def:integer,uss:set(integer)],
				  [forall(use,uss,
					  [forall(j,0..('MAXC'-1),
						  [forall(r,0..'MAXP',
							  [live(temp,r,j) >=
							   sum(i,0..j,c(def,i)) +
							   sum(i,(j+1)..'MAXC',c(use,i)) +
							   p(temp,r) - 2])])])]), 2)
	),
	write_mosel(forall(i,0..'MAXI',[s(i) is binary,
					setmipdir(s(i),'XPRS_PR',10) % WORKED WONDERS
					% setmipdir(s(i),'XPRS_UP')
				       ]), 2),
	write_mosel(forall(i,0..'MAXI',[forall(j,0..'MAXC',[cs(i,j) >= c(i,j) + s(i) - 1])]), 2),
	(   style(live, off) -> true
	;   style(live, binary) ->
	    write_mosel(forall(t,0..'MAXT',
			       [forall(r,0..'MAXP',
				       [forall(j,0..'MAXC',[live(t,r,j) is binary
				                            % setmipdir(live(t,r,j),'XPRS_PR',1000),
							   ])])]), 2)
	;   write_mosel(forall(t,0..'MAXT',
			       [forall(r,0..'MAXP',
				       [forall(j,0..'MAXC',[live(t,r,j) =< 1])])]), 2)
	),
	%% necessary conditions
	(   style(live, off) -> true
	;   write_mosel(forall(t,0..'MAXT',
			       [forall(j,0..'MAXC',[sum(r,0..'MAXP',live(t,r,j)) =< 1])]), 2),
	    write_mosel(forall(t,0..'MAXT',
			       [forall(r,0..'MAXP',
				       [forall(j,0..'MAXC',[live(t,r,j) =< p(t,r)])])]), 2)
	),
	(   style(c, binary) ->
	    write_mosel(forall(i,0..'MAXI',
			       [sum(j,0..'MAXC',c(i,j))=1,
				forall(j,0..'MAXC',
				       [c(i,j) is binary
					% setmipdir(c(i,j),'XPRS_PR',1000)
				       ])]), 2)
	;   write_mosel(forall(i,0..'MAXI',
			       [forall(j,0..'MAXC',
				       [c(i,j) =< 1])]), 2),
	    write_mosel(forall(i,0..'MAXI',[sum(j,0..'MAXC',c(i,j))=1,
					    'C'(i) : (sum(j,0..'MAXC',(j+1)*c(i,j)) is sos1)
				            % setmipdir('C(i)','XPRS_PR',1000)
					   ]), 2)
	),
	(   style(cs, binary) ->
	    write_mosel(forall(i,0..'MAXI',
			       [forall(j,0..'MAXC',
				       [cs(i,j) is binary
					% setmipdir(cs(i,j),'XPRS_PR',1000)
				       ])]), 2)
	;   write_mosel(forall(i,0..'MAXI',
			       [forall(j,0..'MAXC',
				       [cs(i,j) =< 1])]), 2)
	),
	(   style(p, binary) ->
	    write_mosel(forall(t,0..'MAXT',
			       [sum(r,0..'MAXP',p(t,r))=1,
				forall(r,0..'MAXP',
				       [p(t,r) is binary
					% setmipdir(p(t,r),'XPRS_PR',1000)
				       ])]), 2)
	;   write_mosel(forall(t,0..'MAXT',
			       [forall(r,0..'MAXP',
				       [p(t,r) =< 1])]), 2),
	    write_mosel(forall(t,0..'MAXT',[sum(r,0..'MAXP',p(t,r))=1,
					    'P'(t) : (sum(r,0..'MAXP',(r+1)*p(t,r)) is sos1)
					    % setmipdir('P(t)','XPRS_PR',1000)
					   ]), 2)
	),
	write_mosel('CA' :: Coeffs, 2),
	write_mosel('CI' :: Indexes, 2),
	write_mosel('Cost' := sum(j,0..'MAXC',sum(i,1..'NCOST',j*'CA'(i)*c('CI'(i),j))), 2),
	(   foreach(Ctr,Constraints),
	    fromto(LR1,LR2,LR4,[]),
	    fromto(Disjoint1,Disjoint2,Disjoint3,[]),
	    fromto(Mutex1,Mutex2,Mutex3,[])
	do  (   Ctr = (ls(T) #= c(J))
	    ->  LR2 = [T-def(J)|LR4],
		Disjoint2 = Disjoint3, Mutex2 = Mutex3
	    ;   Ctr = (le(T) #= c(J))
	    ->  LR2 = [T-use(J)|LR4],
		Disjoint2 = Disjoint3, Mutex2 = Mutex3
	    ;   Ctr = maximum(Cs,le(T))
	    ->  (   foreach(c(J),Cs),
		    fromto(LR2,[T-use(J)|LR3],LR3,LR4),
		    param(T)
		do  true
		),
		Disjoint2 = Disjoint3, Mutex2 = Mutex3
	    ;   Ctr = disjoint2(_)
	    ->  LR2 = LR4,
	        Disjoint2 = [Ctr|Disjoint3], Mutex2 = Mutex3
	    ;   Ctr = (_ #\/ _)
	    ->  LR2 = LR4,
	        Disjoint2 = Disjoint3, Mutex2 = [Ctr|Mutex3]
	    ;   LR2 = LR4,
		Disjoint2 = Disjoint3, Mutex2 = Mutex3,
		mos_constraint(Ctr)
	    )
	),
	keysort(LR1, LR5),
	keyclumped(LR5, LR6),
	style(live, LiveStyle),
	mos_disjoint2(LiveStyle, Disjoint1, LR6),
	mos_mutex(Mutex1, LR6),
	write_mosel(setparam('"XPRS_LOADNAMES"', true), 2),
	write_mosel(setparam('"XPRS_VERBOSE"', true), 2),
	write_mosel(setparam('"XPRS_COLORDER"', 1), 2),
	% write_mosel(setparam('"XPRS_HEURSEARCHEFFORT"', 2), 2),
	write_mosel(setparam('"XPRS_COVERCUTS"', 0), 2),
	write_mosel(setparam('"XPRS_GOMCUTS"', 0), 2),
	write_mosel(minimize('XPRS_PRI','Cost'), 2),
	write_mosel(writeln('"objective: "', getobjval), 2),
	write_mosel(write('"scheduled: "'), 2),
	write_mosel(forall(i,0..'MAXI',[write(getsol(s(i)), '","')]), 2),
	write_mosel(writeln('" "'), 2),
	write_mosel(write('"cycle   : "'), 2),
	write_mosel(forall(i,0..'MAXI',[write(getsol(sum(j,0..'MAXC',j*c(i,j))), '","')]), 2),
	write_mosel(writeln('" "'), 2),
	write_mosel(write('"register: "'), 2),
	write_mosel(forall(t,0..'MAXT',[write(getsol(sum(r,0..'MAXP',r*p(t,r))), '","')]), 2),
	write_mosel(writeln('" "'), 2),
	write('end-model\n').

mos_constraint(p(I) in Dom) :- !,
	range_to_fdset(Dom, Set),
	fdset_to_list(Set, List),
	(   foreach(J,List),
	    fromto(0,Expr1,Expr1+p(I,J),Expr),
	    param(I)
	do  true
	),
	write_mosel(Expr = 1, 2).
mos_constraint(s(K) #=> p(I) in Dom) :- !,
	range_to_fdset(Dom, Set),
	fdset_to_list(Set, List),
	(   foreach(J,List),
	    fromto(0,Expr1,Expr1+p(I,J),Expr),
	    param(I)
	do  true
	),
	write_mosel(Expr >= s(K), 2).
mos_constraint(s(K) #=> c(I) #>= LB) :- !,
	(   for(J,0,LB-1),
	    fromto(s(K),Expr1,Expr1+c(I,J),Expr),
	    param(I)
	do  true
	),
	write_mosel(Expr =< 1, 2).
mos_constraint(s(I) #= s(J)) :-
	write_mosel(s(I) = s(J), 2).
mos_constraint(p(I) #= p(J)) :- !,
	write_mosel(forall(r,0..'MAXP',[p(I,r) = p(J,r)]), 2).
mos_constraint(p(I) #= J) :- !,
	write_mosel(p(I,J) = 1, 2).
mos_constraint(p(I) #\= p(J)) :- !,
	write_mosel(forall(r,0..'MAXP',[p(I,r) + p(J,r) =< 1]), 2).
mos_constraint(s(K) #= 0 #=> p(I) #= p(J)) :- !,
	write_mosel(forall(r,0..'MAXP',[p(I,r) =< p(J,r) + s(K)]), 2),
	write_mosel(forall(r,0..'MAXP',[p(J,r) =< p(I,r) + s(K)]), 2).
% mos_constraint(s(K) #= 0 #=> c(I) #= _) :- !.
% Did more harm than good?
mos_constraint(s(K) #= 0 #=> c(I) #= c(J)) :- !,
	write_mosel(forall(j,0..'MAXC',[c(I,j) =< c(J,j) + s(K)]), 2),
	write_mosel(forall(j,0..'MAXC',[c(J,j) =< c(I,j) + s(K)]), 2).
% Did more harm than good?
mos_constraint(s(K) #= 0 #=> c(I) #= min(c(L),C2)) :- !,
	(   fromto(C2,min(c(M),C4),C4,c(Last)),
	    fromto(c(L,j),CJ1,CJ1+c(M,j),CJ0),
	    fromto(c(L,k),CK1,CK1+c(M,k),CK0),
	    count(_,3,N)
	do  true
	),
	CJ = CJ0 + c(Last,j),
	CK = CK0 + c(Last,k),
	write_mosel(forall(j,0..'MAXC',[c(I,j) =< CJ + s(K)]), 2),
	write_mosel(forall(j,0..'MAXC',[N*c(I,j) + N*s(K) >= CJ - sum(k,0..(j-1),(N-1)*CK)]), 2).
mos_constraint(s(I) #= 1) :- !,
	write_mosel(s(I) = 1, 2).
mos_constraint(c(I) #= c(J)) :- !,
	write_mosel(forall(j,0..'MAXC',[c(I,j) = c(J,j)]), 2).
mos_constraint(c(I) #= J) :- !,
	write_mosel(c(I,J) = 1, 2).
mos_constraint(c(I) #=< c(J)) :- !,
	write_mosel(forall(i,0..'MAXC',[sum(j,0..i,c(I,j)) >= sum(j,0..i,c(J,j))]), 2).
mos_constraint(c(I) + D*s(I) + D*s(J) - D #=< c(J)) :- !,
	write_mosel(forall(i,0..'MAXC',[sum(j,0..i-D,c(I,j)) - s(I) - s(J) + 2 >= sum(j,0..i,c(J,j))]), 2).
mos_constraint(c(I) #=< J) :- !,
	write_mosel(sum(j,J+1..'MAXC',c(I,j)) = 0, 2).
mos_constraint(sum(Ss) #>= LB) :- !,
	(   foreach(s(I),Ss),
	    foreach(I,Is)
	do  true
	),
	brace(Is, Iset),
	write_mosel(sum(i,Iset,s(i)) >= LB, 2).
mos_constraint(c(I) #>= J) :- !,
	write_mosel(sum(j,0..J-1,c(I,j)) = 0, 2).
mos_constraint(c(I) in L..U) :- !,
	assertz(mos_c_bounds(I,L,U)),
	(   L=:=0 -> true
	;   write_mosel(sum(j,0..(L-1),c(I,j)) = 0, 2)
	),
	write_mosel(sum(j,(U+1)..'MAXC',c(I,j)) = 0, 2).
mos_constraint(cumulative(Tasks,Limit)) :- !, % TODO: dur>1
	(   foreach(task(c(I),1,Ri*s(I)),Tasks),
	    fromto(0,Expr1,Expr1+Ri*cs(I,j),Expr)
	do  true
	),
	write_mosel(forall(j,0..'MAXC',[Expr =< Limit]), 2).
mos_constraint(all_different(Ps)) :- !,
	(   foreach(p(I),Ps),
	    fromto(0,Expr1,Expr1+p(I,r),Expr)
	do  true
	),
	write_mosel(forall(r,0..'MAXP',[Expr =< 1]), 2).
mos_constraint(value_precede_chain(_,_)) :-
	style(value_precede, off), !.
mos_constraint(value_precede_chain(Ints,Ps)) :-
	style(value_precede, strong), !,
	(   fromto(Ints,[A,B|Ints1],[B|Ints1],[_]),
	    param(Ps)
	do  (   foreach(p(I),Ps),
		fromto(0,Expr1,Expr1+p(I,A),_),
		param(A,B)
	    do  write_mosel(Expr1 >= p(I,B), 2)
	    )
	).
mos_constraint(value_precede_chain(Ints,Ps)) :-
	style(value_precede, weak), !,
	value_precede_chain_weak(Ints, Ps). % domains only

mos_disjoint2(off, Defered, DefUse) :- !, % TODO: width>1
	list_to_avl(DefUse, AVL),
	(   foreach(disjoint2(Rects),Defered),
	    param(AVL)
	do  (   fromto(Rects,[R1|Rs],Rs,[_]),
		param(AVL)
	    do  R1 = rect(p(I),p(I)+1,ls(I),le(I)),
		avl_fetch(I, AVL, [def(DI)|UseI]),
		mos_c_bounds(DI, ILB, _),
		(   foreach(use(UI),UseI),
		    fromto(0,IMax1,IMax2,IMax),
		    fromto(HoleI,SetI,(UI,SetI),(HoleI,Set2I))
		do  mos_c_bounds(UI, _, IUB),
		    IMax2 is max(IMax1,IUB)
		),
		(   foreach(R2,Rs),
		    param(AVL,I,ILB,IMax,DI,Set2I)
		do  R2 = rect(p(J),p(J)+1,ls(J),le(J)),
		    avl_fetch(J, AVL, [def(DJ)|UseJ]),
		    mos_c_bounds(DJ, JLB, _),
		    LB is max(ILB,JLB),	% smallest cycle with potential conflict
		    (   foreach(use(UJ),UseJ),
			fromto(0,JMax1,JMax2,JMax),
			fromto(HoleJ,SetJ,(UJ,SetJ),(HoleJ,Set2J))
		    do  mos_c_bounds(UJ, _, JUB),
			JMax2 is max(JMax1,JUB)
		    ),
		    UB is min(IMax,JMax), % greatest cycle with potential conflict
		    write_mosel(forall(cy,LB..UB,
				       [forall(r,0..'MAXP',
					       [forall(ui,{Set2I},
						       [forall(uj,{Set2J},
							       [p(I,r) + p(J,r) +
							       sum(j,ILB..cy,c(DI,j)) +
							       sum(j,JLB..cy,c(DJ,j)) +
							       sum(j,cy+1..IMax,c(ui,j)) +
							       sum(j,cy+1..JMax,c(uj,j))
							       =< 5])])])]), 2)
		)
	    )
	).
mos_disjoint2(_, Defered, DefUse) :- !,
	(   foreach(disjoint2(Rects),Defered)
	do  (   foreach(rect(p(I),p(I)+1,ls(I),le(I)),Rects), % TODO: width>1
		fromto(0,Expr1,Expr1+live(I,r,j),Expr)
	    do  true
	    ),
	    write_mosel(forall(j,0..'MAXC',[forall(r,0..'MAXP',[Expr =< 1])]), 2)
	),
	(   foreach(T1-[def(J1)|Js],DefUse)
	do  format('  liverange(~d,~d,', [T1,J1]),
	    (   foreach(use(J2),Js),
		fromto('{',Sep,',',_)
	    do  write(Sep),
		write(J2)
	    ),
	    write('})\n')
	).

mos_mutex(Defered, DefUse) :-
	(   foreach(Defer,Defered),
	    param(DefUse)
	do  (   Defer = (sum(Ss) #>= 1 #\/ le(P) #=< ls(Q))
	    ->  memberchk(P-[_|Puses], DefUse),
		memberchk(Q-[def(Qi)|_], DefUse),
		length(Puses, M),
		(   fromto(M-M*c(Qi,i),Expr1,Expr1+M*S,Expr),
		    foreach(S,Ss),
		    param(M)
		do  true
		),
		(   foreach(use(Pi),Puses),
		    foreach(Pi,Pis)
		do  true
		),
		brace(Pis, Piset),
		write_mosel(forall(i,0..'MAXC'-1,
				   [sum(u,Piset,sum(j,(i+1)..'MAXC',c(u,j))) =< Expr]), 2)
	    ;   true
	    )
	).

value_precede_chain_weak([_], _) :- !.
value_precede_chain_weak(_, []) :- !.
value_precede_chain_weak([_|Ints], [p(I)|Ps]) :-
	(   foreach(A,Ints),
	    fromto(0,Expr1,Expr1+p(I,A),Expr),
	    param(I)
	do  true
	),
	write_mosel(Expr = 0, 2),
	value_precede_chain_weak(Ints, Ps).

write_mosel(Ctr, Tab) :-
	(for(_,1,Tab) do put_code(0' )),
	write_mosel_ctr(Ctr, Tab),
	nl.

write_mosel_ctr(L =< R, _) :- !,
	write_mosel_expr(L),
	write(' <= '),
	write_mosel_expr(R).
write_mosel_ctr(L = R, _) :- !,
	write_mosel_expr(L),
	write(' = '),
	write_mosel_expr(R).
write_mosel_ctr(L >= R, _) :- !,
	write_mosel_expr(L),
	write(' >= '),
	write_mosel_expr(R).
write_mosel_ctr(forall(Q,Set,List), Tab) :- !,
	Tab2 is Tab+2,
	format('forall(~w in ~w) do\n', [Q,Set]),
	(   foreach(Elt,List),
	    param(Tab2)
	do  write_mosel(Elt, Tab2)
	),
	(for(_,1,Tab) do put_code(0' )),
	write('end-do').
write_mosel_ctr(procedure(Q,Params,List), Tab) :- !,
	Tab2 is Tab+2,
	write('procedure '),
	write(Q),
	(   foreach(Var:Type,Params),
	    fromto('(',Sep,',',_)
	do  (   Type = set(Sub) ->
		format('~w~w:set of ~w', [Sep,Var,Sub])
	    ;   format('~w~w:~w', [Sep,Var,Type])
	    )
	),
	write(')\n'),
	(   foreach(Elt,List),
	    param(Tab2)
	do  write_mosel(Elt, Tab2)
	),
	(for(_,1,Tab) do put_code(0' )),
	write('end-procedure').
write_mosel_ctr(Var :: Value, _) :- !,
	write(Var :: Value).
write_mosel_ctr(Name : Ctr, Tab) :- !,
	write(Name),
	write(' := '),
	write_mosel_ctr(Ctr, Tab).
write_mosel_ctr(Name := Expr, _) :- !,
	write(Name),
	write(' := '),
	write_mosel_expr(Expr).
write_mosel_ctr(setmipdir(A,B), _) :- !,
	write(setmipdir(A,B)).
write_mosel_ctr(setmipdir(A,B,C), _) :- !,
	write(setmipdir(A,B,C)).
write_mosel_ctr(Expr is Prop, _) :- !,
	write_mosel_expr(Expr),
	format(' is_~w', [Prop]).
write_mosel_ctr(write(A), _) :- !,
	write('write('),
	write_mosel_expr(A),
	write(')').
write_mosel_ctr(write(A,B), _) :- !,
	write('write('),
	write_mosel_expr(A),
	write(','),
	write_mosel_expr(B),
	write(')').
write_mosel_ctr(writeln(A), _) :- !,
	write('writeln('),
	write_mosel_expr(A),
	write(')').
write_mosel_ctr(writeln(A,B), _) :- !,
	write('writeln('),
	write_mosel_expr(A),
	write(','),
	write_mosel_expr(B),
	write(')').
write_mosel_ctr(Ctr, _) :-
	write(Ctr).

write_mosel_expr(sum(Q,Set,Expr)) :- !,
	format('(sum(~w in ~w) (', [Q,Set]),
	write_mosel_expr(Expr),
	write('))').
write_mosel_expr(X + Y) :- !,
	write_mosel_expr(X),
	write(+),
	write_mosel_expr(Y).
write_mosel_expr(X - Y) :- !,
	write_mosel_expr(X),
	write(-),
	write_mosel_expr(Y).
write_mosel_expr(getsol(X)) :- !,
	write('getsol('),
	write_mosel_expr(X),
	write(')').
write_mosel_expr(Expr) :-
	write(Expr).

brace(List, {Expr}) :-
	(   foreach(UJ,List),
	    fromto(HoleJ,SetJ,(UJ,SetJ),(HoleJ,Expr))
	do  true
	).

