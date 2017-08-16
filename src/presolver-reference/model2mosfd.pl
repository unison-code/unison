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

model2mos(vars(Vars,_), scalar_product(Coeffs,Terms), Constraints) :-
	write('model "anonymous"\n'),
	write('  uses "advmod"\n'),
	write('  declarations\n'),
	memberchk(c(0..NI) in 0..MaxC,Vars),
	memberchk(p(0..NT) in _,Vars),
	format('    MAXC = ~d\n',[MaxC]),
	(   foreach(Array in _, Vars)
	do  Array =.. [Name,0..LenExpr],
	    format('    ~w: array(0..~d) of mpvar\n', [Name,LenExpr])
	),
	format('    cum: array(0..~d,0..MAXC) of mpvar\n', [NI]),
	write('  end-declarations\n\n'),
	(   foreach(Array2 in 0..Max2, Vars)
	do  Array2 =.. [Name2,0..LenExpr2],
	    format('  forall(i in 0..~d) do\n', [LenExpr2]),
	    (   Max2==sup ->
		format('    ~w(i) is_integer\n', [Name2])
	    ;   Max2==1 ->
		format('    ~w(i) is_binary\n', [Name2])
	    ;   true ->
		format('    ~w(i) is_integer\n', [Name2]),
		format('    ~w(i) <= ~d\n', [Name2,Max2])
	    ),
	    write('  end-do\n')
	),
	format('  forall(i in 0..~d) do\n', [NI]),
	write('    forall(j in 0..MAXC) do\n'),
	write('      cum(i,j) is_binary\n'),
	write('    end-do\n'),
	write('  end-do\n'),
	(   foreach(Ctr,Constraints)
	do  write('  '),
	    mos_constraint(Ctr),
	    nl
	),
	(   foreach(C,Coeffs),
	    foreach(T,Terms),
	    fromto(0,Obj1,Obj1+C*T,Obj)
	do  true
	),
	write('  Cost := '),
	mos_expr(Obj, int),
	nl,
	write('  setparam("XPRS_VERBOSE", true)\n'),
	write('  setparam("XPRS_HEURSEARCHEFFORT", 2)\n'),
	write('  setparam("XPRS_colorder", 1)\n'),
	write('  minimize(Cost)\n'),
	write('  writeln("objective: ", getobjval)\n'),
	write('  write("scheduled: ")\n'),
	format('  forall(i in 0..~d) do\n', [NI]),
	write('    write(getsol(s(i)), ",")\n'),
	write('  end-do\n'),
	write('  writeln(" ")\n'),
	format('  forall(i in 0..~d) do\n', [NI]),
	write('    write(getsol(c(i)), ",")\n'),
	write('  end-do\n'),
	write('  writeln(" ")\n'),
	format('  forall(i in 0..~d) do\n', [NT]),
	write('    write(getsol(p(i)), ",")\n'),
	write('  end-do\n'),
	write('  writeln(" ")\n'),
	write('end-model\n').

mos_domain({I}, X) :-
	write(X=I).
mos_domain(I..J, X) :-
	format('(~w >= ~d and ~w <= ~d)', [X,I,X,J]).
mos_domain(D1\/D2, X) :-
	write('('),
	mos_domain(D1, X),
	write(' or '),
	mos_domain(D2, X),
	write(')').

mos_expr(1*Y, _) :- !,
	mos_expr(Y, int).
mos_expr(X*Y, _) :- !,
	write('('),
	mos_expr(X, int),
	write(*),
	mos_expr(Y, int),
	write(')').
mos_expr(0+Y, _) :- !,
	mos_expr(Y, int).
mos_expr(X+Y, _) :- !,
	write('('),
	mos_expr(X, int),
	write(+),
	mos_expr(Y, int),
	write(')').
mos_expr(X-Y, _) :- !,
	write('('),
	mos_expr(X, int),
	write(-),
	mos_expr(Y, int),
	write(')').
mos_expr(X, _) :-
	write(X).

mos_constraint(X #= Y) :-
	mos_expr(X, int),
	write(' = '),
	mos_expr(Y, int).
mos_constraint(X #=< Y) :-
	mos_expr(X, int),
	write(' <= '),
	mos_expr(Y, int).
mos_constraint(X #>= Y) :-
	mos_expr(X, int),
	write(' >= '),
	mos_expr(Y, int).
mos_constraint(X #\= Y) :-
	mos_constraint(X #=< Y-1),
	write(' or '),
	mos_constraint(X #>= Y+1).
mos_constraint(X in 0..UB) :- !,
	mos_constraint(X #=< UB).
mos_constraint(X in Dom) :-
	mos_domain(Dom, X).
mos_constraint(X #= Y #<=> Z #= 0) :- !, % NB. relaxing iff
	write('implies('),
	mos_constraint(Z #=< 0),
	write(','),
	mos_constraint(X #= Y),
	write(')').
mos_constraint(maximum(Xs,Y)) :- % NB. relaxed version
	(   foreach(X1,Xs),
	    param(Y)
	do  mos_constraint(X1 #=< Y),
	    write('\n  ')
	).
	% write('('),
	% (   foreach(X2,Xs),
	%     fromto('',Sep,' or ',_),
	%     param(Y)
	% do  write(Sep),
	%     mos_constraint(Y #=< X2)
	% ),
	% write(')').
mos_constraint(all_different(Xs)) :-
	(   fromto(Xs,[X|Ys],Ys,[_])
	do  (   foreach(Y,Ys),
		param(X)
	    do  mos_constraint(X #\= Y),
		write('\n  ')
	    )
	).
mos_constraint(cumulative(Tasks1,Lim)) :-
	(   foreach(task(c(I),Dur,U*s(I)),Tasks1),
	    foreach(I,Is),
	    foreach(Dur,Ds),
	    foreach(U,Us)
	do  true
	),
	write('forall(j in 0..MAXC) do\n'),
	(   foreach(I2,Is),
	    foreach(D2,Ds),
	    foreach(U2,Us),
	    fromto(0,Sum1,Sum1+U2*cum(I2,j),Sum)
	do  format('    xor(j+0.5 >= ~w and j-0.5 <= ~w + ~d - 1 and ~w >= 1,~w <= 0)\n',
	    	   [c(I2),c(I2),D2,s(I2),cum(I2,j)])
	    % THIS VERSION GIVES A WORSE BOUND:
	    % format('    implies(j+0.5 >= ~w and j-0.5 <= ~w + ~d - 1,~w >= ~w)\n',
	    % 	   [c(I2),c(I2),D2,cum(I2,j)o,s(I2)])
	),
	write('    '),
	mos_constraint(Sum #=< Lim),
	write('\n  end-do').
mos_constraint(disjoint2(Rects1)) :-
	(   fromto(Rects1,[Rect1|Rects2],Rects2,[_])
	do  (   foreach(Rect2,Rects2),
		param(Rect1)
	    do  Rect1 = rect(PSi,PEi,LSi,LEi),
		Rect2 = rect(PSj,PEj,LSj,LEj),
		mos_constraint(PSi #>= PEi),
		write(' or '),
		mos_constraint(PSi #>= PEj),
		write(' or '),
		mos_constraint(PSj #>= PEi),
		write(' or '),
		mos_constraint(PSj #>= PEj),
		write(' or '),
		mos_constraint(LSi #>= LEi),
		write(' or '),
		mos_constraint(LSi #>= LEj),
		write(' or '),
		mos_constraint(LSj #>= LEi),
		write(' or '),
		mos_constraint(LSj #>= LEj),
		write('\n  ')
	    )
	).
mos_constraint(value_precede_chain(_,_)). % Maybe later
