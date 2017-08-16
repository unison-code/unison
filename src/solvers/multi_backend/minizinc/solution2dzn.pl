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
%   Generator of *.dzn from *.ext.json AND *.out.json
%
%   Mats Carlsson <mats.carlsson@ri.se>
%   http://www.sics.se/people/matsc
%
%   TODO:


:- use_module(library(avl)).
:- use_module(library(ordsets)).
:- use_module(library(lists)).
:- use_module(library(terms)).
:- use_module(library(codesio)).
:- use_module(library(clpfd)).

:- dynamic
	cur_set_index/3,
	cur_literal_index/3.

json2dzn(ExtJsonName) :-
	statistics(runtime, [T1,_]),
	replace_file_extension(ExtJsonName, '.ext.json', '.dzn', OutputName),
	replace_file_extension(ExtJsonName, '.ext.json', '.out.json', OutJsonName),
	tell(OutputName),
	json2avl(ExtJsonName, AVL1),
	model2dzn(AVL1, MAXI, MAXP, MAXT),
	json2avl(OutJsonName, AVL2),
	solution2dzn(MAXI, MAXP, MAXT, AVL2),
	told,
	statistics(runtime, [T2,_]),
	print_message(warning, format('generated ~w in ~d msec',[OutputName,T2-T1])).

solution2dzn(MAXI, MAXP, MAXT, AVL) :-
	avl_fetch(cycles, AVL, FacitC),
	avl_fetch(instructions, AVL, FacitO),
	avl_fetch(temporaries, AVL, FacitT),
	avl_fetch(registers, AVL, FacitR),
	(   foreach(C,FacitC),
	    foreach(A,FacitA)
	do  A is min(C+1,1)
	),
	(   foreach(T,FacitT),
	    foreach(RT,FacitRT),
	    param(FacitR)
	do  I is T+1,
	    nth0(I, [-1|FacitR], RT)
	),
	write_array(facit_a, array(0..MAXI,int), FacitA),
	write_array(facit_c, array(0..MAXI,int), FacitC),
	write_array(facit_o, array(0..MAXI,int), FacitO),
	write_array(facit_t, array(0..MAXP,int), FacitT),
	write_array(facit_r, array(-1..MAXT,int), [-1|FacitR]),
	write_array(facit_rt, array(0..MAXP,int), FacitRT).

model2dzn(AVL, MAXI, MAXP, MAXT) :-
	term_hash([], H),
	retractall(cur_set_index(_,_,_)),
	asserta(cur_set_index(H, [], 1)),
	retractall(cur_literal_index(_,_,_)),
	asserta(cur_literal_index(H, [], 0)),
	compute_bb(AVL, MAXI, MAXP, MAXT, MAXC, MAXO, MAXR,
		   BInsns, BOperands, BTemps, Frequency, MaxCycle, CopyMin, BBOrder),
	format('MAXI = ~d;\nMAXP = ~d;\nMAXT = ~d;\nMAXC = ~d;\nMAXO = ~d;\nMAXR = ~d;\n', [MAXI, MAXP, MAXT, MAXC, MAXO, MAXR]),
	length(BInsns, Nbb),
	write_array(bb_insns, array(1..Nbb,set(int)), BInsns),
	write_array(bb_operands, array(1..Nbb,set(int)), BOperands),
	write_array(bb_temps, array(1..Nbb,set(int)), BTemps),
	write_array(bb_frequency, array(1..Nbb,int), Frequency),
	write_array(bb_maxcycle, array(1..Nbb,int), MaxCycle),
	write_array(bb_copymin, array(1..Nbb,int), CopyMin),
	write_array(bb_order, array(1..Nbb,int), BBOrder),
	%
	compute_insn(AVL, IOperands, Operations, Type, Mand, IOrder),
	write_array(insn_operands, array(0..MAXI,set(int)), IOperands),
	write_array(insn_operations, array(0..MAXI,set(int)), Operations),
	write_array(insn_type, array(0..MAXI,int), Type),
	write_array(insn_mand, array(0..MAXI,bool), Mand),
	write_array(insn_order, array(0..MAXI,int), IOrder),
	%
	avl_fetch(atoms, AVL, Atoms1),
	encode(list(list(int)), list(set(int)),Atoms1, Atoms2),
	length(Atoms2, Natoms1),
	Natoms is Natoms1-1,
	write_array(atom_regs, array(-1..Natoms,set(int)), [[]|Atoms2]),
	%
	avl_fetch(calleesaved, AVL, CalleeSaved1),
	encode(list(int), set(int), CalleeSaved1, CalleeSaved2),
	write_array(calleesaved, set(int), CalleeSaved2),
	avl_fetch(callersaved, AVL, CallerSaved1),
	encode(list(int), set(int), CallerSaved1, CallerSaved2),
	write_array(callersaved, set(int), CallerSaved2),
	avl_fetch(infinite, AVL, Infinite1),
	avl_fetch(range, AVL, Range),
	(   foreach(Inf1,Infinite1),
	    foreach([X4,Y4],Range),
	    fromto(Infinite2,Infinite3,Infinite4,[])
	do  (   Inf1 = 0 -> Infinite3 = Infinite4
	    ;   Infinite3 = [[X4|Y4]|Infinite4]
	    )
	),
	write_array(infinite, set(int), Infinite2),
	%
	avl_fetch(cap, AVL, Cap),
	length(Cap, Nres),
	write_array(res_cap, array(1..Nres,int), Cap),
	avl_fetch(con, AVL, Con),
	transpose(Con, ConT),
	write_array(res_con, array(1..Nres,0..MAXO,int), ConT),
	avl_fetch(dur, AVL, Dur),
	transpose(Dur, DurT),
	write_array(res_dur, array(1..Nres,0..MAXO,int), DurT),
	%
	avl_fetch(strictly_congr, AVL, Congr1),
	(   foreach(L2,Congr1),
	    fromto(Congr2,Congr3,Congr4,[])
	do  (   L2 = [_] -> Congr3 = Congr4
	    ;   encode(list(int), set(int), L2, S2),
		Congr3 = [S2|Congr4]
	    )
	),
	length(Congr2, Ncongr),
	write_array(congr, array(1..Ncongr,set(int)), Congr2),
	%
	avl_fetch(preassign, AVL, Preassign),
	(   foreach([X1,Y1],Preassign),
	    foreach(X1,PreassignX),
	    foreach(Y1,PreassignY)
	do  true
	),
	length(Preassign, Npreassign),
	write_array(preassign_operand, array(1..Npreassign,int), PreassignX),
	write_array(preassign_reg, array(1..Npreassign,int), PreassignY),
	%
	avl_fetch(aligned, AVL, Aligned),
	avl_fetch(adist, AVL, ADist),
	(   foreach([X3,Y3],Aligned),
	    foreach(X3,AlignedX),
	    foreach(Y3,AlignedY)
	do  true
	),
	length(Aligned, Naligned),
	write_array(aligned_def, array(1..Naligned,int), AlignedX),
	write_array(aligned_use, array(1..Naligned,int), AlignedY),
	write_array(aligned_dist, array(1..Naligned,int), ADist),
	%
	avl_fetch(temps, AVL, Temps),
	avl_fetch(use, AVL, Use),
	avl_fetch(last_use, AVL, LastUse),
	(   foreach(Temps1,Temps),
	    foreach(Use1,Use),
	    foreach(S1,TempsArray),
	    foreach(Temps2,Temps2s),
	    foreach(Use3,UseArray),
	    foreach(Use4,LastUseArray),
	    fromto(TempDefs1,TempDefs2,TempDefs3,[]),
	    fromto(Copyrel1,Copyrel2,Copyrel3,[]),
	    count(P,0,_),
	    param(LastUse)
	do  int2bool(Use1, Use3),
	    (ord_member(P, LastUse) -> Use4 = true ; Use4 = false),
	    (   Use1=:=0, Temps1 = [-1|Temps2] -> true
	    ;   Temps1 = Temps2
	    ),
	    (   Use1=:=1 -> TempDefs2 = TempDefs3
	    ;   TempDefs2 = [P|TempDefs3]
	    ),
	    encode(list(int), set(int), Temps2, S1),
	    Temps2 = [Canon|_],
	    (   Canon >= 0,
		Use1=:=1
	    ->  Copyrel2 = [Canon-Temps2|Copyrel3]
	    ;   Copyrel2 = Copyrel3
	    )
	),
	keysort(Copyrel1, Copyrel4),
	keyclumped(Copyrel4, Copyrel5),
	(   foreach(_-Clump,Copyrel5),
	    fromto(Copyrel6,Copyrel7,Copyrel8,[])
	do  ord_union(Clump, Union),
	    (   Union = [_] -> Copyrel7 = Copyrel8
	    ;   encode(list(int), set(int), Union, Set2),
		Copyrel7 = [Set2|Copyrel8]
	    )
	),
	compute_definer(AVL, DefinerArray),
	compute_regset(AVL, Regset),
	write_array(operand_definer, array(0..MAXP,int), DefinerArray),
	write_array(operand_use, array(0..MAXP,bool), UseArray),
	write_array(operand_lastuse, array(0..MAXP,bool), LastUseArray),
	write_array(operand_temps, array(0..MAXP,set(int)), TempsArray),
	write_array(operand_atom, array(0..MAXP,0..MAXO,int), Regset),
	write_array(related_temps, array(1.._,set(int)), Copyrel6),
	%
	avl_fetch(width, AVL, Width),
	avl_fetch(definer, AVL, Definer),
	avl_fetch(minlive, AVL, MinLive),
	avl_fetch(unsafe_temp, AVL, UnsafeTemp),
	(   for(T,0,MAXT),
	    foreach(Uns,UnsafeTempArray),
	    param(UnsafeTemp)
	do  (ord_member(T,UnsafeTemp) -> Uns = true ; Uns = false)
	),
	compute_operands_array(Temps2s, OperandsArray),
	write_array(temp_definer, array(0..MAXT,int), Definer),
	write_array(temp_def, array(0..MAXT,int), TempDefs1),
	write_array(temp_width, array(0..MAXT,int), Width),
	write_array(temp_minlive, array(0..MAXT,int), MinLive),
	write_array(temp_unsafe, array(0..MAXT,bool), UnsafeTempArray),
	write_array(temp_uses, array(0..MAXT,set(int)), OperandsArray),
	%
	avl_fetch(before, AVL, Before1),
	avl_fetch(before2, AVL, Before2),
	append(Before1, Before2, Before3),
	(   foreach([P1,Q1,Disj1],Before3),
	    foreach(P1,BeforePred),
	    foreach(Q1,BeforeSucc),
	    foreach(Disj2,BeforeCond)
	do  encode(disj, set(int), Disj1, Disj2)
	),
	length(Before3, Nbefore),
	write_array(before_pred, array(1..Nbefore,int), BeforePred),
	write_array(before_succ, array(1..Nbefore,int), BeforeSucc),
	write_array(before_cond, array(1..Nbefore,set(int)), BeforeCond),
	%
	avl_fetch(nogoods, AVL, Nogoods1),
	(   foreach(Conj1,Nogoods1),
	    foreach(Conj2,Nogoods2)
	do  encode(conj, int, Conj1, Conj2)
	),
	length(Nogoods1, Nnogood),
	write_array(nogood, array(1..Nnogood,int), Nogoods2),
	%
	compute_across(AVL, AcrossInsn, AcrossRegs, AcrossItems, AcrossTemps, AcrossConds),
	length(AcrossInsn, Nacross),
	write_array(across_insn, array(1..Nacross,int), AcrossInsn),
	write_array(across_regs, array(1..Nacross,set(int)), AcrossRegs),
	write_array(across_items, array(1..Nacross,set(int)), AcrossItems),
	length(AcrossTemps, Nacross_items),
	write_array(across_item_temp, array(1..Nacross_items,int), AcrossTemps),
	write_array(across_item_cond, array(1..Nacross_items,set(int)), AcrossConds),
	%
	compute_setacross(AVL, SetacrossInsn, SetacrossRegs, SetacrossTempSets),
	length(SetacrossInsn, Nsetacross),
	write_array(setacross_insn, array(1..Nsetacross,int), SetacrossInsn),
	write_array(setacross_regs, array(1..Nsetacross,set(int)), SetacrossRegs),
	write_array(setacross_tempsets, array(1..Nsetacross,set(int)), SetacrossTempSets),
	%
	avl_fetch(diffops, AVL, Diffops1),
	encode(list(list(int)), list(set(int)), Diffops1, Diffops2),
	length(Diffops1, Ndiffop),
	write_array(diffop, array(1..Ndiffop,set(int)), Diffops2),
	%
	avl_fetch(diffregs, AVL, Diffregs1),
	encode(list(list(int)), list(set(int)), Diffregs1, Diffregs2),
	length(Diffregs1, Ndiffreg),
	write_array(diffreg, array(1..Ndiffreg,set(int)), Diffregs2),
	%
	avl_fetch(domops, AVL, Domop),
	(   foreach([L3,L4],Domop),
	    foreach(S3,DomopOpnds),
	    foreach(S4,DomopTemps)
	do  encode(list(int), set(int), L3, S3),
	    encode(list(int), set(int), L4, S4)
	),
	length(Domop, Ndomop),
	write_array(domop_operands, array(1..Ndomop, set(int)), DomopOpnds),
	write_array(domop_temps, array(1..Ndomop, set(int)), DomopTemps),
	%
	avl_fetch(domuses, AVL, Domuse),
	(   foreach([P2,Q2,R2],Domuse),
	    fromto(Ps1,Ps2,Ps3,[]),
	    fromto(Qs1,Qs2,Qs3,[]),
	    fromto(Rs1,Rs2,Rs3,[]),
	    param(LastUse,Preassign)
	do  (   member(P2, LastUse)
	    ->  Ps2 = Ps3, Qs2 = Qs3, Rs2 = Rs3
	    ;   member([P2,_], Preassign)
	    ->  Ps2 = Ps3, Qs2 = Qs3, Rs2 = Rs3
	    ;   Ps2 = [P2|Ps3], Qs2 = [Q2|Qs3], Rs2 = [R2|Rs3]
	    )
	),
	length(Domuse, Ndomuse),
	write_array(domuse_p, array(1..Ndomuse,int), Ps1),
	write_array(domuse_q, array(1..Ndomuse,int), Qs1),
	write_array(domuse_r, array(1..Ndomuse,int), Rs1),
	%
	avl_fetch(dominates, AVL, Dominate),
	(   foreach([I1,J1,L5,L6],Dominate),
	    foreach(I1,DominateIng),
	    foreach(J1,DominateEd),
	    foreach(S5,DominateOps),
	    foreach(S6,DominateTemps)
	do  encode(list(int), set(int), L5, S5),
	    encode(list(int), set(int), L6, S6)
	),
	length(Dominate, Ndominate),
	write_array(dominate_ing, array(1..Ndominate,int), DominateIng),
	write_array(dominate_ed, array(1..Ndominate,int), DominateEd),
	write_array(dominate_operations, array(1..Ndominate,set(int)), DominateOps),
	write_array(dominate_temps, array(1..Ndominate,set(int)), DominateTemps),
	%
	avl_fetch(precedences, AVL, Precedence1),
	avl_fetch(precedences2, AVL, Precedence2),
	append(Precedence1, Precedence2, Precedence),
	(   foreach([I2,J2,N2,Disj3],Precedence),
	    foreach(I2,PrecedencePred),
	    foreach(J2,PrecedenceSucc),
	    foreach(N2,PrecedenceDiff),
	    foreach(Disj4,PrecedenceCond)
	do  encode(disj, set(int), Disj3, Disj4)
	),
	length(Precedence, Nprecedence),
	write_array(precedence_pred, array(1..Nprecedence,int), PrecedencePred),
	write_array(precedence_succ, array(1..Nprecedence,int), PrecedenceSucc),
	write_array(precedence_diff, array(1..Nprecedence,int), PrecedenceDiff),
	write_array(precedence_cond, array(1..Nprecedence,set(int)), PrecedenceCond),
	%
	compute_copy_tables(AVL, Exists, Iffall,
			    RelationInsns, RelationTemps, RelationNtuples,
			    RelationRange, Int),
	length(Exists, Nexists),
	length(Iffall, Niffall),
	length(RelationInsns, Nrelation),
	length(Int, Nint),
	write_array(table_exists_insns, array(1..Nexists,set(int)), Exists),
	write_array(table_iffall_insns, array(1..Niffall,set(int)), Iffall),
	write_array(relation_insns, array(1..Nrelation,set(int)), RelationInsns),
	write_array(relation_temps, array(1..Nrelation,set(int)), RelationTemps),
	write_array(relation_ntuples, array(1..Nrelation,int), RelationNtuples),
	write_array(relation_range, array(1..Nrelation,set(int)), RelationRange),
	write_array(ints, array(1..Nint,int), Int),
	%
	avl_fetch(calleesaved_spill, AVL, Spill1),
	(   foreach(L7,Spill1),
	    foreach(S7,Spill2)
	do  encode(list(int), set(int), L7, S7)
	),
	write_array(calleesaved_spill, array(1.._,set(int)), Spill2),
	(   Spill1=[]
	->  write_array(cs_spill_transpose, array(0..0,1..0,int), [])
	;   transpose(Spill1, SpillT),
	    write_array(cs_spill_transpose, array(1.._,1.._,int), SpillT)
	),
	%
	literals_postlude(LOp, LArg1, LArg2),
	length(LOp, Nliteral),
	write_array(literal_op, array(1..Nliteral,int), LOp),
	write_array(literal_arg1, array(1..Nliteral,int), LArg1),
	write_array(literal_arg2, array(1..Nliteral,int), LArg2),
	sets_postlude(Sets),
	length(Sets, Nset),
	write_array(sets, array(1..Nset,set(int)), Sets).

compute_bb(AVL, MAXI, MAXP, MAXT, MAXC, MAXO, MAXR,
	   Insns, Operands, Temps, Frequency, MaxCycle, CopyMin, BBOrder) :-
	avl_fetch(freq, AVL, Frequency),
	avl_fetch(maxc, AVL, MaxCycle),
	avl_fetch(copymin, AVL, CopyMin),
	avl_fetch(ins, AVL, Ins),
	avl_fetch(tmp, AVL, Tmp),
	avl_fetch(operands, AVL, AllOperands),
	(   foreach(Ins1,Ins),
	    foreach(Tmp1,Tmp),
	    foreach([[InsMin|InsMax]], Insns),
	    foreach(Opnds1, Operands),
	    foreach(Tmps1, Temps),
	    fromto(bb(-1,-1,-1),bb(InsPrev,OpndPrev,TmpPrev),bb(InsMax,OpndMax,TmpMax),
		   bb(MAXI,MAXP,MAXT)),
	    foreach(NLen-BB1,KL1),
	    foreach(_-BB2,KL2),
	    foreach(BB2,BBOrder),
	    count(BB1,1,_),
	    param(AllOperands)
	do  Ins1 = [InsMin|_],
	    InsMin is InsPrev+1,
	    OpndMin is OpndPrev+1,
	    TmpMin is TmpPrev+1,
	    last(Ins1, InsMax),
	    NLen is InsMin-InsMax,
	    (   Tmp1 = []	% no temps, no opnds?
	    ->  TmpMax is TmpMin-1,
		OpndMax is OpndMin-1,
		Opnds1 = [],
		Tmps1 = []
	    ;   last(Tmp1, TmpMax),
		prefix_length(AllOperands, OpPartA, InsMin),
		append(OpPartA, OpPartBC, AllOperands),
		NI is InsMax-InsPrev,
		prefix_length(OpPartBC, OpPartB, NI),
		append(OpPartB, OpFlatB),
		last(OpFlatB, OpndMax),
		Opnds1 = [[OpndMin|OpndMax]],
		Tmps1 = [[TmpMin|TmpMax]]
	    )
	),
	keysort(KL1, KL2),
	max_member(MAXC, MaxCycle),
	avl_fetch('O', AVL, OList),
	last(OList, MAXO),
	avl_fetch(atoms, AVL, Atoms),
	ord_union(Atoms, RList),
	last(RList, MAXR).

compute_insn(AVL, Operands2, Operations2, Type, Mand, IOrder) :-
	avl_fetch(operands, AVL, Operands1),
	avl_fetch(operations, AVL, Operations1),
	avl_fetch(ins, AVL, Ins),
	avl_fetch(temps, AVL, Temps),
	avl_fetch(type, AVL, Type),
	avl_fetch(use, AVL, Use),
	(   foreach(Opnds1,Operands1),
	    foreach(Ops1,Operations1),
	    foreach(Opnds2,Operands2),
	    foreach(Ops2,Operations2),
	    foreach(M,Mand)
	do  encode(list(int), set(int), Opnds1, Opnds2),
	    encode(list(int), set(int), Ops1, Ops2),
	    (Ops1 = [0|_] -> M = false ; M = true)
	),
	(   foreach(Ts,Temps),
	    foreach(U,Use),
	    fromto(KL1,KL2,KL4,[]),
	    param(Temps)
	do  (   U=:=0 -> KL2 = KL4
	    ;   (   foreach(T,Ts),
		    fromto(KL2,[T-(-1)|KL3],KL3,KL4)
		do  true
		)
	    )
	),
	keysort(KL1, KL5),
	keyclumped(KL5, KL6),
	ord_list_to_avl(KL6, Temp2Rank),
	(   foreach(Interval,Ins),
	    count(BB,1,_),
	    fromto(Operands1,Operands3,Operands5,[]),
	    fromto(KL7,KL8,KL10,[]),
	    param(Use,Temps,Temp2Rank)
	do  (   foreach(I,Interval),
		fromto(Operands3,[Ps|Operands4],Operands4,Operands5),
		fromto(KL8,[key(BB,R)-I|KL9],KL9,KL10),
		param(BB,Use,Temps,Temp2Rank)
	    do  (   Ps = [_,Pd],
		    nth0(Pd, Use, 0),
		    nth0(Pd, Temps, [-1,T2])
		->  avl_fetch(T2, Temp2Rank, Rank),
		    sumlist(Rank, R)
		;   R = 0
		)
	    )
	),
	keysort(KL7, KL11),
	(   foreach(_-J,KL11),
	    foreach(J,IOrder)
	do  true
	).

compute_across(AVL, AcrossInsn, AcrossRegs, AcrossItems, AcrossTemps1, AcrossConds1) :-
	avl_fetch(across, AVL, Across1),
	(   foreach([I,RSet1,AItems1],Across1),
	    foreach(I,AcrossInsn),
	    foreach(RSet2,AcrossRegs),
	    foreach(AcrossItem,AcrossItems),
	    fromto(AcrossTemps1,AcrossTemps2,AcrossTemps4,[]),
	    fromto(AcrossConds1,AcrossConds2,AcrossConds4,[]),
	    fromto(1,AcrossIndex1,AcrossIndex3,_)
	do  encode(list(int), set(int), RSet1, RSet2),
	    length(AItems1, N),
	    AcrossIndex2 is AcrossIndex1 + N-1,
	    AcrossIndex3 is AcrossIndex2 + 1,
	    AcrossItem = [[AcrossIndex1|AcrossIndex2]],
	    (   foreach([T,Disj1],AItems1),
		fromto(AcrossTemps2,[T|AcrossTemps3],AcrossTemps3,AcrossTemps4),
		fromto(AcrossConds2,[Disj2|AcrossConds3],AcrossConds3,AcrossConds4)
	    do  encode(disj, set(int), Disj1, Disj2)
	    )
	).

compute_setacross(AVL, AcrossInsn, AcrossRegs, AcrossTempSets) :-
	avl_fetch(set_across, AVL, Across1),
	(   foreach([I,RSet1,TempSets1],Across1),
	    foreach(I,AcrossInsn),
	    foreach(RSet2,AcrossRegs),
	    foreach(TempSets2,AcrossTempSets)
	do  encode(list(int), set(int), RSet1, RSet2),
	    encode(list(list(int)), set(int), TempSets1, TempSets2)
	).

compute_copy_tables(AVL, Exists1, Iffall1,
		    RelationInsns, RelationTemps, RelationNtuples,
		    RelationRange, Int) :-
	avl_fetch(copy_tables, AVL, CopyTables),
	avl_fetch(tmp_tables, AVL, TmpCopyTables),
	(   foreach([Insns,Extension],CopyTables),
	    fromto(Exists1,Exists2,Exists3,[]),
	    fromto(Iffall1,Iffall2,Iffall3,[]),
	    fromto(Relation1,Relation2,Relation3,TmpCopyTables)
	do  (   Extension = [[0,0],[1,1]]
	    ->  Iffall2 = [ISet|Iffall3],
		Exists2 = Exists3,
		Relation2 = Relation3,
		encode(list(int), set(int), Insns, ISet)
	    ;   at_least_one_table(Extension)
	    ->  Exists2 = [ISet|Exists3],
		Iffall2 = Iffall3,
		Relation2 = Relation3,
		encode(list(int), set(int), Insns, ISet)
	    ;   Exists2 = Exists3,
		Iffall2 = Iffall3,
		Relation2 = [[Insns,[],Extension]|Relation3]
	    )
	),
	(   foreach([Insns2,Temps2,Extension2],Relation1),
	    foreach(ISet2,RelationInsns),
	    foreach(TSet2,RelationTemps),
	    foreach(NR,RelationNtuples),
	    foreach(Range,RelationRange),
	    foreach(Flat,Flats),
	    fromto(0,Sofar,Max,_)
	do  encode(list(int), set(int), Insns2, ISet2),
	    encode(list(int), set(int), Temps2, TSet2),
	    length(Extension2, NR),
	    append(Extension2, Flat),
	    length(Flat, NE),
	    Min is Sofar+1,
	    Max is Sofar+NE,
	    Range = (Min..Max)
	),
	append(Flats, Int).

% true if the relation is equivalent to a big OR
at_least_one_table(Table) :-
	Table = [Row|_],
	last(Row, 1),
	length(Row, N),
	length(Table, M),
	(2**N)-1 =:= M.

% operand -> insn
compute_definer(AVL, Array1) :-
	avl_fetch(operands, AVL, Operands),
	(   foreach(Opnds,Operands),
	    count(I,0,_),
	    fromto(Array1,Array2,Array4,[])
	do  (   foreach(_,Opnds),
		fromto(Array2,[I|Array3],Array3,Array4),
		param(I)
	    do  true
	    )
	).

% class(i,op,p) represented as regset[p][op] because i is a function of p
compute_regset(AVL, Regset1) :-
	avl_fetch(class, AVL, Class),
	avl_fetch(operands, AVL, Operands),
	avl_fetch(operations, AVL, Operations),
	avl_fetch('O', AVL, AllOps),
	(   foreach(Opnds,Operands),
	    foreach(Ops,Operations),
	    foreach(ClassI,Class),
	    fromto(Regset1,Regset2,Regset4,[]),
	    param(AllOps)
	do  (   ClassI = [[]] -> Regset2 = Regset4
	    ;   transpose(ClassI, ClassIT),
		(   foreach(_,Opnds),
		    foreach(ClassITP, ClassIT),
		    fromto(Regset2,[Row|Regset3],Regset3,Regset4),
		    param(AllOps,Ops)
		do  (   foreach(O,AllOps),
			foreach(Elt,Row),
			param(Ops,ClassITP)
		    do  (   nth0(I, Ops, O)
			->  nth0(I, ClassITP, Elt)
			;   Elt = -1
			)
		    )
		)
	    )
	).

% for temp_operands: compute sets of use operands
compute_operands_array(Tempss, OperandsArray) :-
	(   foreach(Temps,Tempss),
	    fromto(KL1,KL2,KL4,[]),
	    count(P,0,_)
	do  (   foreach(T,Temps),
		fromto(KL2,[T-P|KL3],KL3,KL4),
		param(P)
	    do  true
	    )
	),
	keysort(KL1, KL5),
	keyclumped(KL5, KL6),
	KL6 = [-1-_|KL7],
	(   foreach(Q-[_|Clump],KL7), % exclude the definer
	    foreach(Set,OperandsArray),
	    count(Q,0,_)
	do  encode(list(int), set(int), Clump, Set)
	).

int2bool(0, false).
int2bool(1, true).

encode(list(list(int)), set(int), L1, Set) :- !,
	(   foreach(X,L1),
	    foreach(Y,L2)
	do  encode(list(int), int, X, Y)
	),
	list_to_fdset(L2, Set).
encode(list(Type1), list(Type2), L1, L2) :- !,
	(   foreach(X,L1),
	    foreach(Y,L2),
	    param(Type1,Type2)
	do  encode(Type1, Type2, X, Y)
	).
encode(list(int), set(int), List, Set) :- !,
	list_to_fdset(List, Set).
encode(list(int), int, List, Index) :- !,
	list_to_fdset(List, Set),
	term_hash(Set, H),
	(   cur_set_index(H, Set, Index) -> true
	;   once(cur_set_index(_, _, Index0)),
	    Index is Index0+1,
	    asserta(cur_set_index(H, Set, Index))
	).
encode(disj, set(int), Disj1, Set) :-
	(   foreach(D1,Disj1),
	    foreach(D2,Disj2)
	do  encode(conj, int, D1, D2)
	),
	list_to_fdset(Disj2, Set).
encode(conj, int, Conj1, SetOfLit) :-
	(   foreach(C1,Conj1),
	    foreach(C2,Conj2)
	do  encode(literal, int, C1, C2)
	),
	encode(list(int), int, Conj2, SetOfLit).
encode(literal, int, Literal, Index) :-
	term_hash(Literal, H),
	(   cur_literal_index(H, Literal, Index) -> true
	;   once(cur_literal_index(_, _, Index0)),
	    Index is Index0+1,
	    asserta(cur_literal_index(H, Literal, Index))
	).

literals_postlude(Ops, Args1, Args2) :-
	findall(Literal, cur_literal_index(_,Literal,_), Literals1),
	reverse(Literals1, [_|Literals2]),
	(   foreach(Lit,Literals2),
	    foreach(Op, Ops),
	    foreach(X, Args1),
	    foreach(Y, Args2)
	do  (   Lit = [Op,X,Y] -> true
	    ;   Lit = [Op,X], Y = 0
	    )
	).

sets_postlude(Sets2) :-
	findall(Set, cur_set_index(_,Set,_), Sets1),
	reverse(Sets1, Sets2).

write_array(Name, array(IndexSet1,Type), Array) :-
	IndexSet1 = 1.._, !,
	write(Name),
	write(' = '),
	write_list(Type, Array),
	write(';\n').
write_array(Name, array(IndexSet1,Type), Array) :- !,
	write(Name),
	write(' = array1d('),
	write(IndexSet1),
	write(', '),
	write_list(Type, Array),
	write(');\n').
write_array(Name, array(IndexSet1,IndexSet2,Type), Array) :-
	IndexSet1 = 1.._,
	IndexSet2 = 1.._, !,
	write(Name),
	write(' = '),
	write_list2d(Type, Array),
	write(';\n').
write_array(Name, array(IndexSet1,IndexSet2,Type), Array) :- !,
	write(Name),
	write(' = array2d('),
	write(IndexSet1),
	write(', '),
	write(IndexSet2),
	write(', '),
	append(Array, List),
	write_list(Type, List),
	write(');\n').
write_array(Name, Type, Array) :-
	write(Name),
	write(' = '),
	write_elt(Type, Array),
	write(';\n').

write_list(int, L) :- !,
	write(L).
write_list(bool, L) :- !,
	write(L).
write_list(_, []) :- !,
	write([]).
write_list(Type, L) :-
	(   foreach(Elt,L),
	    fromto('[',Sep,', ',_),
	    param(Type)
	do  write(Sep),
	    write_elt(Type, Elt)
	),
	write(']').

write_list2d(Type, Rows) :-
	current_output(Stream),
	line_position(Stream, Pos),
	write('['),
	(   foreach(Row,Rows),
	    param(Pos,Type)
	do  (   foreach(X,Row),
		fromto('| ',Sep,', ', _),
		param(Type)
	    do  write(Sep),
		write_elt(Type, X)
	    ),
	    nl,
	    (for(_,0,Pos) do write(' '))
	),
	write('|]').

write_elt(int, X) :- !,
	write(X).
write_elt(bool, X) :- !,
	write(X).
write_elt(set(int), []) :- !,
	write({}).
write_elt(set(int), A..B) :- !,
	write(A..B).
write_elt(set(int), FD) :-
	(   foreach(R,FD),
	    fromto(Elts1,Elts2,Elts3,[]),
	    fromto(Rest1,Rest2,Rest3,[])
	do  R = [A|B],
	    (   B-A =:= 0
	    ->  Elts2 = [A|Elts3],
		Rest2 = Rest3
	    ;   B-A =:= 1
	    ->  Elts2 = [A,B|Elts3],
		Rest2 = Rest3
	    ;   B-A =:= 2
	    ->  C is A+1,
		Elts2 = [A,C,B|Elts3],
		Rest2 = Rest3
	    ;   Rest2 = [R|Rest3],
		Elts2 = Elts3
	    )
	),
	(   Elts1 = [] -> USep1 = ''
	;   USep1 = ' union ',
	    (   foreach(X,Elts1),
		fromto('{',Sep,', ',_)
	    do  write(Sep),
		write(X)
	    ),
	    write('}')
	),
	(   foreach([E|F],Rest1),
	    fromto(USep1,USep2,' union ',_)
	do  write(USep2),
	    write(E..F)
	).

replace_file_extension(InputName, BeforeExt, AfterExt, OutputName) :-
	atom_concat(BaseName, BeforeExt, InputName),
	atom_concat(BaseName, AfterExt, OutputName).

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

term_to_keylist((T1,T2)) --> !,
	term_to_keylist(T1),
	term_to_keylist(T2).
term_to_keylist(Qname:T) --> [Name-T],
	{atom_codes(Name, Qname)}.

