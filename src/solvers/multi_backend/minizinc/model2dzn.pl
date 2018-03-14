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
%   Generator of *.dzn from *.ext.json got code-generation.mzn
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
	cur_expr_index/3.

json2dzn(InputName) :-
	statistics(runtime, [T1,_]),
	replace_file_extension(InputName, '.ext.json', '.dzn', OutputName),
	json2avl(InputName, AVL1),
	tell(OutputName),
	call_cleanup(model2dzn(AVL1), told),
	statistics(runtime, [T2,_]),
	print_message(warning, format('generated ~w in ~d msec',[OutputName,T2-T1])).

model2dzn(AVL0) :-
	term_hash([], H),
	retractall(cur_set_index(_,_,_)),
	asserta(cur_set_index(H, [], 1)),
	retractall(cur_expr_index(_,_,_)),
	asserta(cur_expr_index(H, [], 0)),
	avl_fetch(use, AVL0, Use0),
	(   foreach(U0,Use0),
	    foreach(U,Use)
	do  bool_to_int(U0, U)
	),
	avl_store(use, AVL0, Use, AVL),
	compute_bb(AVL, MAXF, MAXO, MAXP, MAXT, MAXC, MAXI, MAXR,
		   BInsns, BOperands, BTemps, BSubsumed, Frequency, MaxCycle, OptionalMin, BBOrder),
	format('MAXF = ~d;\nMAXO = ~d;\nMAXP = ~d;\nMAXT = ~d;\nMAXC = ~d;\nMAXI = ~d;\nMAXR = ~d;\n', [MAXF, MAXO, MAXP, MAXT, MAXC, MAXI, MAXR]),
	length(BInsns, Nbb),
	avl_fetch(optimize_resource, AVL0, OptRes),
	(OptRes = [-1] -> OptCyc = true ; OptCyc = false),
	format('optimize_cycles = ~w;\n', [OptCyc]),
	write_array(bb_ops, array(1..Nbb,set(int)), BInsns),
	write_array(bb_operands, array(1..Nbb,set(int)), BOperands),
	write_array(bb_temps, array(1..Nbb,set(int)), BTemps),
	write_array(bb_subsumed, array(1..Nbb,set(int)), BSubsumed),
	write_array(bb_frequency, array(1..Nbb,int), Frequency),
	write_array(bb_maxcycle, array(1..Nbb,int), MaxCycle),
	write_array(bb_optional_min, array(1..Nbb,int), OptionalMin),
	write_array(bb_order, array(1..Nbb,int), BBOrder),
	%
	compute_insn(AVL, IOperands, Instructions, Type, Mand, IOrder),
	write_array(op_operands, array(0..MAXO,set(int)), IOperands),
	write_array(op_instructions, array(0..MAXO,set(int)), Instructions),
	write_array(op_type, array(0..MAXO,int), Type),
	write_array(op_mand, array(0..MAXO,bool), Mand),
	write_array(op_order, array(0..MAXO,int), IOrder),
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
	%
	avl_fetch(callersaved, AVL, CallerSaved1),
	encode(list(int), set(int), CallerSaved1, CallerSaved2),
	write_array(callersaved, set(int), CallerSaved2),
	%
	avl_fetch(infinite, AVL, Infinite1),
	avl_fetch(range, AVL, Range),
	avl_fetch(bounded, AVL, Bounded),
	(   foreach(Inf1,Infinite1),
	    foreach([X4,Y4],Range),
	    foreach([[X4|Y4]],RangeAsSet),
	    fromto(Infinite2,Infinite3,Infinite4,[])
	do  (   Inf1 = false -> Infinite3 = Infinite4
	    ;   Infinite3 = [[X4|Y4]|Infinite4]
	    )
	),
	length(Range, NR),
	write_array(infinite, set(int), Infinite2),
	write_array(range, array(1..NR,set(int)), RangeAsSet),
	write_array(bounded, array(1..NR,bool), Bounded),
	%
	avl_fetch(cap, AVL, Cap),
	length(Cap, Nres),
	write_array(res_cap, array(1..Nres,int), Cap),
	avl_fetch(con, AVL, Con),
	transpose(Con, ConT),
	write_array(res_con, array(1..Nres,0..MAXI,int), ConT),
	avl_fetch(dur, AVL, Dur),
	transpose(Dur, DurT),
	write_array(res_dur, array(1..Nres,0..MAXI,int), DurT),
	avl_fetch(off, AVL, Off),
	transpose(Off, OffT),
	write_array(res_off, array(1..Nres,0..MAXI,int), OffT),
	%
	avl_fetch(congr, AVL, Congr1),
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
	avl_fetch(strictly_congr, AVL, SCongr1),
	(   foreach(SL2,SCongr1),
	    fromto(SCongr2,SCongr3,SCongr4,[])
	do  (   SL2 = [_] -> SCongr3 = SCongr4
	    ;   encode(list(int), set(int), SL2, SS2),
		SCongr3 = [SS2|SCongr4]
	    )
	),
	length(SCongr2, NScongr),
	write_array(strictly_congr, array(1..NScongr,set(int)), SCongr2),
	%
	pairs_to_arrays(AVL, preassign, preassign_operand, preassign_reg),
	%
	avl_fetch(aligned, AVL, Aligned),
	avl_fetch(adist, AVL, ADist),
	(   foreach([X3,Xi3,Y3,Yi3],Aligned),
	    foreach(X3,AlignedX),
	    foreach(Xi3,AlignedXi),
	    foreach(Y3,AlignedY),
	    foreach(Yi3,AlignedYi)
	do  true
	),
	length(Aligned, Naligned),
	write_array(aligned_def, array(1..Naligned,int), AlignedX),
	write_array(aligned_use, array(1..Naligned,int), AlignedY),
	write_array(aligned_defi, array(1..Naligned,int), AlignedXi),
	write_array(aligned_usei, array(1..Naligned,int), AlignedYi),
	write_array(aligned_dist, array(1..Naligned,int), ADist),
	%
	pairs_to_arrays(AVL, adjacent, adj_from, adj_to),
	%
	pairs_to_arrays(AVL, quasi_adjacent, quasi_adj_from, quasi_adj_to),
	%
	avl_fetch(long_latency_index, AVL, LongLatencyIndex1),
	avl_fetch(long_latency_def_use, AVL, LongLatencyDU),
	encode(list(list(list(int))), list(list(set(int))), LongLatencyIndex1, LongLatencyIndex2),
	length(LongLatencyIndex2, LLI),
	write_array(long_latency_index, array(1..LLI,1..4,set(int)), LongLatencyIndex2),
	data_to_arrays_zero_based(LongLatencyDU, long_latency_def, long_latency_use),
	%
	avl_fetch(last_use, AVL, LastUse),
	avl_fetch(temps, AVL, Temps),
	findall(V, (nth0(P99,Temps,[-1,V]), nth0(P99,Use,0)), TOpt1),
	sort(TOpt1, TOpt),	% all optional temps
	(   foreach(Temps1,Temps),
	    foreach(Use1,Use),
	    foreach(S1,TempsArray),
	    foreach(Use3,UseArray),
	    foreach(Use4,LastUseArray),
	    fromto(Copyrel1,Copyrel2,Copyrel3,[]),
	    count(P,0,_),
	    param(LastUse,Type,TOpt)
	do  int2bool(Use1, Use3),
	    (ord_member(P, LastUse) -> Use4 = true ; Use4 = false),
	    encode(list(int), set(int), Temps1, S1),
	    Temps1 = [Canon|_],
	    (   Use1=:=1,
		ord_nonmember(Canon, [-1|TOpt])
	    ->  Copyrel2 = [Canon-Temps1|Copyrel3]
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
	compute_regset(AVL, Regset),
	compute_definer(AVL, DefinerArray),
	write_array(operand_definer, array(0..MAXP,int), DefinerArray),
	write_array(operand_use, array(0..MAXP,bool), UseArray),
	write_array(operand_lastuse, array(0..MAXP,bool), LastUseArray),
	write_array(operand_temps, array(0..MAXP,set(int)), TempsArray),
	write_array(operand_atom, array(0..MAXP,0..MAXI,int), Regset),
	write_array(related_temps, array(1.._,set(int)), Copyrel6),
	%
	avl_fetch(width, AVL, Width),
	avl_fetch(definer, AVL, Definer),
	avl_fetch(minlive, AVL, MinLive),
	(   foreach(TDef1,Definer),
	    foreach(TDef2,TDefs),
	    param(DefinerArray)
	do  nth0(TDef1, DefinerArray, TDef2)
	),
	compute_operands_array(Temps, Use0, OperandsArray),
	write_array(temp_definer, array(0..MAXT,int), TDefs), % defining operation
	write_array(temp_def, array(0..MAXT,int), Definer),   % defining operand
	write_array(temp_width, array(0..MAXT,int), Width),
	write_array(temp_minlive, array(0..MAXT,int), MinLive),
	write_array(temp_uses, array(0..MAXT,set(int)), OperandsArray),
	%
	avl_fetch(packed, AVL, Packed),
	write_array(packed_pq, array(1.._,1..2,int), Packed),
	%
	avl_fetch(before, AVL, Before1),
	% avl_fetch(before2, AVL, Before2),
	% append(Before1, Before2, Before12),
	(   foreach([P1,Q1,Disj1],Before1),
	    foreach(P1,BeforePred),
	    foreach(Q1,BeforeSucc),
	    foreach(Disj2,BeforeCond)
	do  encode(expr, int, Disj1, Disj2)
	),
	length(BeforePred, Nbefore),
	write_array(before_pred, array(1..Nbefore,int), BeforePred),
	write_array(before_succ, array(1..Nbefore,int), BeforeSucc),
	write_array(before_cond, array(1..Nbefore,int), BeforeCond),
	%
	avl_fetch(nogoods, AVL, Nogoods1),
	(   foreach(Conj1,Nogoods1),
	    foreach(Conj2,Nogoods2)
	do  encode(expr, int, Conj1, Conj2)
	),
	length(Nogoods1, Nnogood),
	write_array(nogood, array(1..Nnogood,int), Nogoods2),
	%
	compute_across(AVL, AcrossInsn, AcrossRegs, AcrossItems, AcrossTemps, AcrossConds),
	length(AcrossInsn, Nacross),
	write_array(across_op, array(1..Nacross,int), AcrossInsn),
	write_array(across_regs, array(1..Nacross,set(int)), AcrossRegs),
	write_array(across_items, array(1..Nacross,set(int)), AcrossItems),
	length(AcrossTemps, Nacross_items),
	write_array(across_item_temp, array(1..Nacross_items,int), AcrossTemps),
	write_array(across_item_cond, array(1..Nacross_items,int), AcrossConds),
	%
	compute_setacross(AVL, SetacrossInsn, SetacrossRegs, SetacrossTempSets),
	length(SetacrossInsn, Nsetacross),
	write_array(setacross_op, array(1..Nsetacross,int), SetacrossInsn),
	write_array(setacross_regs, array(1..Nsetacross,set(int)), SetacrossRegs),
	write_array(setacross_tempsets, array(1..Nsetacross,set(int)), SetacrossTempSets),
	%
	avl_fetch(difftemps, AVL, Difftemps1),
	encode(list(list(int)), list(set(int)), Difftemps1, Difftemps2),
	length(Difftemps1, Ndifftemp),
	write_array(difftemp, array(1..Ndifftemp,set(int)), Difftemps2),
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
	avl_fetch(infassign, AVL, InfAssign),
	length(InfAssign, NIA),
	write_array(infassign, array(1..NIA,1..4,int), InfAssign),
	%
	avl_fetch(space, AVL, Space),
	length(Space, NSpace),
	NS1 is NSpace-1,
	write_array(space, array(-1..NS1,int), [-1|Space]),
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
	write_array(dominate_instructions, array(1..Ndominate,set(int)), DominateOps),
	write_array(dominate_temps, array(1..Ndominate,set(int)), DominateTemps),
	%
	avl_fetch(precedences, AVL, Precedence1),
	% avl_fetch(precedences2, AVL, Precedence2),
	% append(Precedence1, Precedence2, Precedence12),
	(   foreach(Pre1,Precedence1),
	    foreach(Pre2,Precedence3)
	do  encode(expr, int, Pre1, Pre2)
	),
	length(Precedence3, Nprecedence),
	write_array(precedence, array(1..Nprecedence,int), Precedence3),
	%
	compute_active_tables(AVL, Exists, Iffall,
			      RelationInsns, RelationTemps, RelationNtuples,
			      RelationRange, Int),
	length(Exists, Nexists),
	length(Iffall, Niffall),
	length(RelationInsns, Nrelation),
	length(Int, Nint),
	write_array(table_exists_ops, array(1..Nexists,set(int)), Exists),
	write_array(table_iffall_ops, array(1..Niffall,set(int)), Iffall),
	write_array(relation_ops, array(1..Nrelation,set(int)), RelationInsns),
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
	avl_fetch(activators, AVL, Activators),
	(   foreach(Insns1,Activators),
	    foreach(Insns2-I3,KL1),
	    count(I3,0,_)
	do  sort(Insns1,Insns2)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(Insns3-Ops3,KL3),
	    fromto(ActInsns1,ActInsns2,ActInsns3,[]),
	    fromto(ActOps1,ActOps2,ActOps3,[])
	do  (   Insns3 = []
	    ->  ActInsns2 = ActInsns3,
		ActOps2 = ActOps3
	    ;   encode(list(int), set(int), Insns3, Insns4),
		encode(list(int), set(int), Ops3, Ops4),
		ActInsns2 = [Insns4|ActInsns3],
		ActOps2 = [Ops4|ActOps3]
	    )
	),
	write_array(activator_insns, array(1.._,set(int)), ActInsns1),
	write_array(activator_ops, array(1.._,set(int)), ActOps1),
	%
	avl_fetch(predecessors, AVL, Predecessors),
	(   foreach([PPred,PSucc,PLat],Predecessors),
	    foreach(PPredSet,PPredSets),
	    foreach(PSucc,PSuccs),
	    foreach(PLat,PLats)
	do  encode(list(int), set(int), PPred, PPredSet)
	),
	write_array(predecessors_preds, array(1.._,set(int)), PPredSets),
	write_array(predecessors_succ, array(1.._,int), PSuccs),
	write_array(predecessors_lat, array(1.._,int), PLats),
	%
	avl_fetch(successors, AVL, Successors),
	(   foreach([PPred2,PSucc2,PLat2],Successors),
	    foreach(PSuccSet2,PSuccSets2),
	    foreach(PPred2,PPreds2),
	    foreach(PLat2,PLats2)
	do  encode(list(int), set(int), PSucc2, PSuccSet2)
	),
	write_array(successors_succs, array(1.._,set(int)), PSuccSets2),
	write_array(successors_pred, array(1.._,int), PPreds2),
	write_array(successors_lat, array(1.._,int), PLats2),
	%
	avl_fetch(value_precede_chains, AVL, VPChain),
	(   foreach([VPTs,VPRss],VPChain),
	    foreach(VPTs,VPTss),
	    fromto(0,Cum0,Cum2,_),
	    fromto(Trip1,Trip2,Trip5,[])
	do  length(VPTs, VPTsn),
	    Cum1 is Cum0+1,
	    Cum2 is Cum0+VPTsn,
	    (   VPTs = [] -> Trip2 = Trip5
	    ;   (   foreach(VPRs,VPRss),
		    fromto(Trip2,Trip3,Trip4,Trip5),
		    param(Cum1,Cum2)
		do  Trip3 = [[Cum1,Cum2,VPRset]|Trip4],
		    encode(list(int), set(int), VPRs, VPRset)
		)
	    )
	),
	append(VPTss, VPTemps),
	(   Trip1 = [] -> [VPMin,VPMax,VPRegs] = [[],[],[]]
	;   transpose(Trip1, [VPMin,VPMax,VPRegs])
	),
	write_array(value_precede_min, array(1.._,int), VPMin),
	write_array(value_precede_max, array(1.._,int), VPMax),
	write_array(value_precede_regs, array(1.._,set(int)), VPRegs),
	write_array(value_precede_temps, array(1.._,int), VPTemps),
	%
	compute_lat_table(AVL, LatTable, []),
	length(LatTable, NLatTable),
	write_array(lat_table, array(1..NLatTable,1..4,int), LatTable),
	transpose(LatTable, [_,_,_,Lats]),
	min_member(MinLat, Lats),
	max_member(MaxLat, Lats),
	format('MINL = ~d;\nMAXL = ~d;\n', [MinLat,MaxLat]),
	%
	pairs_to_arrays(AVL, preschedule, preschedule_op, preschedule_cycle),
	%
	pairs_to_arrays(AVL, exrelated, exrelated_p, exrelated_q),
	%
	avl_fetch(table, AVL, ExTables),
	(   foreach(ExTable,ExTables),
	    foreach(ExMin..ExMax,ExRanges),
	    fromto(0,SoFar1,SoFar2,_)
	do  length(ExTable, ExLen),
	    ExMin is SoFar1+1,
	    ExMax is SoFar1+ExLen,
	    SoFar2 = ExMax
	),
	write_array(exrelated_rows, array(1.._,set(int)), ExRanges),
	append(ExTables, ExTableA),
	length(ExTableA, ExLenA),
	write_array(exrelated_ext, array(1..ExLenA,1..2,int), ExTableA),
	%
	compute_bypass_table(AVL, BypassTable, []),
	length(BypassTable, NBypassTable),
	write_array(bypass_table, array(1..NBypassTable,1..3,int), BypassTable),
	%
	avl_fetch('E', AVL, Adhoc1),
	(   foreach(Expr1,Adhoc1),
	    foreach(Expr2,Adhoc2)
	do  encode(expr, int, Expr1, Expr2)
	),
	length(Adhoc1, Nadhoc),
	write_array(adhoc, array(1..Nadhoc,int), Adhoc2),
	%
	avl_fetch(temp_domain, AVL, TempDomain1),
	(   foreach(TD1,TempDomain1),
	    foreach(TD2,TempDomain2)
	do  list_to_fdset(TD1, TD2)
	),
	write_array(temp_domain, array(1..Nbb,set(int)), TempDomain2),
	%
	exprs_postlude(LOp, LArg1, LArg2, LArg3, Lchildren),
	length(LOp, Nexpr),
	write_array(expr_op, array(1..Nexpr,int), LOp),
	write_array(expr_arg1, array(1..Nexpr,int), LArg1),
	write_array(expr_arg2, array(1..Nexpr,int), LArg2),
	write_array(expr_arg3, array(1..Nexpr,int), LArg3),
	write_array(expr_children, array(1..Nexpr,set(int)), Lchildren),
	sets_postlude(Sets),
	length(Sets, Nset),
	write_array(sets, array(1..Nset,set(int)), Sets).

pairs_to_arrays(AVL, Tag, FirstArr, SecondArr) :-
	avl_fetch(Tag, AVL, Data),
	(   foreach([X1,Y1],Data),
	    foreach(X1,DataX),
	    foreach(Y1,DataY)
	do  true
	),
	length(Data, N),
	write_array(FirstArr, array(1..N,int), DataX),
	write_array(SecondArr, array(1..N,int), DataY).

data_to_arrays_zero_based(Data, FirstArr, SecondArr) :-
	(   foreach([X1,Y1],Data),
	    foreach(X1,DataX),
	    foreach(Y1,DataY)
	do  true
	),
	length(Data, N),
	M is N-1,
	write_array(FirstArr, array(0..M,int), DataX),
	write_array(SecondArr, array(0..M,int), DataY).

compute_bb(AVL, MAXF, MAXO, MAXP, MAXT, MAXC, MAXI, MAXR,
	   Insns, Operands, Temps, Subsumed, Frequency, MaxCycle, OptionalMin, BBOrder) :-
	avl_fetch(maxf, AVL, MAXF),
	avl_fetch(freq, AVL, Frequency),
	avl_fetch(maxc, AVL, MaxCycle),
	avl_fetch(optional_min, AVL, OptionalMin),
	avl_fetch(ops, AVL, Ops),
	avl_fetch(tmp, AVL, Tmp),
	avl_fetch(subsumed_resources, AVL, Subsumed1),
	avl_fetch(operands, AVL, AllOperands),
	(   foreach(Op1,Ops),
	    foreach(Tmp1,Tmp),
	    foreach(Sub1,Subsumed1),
	    foreach([[OpMin|OpMax]], Insns),
	    foreach(Opnds1,Operands),
	    foreach(Tmps1,Temps),
	    foreach(Sub,Subsumed),
	    fromto(bb(-1,-1,-1),bb(OpPrev,OpndPrev,TmpPrev),bb(OpMax,OpndMax,TmpMax),
		   bb(MAXO,MAXP,MAXT)),
	    foreach(NLen-BB1,KL1),
	    foreach(_-BB2,KL2),
	    foreach(BB2,BBOrder),
	    count(BB1,1,_),
	    param(AllOperands)
	do  Op1 = [OpMin|_],
	    OpMin is OpPrev+1,
	    OpndMin is OpndPrev+1,
	    TmpMin is TmpPrev+1,
	    last(Op1, OpMax),
	    NLen is OpMin-OpMax,
	    list_to_fdset(Sub1, Sub),
	    (   Tmp1 = []	% no temps, no opnds?
	    ->  TmpMax is TmpMin-1,
		OpndMax is OpndMin-1,
		Opnds1 = [],
		Tmps1 = []
	    ;   last(Tmp1, TmpMax),
		prefix_length(AllOperands, OpPartA, OpMin),
		append(OpPartA, OpPartBC, AllOperands),
		NI is OpMax-OpPrev,
		prefix_length(OpPartBC, OpPartB, NI),
		append(OpPartB, OpFlatB),
		last(OpFlatB, OpndMax),
		Opnds1 = [[OpndMin|OpndMax]],
		Tmps1 = [[TmpMin|TmpMax]]
	    )
	),
	keysort(KL1, KL2),
	max_member(MAXC, MaxCycle),
	avl_fetch('I', AVL, IList),
	last(IList, MAXI),
	avl_fetch(atoms, AVL, Atoms),
	ord_union(Atoms, RList),
	last(RList, MAXR).

compute_insn(AVL, Operands2, Instructions2, Type, Mand, IOrder) :-
	avl_fetch(operands, AVL, Operands1),
	avl_fetch(instructions, AVL, Instructions1),
	avl_fetch(ops, AVL, Insns),
	avl_fetch(temps, AVL, Temps),
	avl_fetch(type, AVL, Type),
	avl_fetch(use, AVL, Use),
	(   foreach(Opnds1,Operands1),
	    foreach(Insns1,Instructions1),
	    foreach(Opnds2,Operands2),
	    foreach(Insns2,Instructions2),
	    foreach(M,Mand)
	do  encode(list(int), set(int), Opnds1, Opnds2),
	    encode(list(int), set(int), Insns1, Insns2),
	    (Insns1 = [0|_] -> M = false ; M = true)
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
	(   foreach(Interval,Insns),
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
	    (   foreach([T,Expr1],AItems1),
		fromto(AcrossTemps2,[T|AcrossTemps3],AcrossTemps3,AcrossTemps4),
		fromto(AcrossConds2,[Expr2|AcrossConds3],AcrossConds3,AcrossConds4)
	    do  encode(expr, int, Expr1, Expr2)
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

compute_active_tables(AVL, Exists1, Iffall1,
		      RelationInsns, RelationTemps, RelationNtuples,
		      RelationRange, Int) :-
	avl_fetch(active_tables, AVL, ActiveTables),
	avl_fetch(tmp_tables, AVL, TmpActiveTables),
	(   foreach([Insns,Extension],ActiveTables),
	    fromto(Exists1,Exists2,Exists3,[]),
	    fromto(Iffall1,Iffall2,Iffall3,[]),
	    fromto(Relation1,Relation2,Relation3,TmpActiveTables)
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
	avl_fetch(instructions, AVL, Instructions),
	avl_fetch('I', AVL, AllInsns),
	(   foreach(Opnds,Operands),
	    foreach(Insns,Instructions),
	    foreach(ClassI,Class),
	    fromto(Regset1,Regset2,Regset4,[]),
	    param(AllInsns)
	do  (   Opnds = [] -> Regset2 = Regset4
	    ;   transpose(ClassI, ClassIT),
		(   foreach(_,Opnds),
		    foreach(ClassITP, ClassIT),
		    fromto(Regset2,[Row|Regset3],Regset3,Regset4),
		    param(AllInsns,Insns)
		do  (   foreach(I,AllInsns),
			foreach(Elt,Row),
			param(Insns,ClassITP)
		    do  (   nth0(O, Insns, I)
			->  nth0(O, ClassITP, Elt)
			;   Elt = -1
			)
		    )
		)
	    )
	).

% for temp_operands: compute sets of use operands
compute_operands_array(Tempss, Uses, OperandsArray) :-
	(   foreach(Temps,Tempss),
	    foreach(Use,Uses),
	    fromto(KL1,KL2,KL4,[]),
	    count(P,0,_)
	do  (   Use = false -> KL2 = KL4
	    ;   (   foreach(T,Temps),
		    fromto(KL2,[T-P|KL3],KL3,KL4),
		    param(P)
		do  true
		)
	    )
	),
	keysort(KL1, KL5),
	keyclumped(KL5, KL6),
	KL6 = [-1-_|KL7],
	(   foreach(Q-Clump,KL7),
	    foreach(Set,OperandsArray),
	    count(Q,0,_)
	do  encode(list(int), set(int), Clump, Set)
	).

compute_lat_table(AVL) -->
	{avl_fetch(lat, AVL, Lat)},
	{avl_fetch(instructions, AVL, Ins)},
	{avl_fetch(operands, AVL, Opnd)},
	(   foreach(LatO,Lat),
	    foreach(InsO,Ins),
	    foreach(OpndO,Opnd),
	    count(O,0,_)
	do  (   foreach(I,InsO),
		foreach(LatOI,LatO),
		param(OpndO,O)
	    do  (   foreach(P,OpndO),
		    foreach(L,LatOI),
		    param(O,I)
		do  [[O,I,P,L]]
		)
	    )
	).

compute_bypass_table(AVL) -->
	{avl_fetch(bypass, AVL, Bypass)},
	{avl_fetch(instructions, AVL, Ins)},
	{avl_fetch(operands, AVL, Opnd)},
	(   foreach(BypassO,Bypass),
	    foreach(InsO,Ins),
	    foreach(OpndO,Opnd),
	    count(O,0,_)
	do  (   foreach(I,InsO),
		foreach(BypassOI,BypassO),
		param(OpndO,O)
	    do  (   foreach(P,OpndO),
		    foreach(L,BypassOI),
		    param(O,I)
		do  ({L=true} -> [[O,I,P]] ; [])
		)
	    )
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
encode(expr, int, Expr, Index) :-
	expr_quad(Expr, Quad),
	encode_quad(Quad, Index).

encode_quad(Quad, Index) :-
	term_hash(Quad, H),
	(   cur_expr_index(H, Quad, Index) -> true
	;   once(cur_expr_index(_, _, Index0)),
	    Index is Index0+1,
	    asserta(cur_expr_index(H, Quad, Index))
	).

% 0..4 are connectives, 5..13 are true exprs
expr_quad([0|Args], Quad) :- !, % OR
	(   foreach(A,Args),
	    foreach(Q,Quads),
	    foreach(E,Encoded)
	do  expr_quad(A, Q),
	    encode_quad(Q, E)
	),
	(   Quads = [Q1]
	->  Quad = Q1
	;   Quad = [0,0,0,0,Encoded1],
	    sort(Encoded, Encoded1)
	).
expr_quad([1|Args], Quad) :- !, % AND
	(   foreach(A,Args),
	    foreach(Q,Quads),
	    foreach(E,Encoded)
	do  expr_quad(A, Q),
	    encode_quad(Q, E)
	),
	(   Quads = [Q1]
	->  Quad = Q1
	;   Quad = [1,0,0,0,Encoded1],
	    sort(Encoded, Encoded1)
	).
expr_quad([2,X,Y], Quad) :- !, % XOR
	expr_quad(X, Quad1),
	expr_quad(Y, Quad2),
	encode_quad(Quad1, I1),
	encode_quad(Quad2, I2),
	Quad = [2,I1,I2,0,[]].
expr_quad([3,[0|Args],Y], Quad) :- !, % P1 \/ ... \/ Pn IMPLIES Q ---> (P1->Q) /\ ... /\ (Pn->Q)
	(   foreach(A,Args),
	    foreach([3,A,Y],Conjuncts),
	    param(Y)
	do  true
	),
	expr_quad([1|Conjuncts], Quad).
expr_quad([3,[1|Args],Y], Quad) :- !, % P1 /\ ... /\ Pn IMPLIES Q ---> NOT P1 \/ ... \/ NOT Pn \/ Q
	(   foreach(A,Args),
	    foreach(D,Disjuncts)
	do  (A = [4,D] -> true ; D = [4,A])
	),
	expr_quad([0,Y|Disjuncts], Quad).
expr_quad([3,X,Y], Quad) :- !, % IMPLIES
	expr_quad(X, Quad1),
	expr_quad(Y, Quad2),
	encode_quad(Quad1, I1),
	encode_quad(Quad2, I2),
	Quad = [3,I1,I2,0,[]].
expr_quad([4,X], Quad) :- !, % NOT
	expr_quad(X, Quad1),
	encode_quad(Quad1, I1),
	Quad = [4,I1,0,0,[]].
expr_quad(Expr, Quad) :-	% expr proper
	(   Expr = [T,X] -> Quad = [T,X,0,0,[]]
	;   Expr = [T,X,Y] -> Quad = [T,X,Y,0,[]]
	;   Expr = [T,X,Y,Z] -> Quad = [T,X,Y,Z,[]]
	).

exprs_postlude(Ops, Args1, Args2, Args3, Children) :-
	findall(Expr, cur_expr_index(_,Expr,_), Exprs1),
	reverse(Exprs1, [_|Exprs2]),
	Exprs2 \== [], !,
	transpose(Exprs2, [Ops,Args1,Args2,Args3,ChildrenLists]),
	encode(list(list(int)), list(set(int)), ChildrenLists, Children).	
exprs_postlude([], [], [], [], []).

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
write_array(Name, array(1.._,1..B,_), []) :- !,
	format('~w = array2d(1..0, 1..~d, []);\n', [Name,B]).
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

bool_to_int(true, 1).
bool_to_int(false, 0).
