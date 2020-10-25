%
%  Main authors:
%    Mats Carlsson <mats.carlsson@ri.se>
%
%  Contributing authors:
%    Roberto Castaneda Lozano <rcas@acm.org>
%    Noric Couderc <noric@sics.se>
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
/***************************************************************
PROVISIONAL DOCUMENTATION OF GENERATED JSON

"before":       list of BeforeItem
"before2":      list of BeforeItem
"nogoods":      list of Nogood
"nogoods2":     list of Nogood
"across":       list of AcrossItem
"set_across":   list of SetAcrossItem
"difftemps":	list of DiffTemp
"diffregs":	list of DiffReg
"domops":	list of DomOp
"optional_min": list of OptionalMin, one per basic block
"dominates":    list of Domination
"last_use":     list of LastUse
"precedences":  list of Precedence
"precedences2": list of Precedence
"active_tables":list of ActiveTable
"tmp_tables":	list of CopyTmpTable
"unsafe_temp":  list of UnsafeTemp
"calleesaved_spill":  list of CalleeSavedSpill

Of the above, "before2", "nogoods2" and "precedences2" are inferred from
across-call analysis, whereas "before", "nogoods" and "precedences" are more basic.
"nogoods2" is expected to be totally subsumed by "across".

Please note! "precedences" _includes_ the data and fixed precedences of
the basic model.

// if Disj then live range of P precedes live range of Q
BeforeItem ::= [P,Q,Disj]

// Conj is false
Nogood ::= Conj

// For the given call insns I, the registers of:
// - the caller-saved registers plus the registers in Rset
// - for each temp T where [Disj is true OR ls(T) <= c(I) < le(I)], r(T) (ext. by width)
// must be all different
AcrossItem ::= [I,Rset,list of AcrossSubitem]

AcrossSubitem ::= [T,Disj]

// For the given call insns I, the registers of:
// - the caller-saved registers plus the registers in Rset
// - for each Tset, one member T for which [ls(T) <= c(I) < le(I)], r(T) (ext. by width)
// must be all different
SetAcrossItem ::= [I,Rset,list of Tset]

// The temps of the operands are all different
DiffTemp ::= Pset

// The regs of the temps of the operands are all different
DiffReg ::= Pset

// For the given operands, the given temps are interchangeable
/  let Tset = [T1,T2,T3...]
// among Pset, T1 must precede any use of T2, which must precede any use of T3, etc.
DomOp ::= [Pset,Tset]

// At least N copies are active in the basic block
OptionalMin ::= N

// a(I) or not a(J)
//      or i(J) in Iset
//      or   TJ in Tset
//      where TJ is the temp being used by the copy instruction J
Domination ::= [I,J,Iset,Tset]

// The operand P is the last use of its temporary
LastUse ::= P

// The temporary T may be live past its last use
UnsafeTemp ::= T

// For each CalleeSavedSpillSet, either all insns are active, or none
// The first CalleeSavedSpillSet dominates the second one, and so on
CalleeSavedSpill ::= list of CalleeSavedSpillSet

CalleeSavedSpillSet ::= list of I

// If Disj then c(I) + N <= c(J)
Precedence ::= [I,J,N,Disj]

// the 2nd element is a set of allowed combinations for the given a(I)
ActiveTable = [Iset,list of list of I]

// Table is a set of allowed combinations for the given a(I)++t(P)
// i.e. each row is of length |Iset| + |Pset|
CopyTmpTable = [Iset,Pset,list of list of I]

Disj ::= list of Conj // a disjunction

Conj ::= list of Lit // a conjunction

Lit ::= [0,P,Q] // the temp of P is the temp of Q
      | [1,P,T] // T is the temp of P
      | [2,O]   // a(I) = 1
      | [3,O,I] // i(O) = I
      | [4,P,Q] // operands P and Q have overlapping live ranges
      | [5,T,U] // temps T and U have overlapping live ranges
      | [6,T]   // temp T uses a caller-saved register
      | [7,O,I] // i(O) != I
      | [8,P,C] // operand P is connected to reg in class C

B ::= Boolean
I ::= integer // instruction
J ::= integer // instruction
N ::= integer // just a scalar
O ::= integer // operation
P ::= integer // operand
Q ::= integer // operand
R ::= integer // register
T ::= integer // temporary
U ::= integer // temporary
Iset ::= list of I
Oset ::= list of O
Pset ::= list of P
Rset ::= list of R
Tset ::= list of T
***************************************************************/

	% cur_operation/5,	 % (?Insn,?Type,?Instructions,?Uses,?Defs)
	% cur_operand/4,	 % (?Operand,?Insn,?Use,?Temps)
	% cur_temp/4.		 % (?Temp,?Width,?Operands,?Definer)

:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).
:- use_module(library(timeout)).

:- ensure_loaded('../solvers/multi_backend/common/common').
:- ensure_loaded('../solvers/multi_backend/common/lib').

:- dynamic
	cur_congruence/2,
	cur_difftemp/1,
	cur_p_before_p/3,
	cur_t_before_t/3,
	cur_subsumed_nogood/2,
	cur_redefiner/3,
	cur_remat/2,		% (OrigTemp, RematTemp)
	more_nogood/1,
	copy_table/2,
	tmp_table/3.

presolve(InputName) :-
	prolog_flag(argv, Argv),
	presolve(InputName, Argv).

presolve(InputName, Argv) :-
	statistics(walltime, [T1,_]),
	(   nextto('-t', TimeAtom, Argv)
	->  atom_codes(TimeAtom, TimeCodes),
	    number_codes(Quota, TimeCodes)
	;   Quota is 1<<28
	),
	(   nextto('-o', OutputName, Argv) -> true
	;   replace_file_extension(InputName, '.json', '.ext.json', OutputName)
	),
	time_out(presolve(InputName, OutputName, Quota), Quota, Flag),
	statistics(walltime, [T2,_]),
	(   Flag==time_out
	->  print_message(warning, format('presolving ~w timed out',[InputName]))
	;   print_message(warning, format('presolved ~w in ~d msec',[InputName,T2-T1]))
	).

presolve(InputName, OutputName, Quota) :-
	json2avl(InputName, AVL1),
	presolve_avl(AVL1, AVL2, Quota),
	avl2json(OutputName, AVL2).


presolve_avl(AVL0, AVL24, Quota) :-
	avl_fetch(def_opr, AVL0, Definer), % definer of opnd p
	avl_fetch(instructions, AVL0, Instructions), % set of instructions for operation o
	avl_fetch(operands, AVL0, Operands), % set of operands of insn i
	avl_fetch(temps, AVL0, Temps), % set of temporary candidates for op p
	avl_fetch(ops, AVL0, Ops),     % set of operations of block b
	avl_fetch(type, AVL0, Type),
	avl_fetch(dep, AVL0, Dep),
	avl_fetch(width, AVL0, Width),
	avl_fetch(use, AVL0, UseBool), % whether operand p is a use
	avl_fetch(atoms, AVL0, Atoms),
	(   foreach(UseBool1,UseBool),
	    foreach(Use1,Use)
	do  bool_to_int(UseBool1, Use1)
	),
	length(Type, MAXI1),
	MAXI is MAXI1-1,
	length(Temps, MAXP1),
	MAXP is MAXP1-1,
	length(Width, MAXT1),
	MAXT is MAXT1-1,
	retractall(more_nogood(_)),
	retractall(cur_subsumed_nogood(_,_)),
	retractall(cur_p_before_p(_,_,_)),
	retractall(cur_t_before_t(_,_,_)),
	retractall(cur_difftemp(_)),
	retractall(cur_redefiner(_,_,_)),
	retractall(cur_remat(_,_)),
	assert_json(Operands, Instructions, Type, Use, Temps, Width, Definer),
	gen_congr(AVL0, AVL1),
	gen_modulo_2(AVL1),
	gen_cur_congruence(AVL1),
	(   foreach(OpsBB,Ops),
	    foreach(DepBB,Dep),
	    foreach(DDGraph,DDGraphs)
	do  data_dependency_graph(OpsBB, DepBB, DDGraph)
	),
	last_use(AVL1, MAXI, MAXT, DDGraphs, LastUse),
	compute_opnd2lat(AVL1, Opnd2Lat),
	gen_unsafe_temp(AVL1, Opnd2Lat, UnsafeTemp),
	gen_before(AVL1, LastUse, UnsafeTemp, Opnd2Lat, Before0),
	before_vs_nogoods(Before0, Before, Nogoods1, Nogoods2),
	gen_dominates(MAXI, Dominates1, []),
	difftemps(AVL1, MAXI, LastUse, DiffTempsCliques),
	dominating_insns(AVL1, Nogoods2, Nogoods3),
	gen_infeasible(AVL1, MAXI, Before, Alldiffs1, Nogoods3, Nogoods4),
	findall(N, redefined_operand_nogood(Opnd2Lat, N), Nogoods4, Nogoods4c),
	findall(Fact, (cur_difftemp(Fact0), strip_p(Fact0,Fact)), DiffTemps1, DiffTempsCliques),
	sort(DiffTemps1, DiffTemps),
	(   foreach(Ps,Alldiffs1),
	    fromto(DiffRegs1,DiffRegs2,DiffRegs3,[])
	do  (   Ps = []
	    ->  DiffRegs2 = DiffRegs3
	    ;   Ps = [_]
	    ->  DiffRegs2 = DiffRegs3
	    ;   Ps = [p(P1),_], cur_operand(P1, _, _, [-1|_]) % optional copy
	    ->  DiffRegs2 = DiffRegs3
	    ;   strip_p(Ps, Is),
		DiffRegs2 = [Is|DiffRegs3]
	    )
	),
	temp_domination(MAXI, MAXP, DomOps, []),
	(   foreach(OpsBB1,Ops),
	    foreach(DDGraph1,DDGraphs),
	    fromto(Across1,Across2,Across3,[]),
	    fromto(CondBefore1,CondBefore2,CondBefore3,[]),
	    fromto(AltAcross1,AltAcross2,AltAcross3,[]),
	    param(AVL1)
	do  (   foreach(I1,OpsBB1),
		fromto(CruxBB1,CruxBB2,CruxBB3,[]),
		param(SeenTail)
	    do  cur_operation(I1, TypeName, _, _, _),
		(   member(TypeName, [in,function])
		->  CruxBB2 = [I1|CruxBB3]
		;   TypeName = out, var(SeenTail) % if tail-recursive, don't include (out)
		->  CruxBB2 = [I1|CruxBB3]
		;   TypeName = tail
		->  SeenTail = true, CruxBB2 = CruxBB3
		;   CruxBB2 = CruxBB3
		)
	    ),
	    OpsBB1 = [I0|_],
	    last(OpsBB1, In),
	    gen_across_call(CruxBB1, AVL1, I0/In, DDGraph1,
			    CondBefore2, CondBefore3,
			    Across2, Across3),
	    alt_across_call(CruxBB1, DDGraph1,
			    AltAcross2, AltAcross3)
	),
	caller_saved_temps(AVL1, CSTemps),
	caller_saved_opnds(AVL1, CSOpnds),
	singletons(AVL1, Singletons),
	(   foreach([I3,t(T3),Cond3],Across1),
	    fromto(Across4,Across5,Across6,[]),
	    fromto(Nogoods4c,Nogoods5,Nogoods6,[]),
	    param(CSTemps,CSOpnds,Singletons)
	do  nogoods_or_across(I3, T3, Cond3, CSTemps, CSOpnds, Singletons,
			      Nogoods5, Nogoods6, Across5, Across6)
	),
	non_subsumed_nogoods(Nogoods1, Nogoods9),
	(   foreach(Nogood,Nogoods9),
	    foreach(Json,JNogoods1)
	do  conj_to_json(Nogood, Json)
	),
	difftemp_binary_nogoods(DiffTemps, DiffTempBinJNogoods1, []),
	difftemp_binary_nogoods_redundant(DiffTemps, DiffTempBinRNogoods1, []),
	sort(DiffTempBinJNogoods1, DiffTempBinJNogoods2),
	sort(DiffTempBinRNogoods1, DiffTempBinRNogoods2),
	ord_union(DiffTempBinJNogoods2, DiffTempBinRNogoods2, DiffTempBinJRNogoods),
	kernel_set(JNogoods1, DiffTempBinJRNogoods, JNogoods5),
	sort_by_length(JNogoods5, JNogoods5s),
	across_to_json(AVL1, Across4, Across7, JNogoods5s),
	findall(NG1, more_nogood(NG1), MoreNogoods1),
	kernel_set(MoreNogoods1, JNogoods5, MoreNogoods2), % TOTAL set of nogoods
	ord_subtract(MoreNogoods2, JNogoods5, MoreNogoods3), % Increment over JNogoods5
	sort_by_length(MoreNogoods2, MoreNogoods2s),
	findall([P,Q,CB2],
		(member([P,Q,CB1],CondBefore1), filter_condition(CB1,MoreNogoods2s,CB2), CB2\==[]),
		CondBefore4),
	sort(CondBefore4, CondBefore5), % canonicalize
	gen_before_precedences(Before, BeforePrecedences1),
	gen_before_precedences(CondBefore5, CondBeforePrecedences1),
	gen_fixed_precedences(AVL1, FixedPrecedences1, []),
	% gen_data_precedences(AVL1, MAXI, Opnd2Lat, LastUse, DataPrecedences1, []), % obsolete since slack variables were added
	gen_region_precedences(AVL1, RegionPrecedences1),
	% including CondBeforePrecedences gives LOTS of spurious cycles,
	% too many to be useful
	append([BeforePrecedences1,FixedPrecedences1/*,DataPrecedences1*/,RegionPrecedences1], Precedences1),
	sort(Precedences1, Precedences2), % there are duplicates
	findall([P,Q,N,BP2],
		(member([P,Q,N,BP1],Precedences2), filter_condition(BP1,JNogoods5s,BP2), BP2\==[]),
		Precedences3),
	findall([P,Q,N,BP2],
		(member([P,Q,N,BP1],CondBeforePrecedences1), filter_condition(BP1,JNogoods5s,BP2), BP2\==[]),
		CondBeforePrecedences2),
	Predecessors=[], Successors=[], % gen_predecessors_successors(AVL1, Predecessors, Successors),
	quasi_adjacent(AVL1, QuasiAdjacent),
	detect_cycles(Precedences3, CycleJNogoods),
	kernel_set(CycleJNogoods, JNogoods5, AllJNogoods1),
	ord_subtract(AllJNogoods1, DiffTempBinJRNogoods, AllJNogoods2),
	length(JNogoods5, NJNogoods5),
	length(AllJNogoods1, NAllJNogoods1),
	length(AllJNogoods2, NAllJNogoods2),
	print_message(warning, format('~d nogoods, ~d after cycle detection, ~d after removing difftemp nogoods',[NJNogoods5,NAllJNogoods1,NAllJNogoods2])),
	alt_across_to_json(AVL1, AltAcross1, AltAcross4),
	% compute valid a(_) combinations, per class
	(   foreach(Atomsi,Atoms),
	    fromto(0,MP1,MP3,MAXR)
	do  last(Atomsi, MP2),
	    MP3 is max(MP1,MP2)
	),
	statistics(walltime, [Used2,_]),
	Cutoff2 is 3*(Quota-Used2)//4,
	on_exception(Excp,	% catchall, e.g. for memory resource error
		     time_out(gen_active_tables(AVL1, MAXI, MAXP, MAXR, MAXT,
					      AllJNogoods2, DiffTemps, DiffRegs1,
					      Dominates1, DomOps, LastUse, QuasiAdjacent),
			      Cutoff2, _),
		     print_message(error, Excp)),
	findall([CopySet,Table], retract(copy_table(CopySet,Table)), ActiveTables1),
	filter_active_tables(ActiveTables1, ActiveTables2, Dominates1, Dominates2),
	activator_active_tables(AVL1, ActiveTables2, ActiveTables3),
	optional_min_active_tables(AVL1, ActiveTables3, OptionalMin),
	findall([CopySet,PSet,Table], retract(tmp_table(CopySet,PSet,Table)), TmpTables),
	% end valid Ai combinations
	copyrel_star(AVL1, CopyRelStar),
	callee_saved_spill(AVL1, CopyRelStar, CalleeSavedSpill),
	presolve_dur(AVL1, AVL1b),
	normalize_precedences(Precedences3, Precedences4),
	normalize_precedences(CondBeforePrecedences2, CondBeforePrecedences3),
	instr_cond(AVL1, InstrCond),
	register_symmetry_breaking(VPChains, AVL1),
	avl_store(before, AVL1b, Before, AVL2),
	avl_store(before2, AVL2, CondBefore5, AVL3),
	avl_store(nogoods, AVL3, AllJNogoods2, AVL4),
	avl_store(nogoods2, AVL4, MoreNogoods3, AVL5), % expect: dominated by across
	avl_store(across, AVL5, Across7, AVL6),
	avl_store(set_across, AVL6, AltAcross4, AVL7),
	avl_store(difftemps, AVL7, DiffTemps, AVL8),
	avl_store(domops, AVL8, DomOps, AVL9),
	avl_store(optional_min, AVL9, OptionalMin, AVL10),
	avl_store(dominates, AVL10, Dominates2, AVL11),
	avl_store(last_use, AVL11, LastUse, AVL12),
	avl_store(precedences, AVL12, Precedences4, AVL13),
	avl_store(precedences2, AVL13, CondBeforePrecedences3, AVL14),
	avl_store(active_tables, AVL14, ActiveTables3, AVL15),
	avl_store(tmp_tables, AVL15, TmpTables, AVL16),
	avl_store(diffregs, AVL16, DiffRegs1, AVL17),
	avl_store(unsafe_temp, AVL17, UnsafeTemp, AVL18),
	avl_store(calleesaved_spill, AVL18, CalleeSavedSpill, AVL19),
	avl_store(predecessors, AVL19, Predecessors, AVL20),
	avl_store(successors, AVL20, Successors, AVL21),
	avl_store(instr_cond, AVL21, InstrCond, AVL22),
	avl_store(quasi_adjacent, AVL22, QuasiAdjacent, AVL23),
	avl_store(value_precede_chains, AVL23, VPChains, AVL24).

register_symmetry_breaking(TsRss2, AVL) :-
	avl_fetch(range, AVL, Range),
	avl_fetch(infinite, AVL, Infinite),
	findall(C-W, p_class_width(_, C, W, AVL), CWs1),
	sort(CWs1, CWs2),
	(nth0(Infi, Infinite, true) -> true),
	nth0(Infi, Range, [Limit|_]),
	reg_sym_eq_classes(CWs2, Limit, Cls2Regs, AVL),
	(   foreach(Cs-Rs,Cls2Regs),
	    fromto(TsRs1,TsRs2,TsRs3,[]),
	    param(AVL)
	do  length(Rs, Size),
	    (   Size =:= 1 -> TsRs2 = TsRs3
	    ;   findall(NW1-T1, t_no_preass_can_use_class(T1, NW1, Cs, Size, AVL), NWTs1),
		sort(NWTs1, NWTs2),
		(   foreach(NW2-T2,NWTs2),
		    foreach(T2,Ts),
		    fromto(8,Wmin1,Wmin2,Wmin3)
		do  Wmin2 is min(Wmin1,-NW2)
		),
		(   Size =< Wmin3 -> TsRs2 = TsRs3
		;   TsRs2 = [Ts-Rs|TsRs3]
		)
	    )
	),
	sort(TsRs1, TsRs4),	% ensure sorted Rss
	keyclumped(TsRs4, TsRss1),
	(   foreach(Ts1-Rss1,TsRss1),
	    foreach([Ts1,Rss1],TsRss2)
	do  true
	).
	% ensure no linking alignment constraints -- don't know how to make it accurate
	% findall(AlignedTs, aligned_ts(AlignedTs, AVL), AlignedTss),
	% (   foreach(Ts2-_,TsRss1),
	%     param(AlignedTss)
	% do  (   foreach(AlignedTs2,AlignedTss),
	% 	param(Ts2)
	%     do  (   ord_disjoint(AlignedTs2, Ts2) -> true
	% 	;   ord_subset(AlignedTs2, Ts2) -> true
	% 	;   print_message(warning, alignment_vs_symmetry(AlignedTs2,Ts2))
	% 	)
	%     )
	% ).

aligned_ts(AlignedTs, AVL) :-
	avl_fetch(aligned, AVL, Aligned),
	member([P1,_,P2,_], Aligned),
	cur_operand(P1, _, _, Ts1),
	cur_operand(P2, _, _, Ts2),
	ord_union(Ts1, Ts2, Ts12),
	ord_del_element(Ts12, -1, AlignedTs).

reg_sym_eq_classes(CWs, Limit, Cls2Regs, AVL) :-
	avl_fetch(preassign, AVL, Preassign),
	avl_fetch(atoms, AVL, Atoms),
	avl_fetch(calleesaved, AVL, CalleeSaved),
	(   foreach(C-W,CWs),
	    fromto(KL1,KL2,KL7,KL8),
	    param(Atoms,Limit)
	do  nth0(C, Atoms, Set),
	    last(Set, Last),
	    (   Last >= Limit -> KL2 = KL7
	    ;   (   foreach(S,Set),
		    param(C,W),
		    fromto(KL2,KL3,KL6,KL7)
		do  (   for(S1,S,S+W-1),
			fromto(KL3,KL4,KL5,KL6),
			param(C)
		    do  KL4 = [S1-C|KL5]
		    )
		)
	    )
	),
	(   foreach([P1,R1],Preassign),
	    fromto(KL8,KL9,KL12,[]),
	    param(CalleeSaved)
	do  cur_operand(P1, I1, _, [T1|_]),
	    cur_operation(I1, Ty, _, _, _),
	    cur_temp(T1, W1, Ps, _),
	    (   Ps = [P1,P2],	% one use
		cur_operand(P2, I2, 1, _),
		cur_operation(I2, kill, _, _, _)
	    ->  CC = o(I1)
	    ;   member(Ty, [in,out]),
		ord_member(R1, CalleeSaved)
	    ->  CC = o(I1)
	    ;   CC = p(P1)
	    ),
	    (   for(R2,R1,R1+W1-1),
		fromto(KL9,KL10,KL11,KL12),
		param(CC)
	    do  KL10 = [R2-CC|KL11]
	    )
	),
	sort(KL1, KL13),	% ensure sorted Cls
	keyclumped(KL13, Reg2Cls),
	(   foreach(K-V,Reg2Cls),
	    foreach(V-K,KL14)
	do  true
	),
	keysort(KL14, KL15),
	keyclumped(KL15, Cls2Regs).

p_class_width(P, C, W, AVL) :-
	avl_fetch(class, AVL, Class),
	avl_fetch(operands, AVL, Operands),
	cur_operand(P, Oper, _, Ts),
	last(Ts, T),
	cur_temp(T, W, _, _),
	nth0(Oper, Operands, [Base|_]),
	Ix is P-Base,
	nth0(Oper, Class, Class1),
	member(Class2, Class1),
	nth0(Ix, Class2, C).

t_no_preass_can_use_class(T, NW, Cs, Wlimit, AVL) :-
	avl_fetch(class, AVL, Class),
	avl_fetch(operands, AVL, Operands),
	avl_fetch(preassign, AVL, Preassign),
	avl_fetch(packed, AVL, Packed),
	findall(P1, cur_redefiner(_,_,P1), Ps1),
	sort(Ps1, Ps2),
	(   foreach([P3,_],Preassign),
	    foreach(P3,Ps3)
	do  true
	),
	ord_union(Ps2, Ps3, ForbiddenPs),
	(   foreach([_,P4],Packed),
	    foreach(Ts4,Tss4)
	do  cur_operand(P4, _, _, Ts4)
	),
	ord_union(Tss4, ForbiddenTs),
	cur_operand(P1, Oper, 0, Ts), % def only
	cur_operation(Oper, Type, _, _, _),
	memberchk(Type, [copy,linear]),
	ord_nonmember(P1, ForbiddenPs),
	last(Ts, T),
	ord_nonmember(T, ForbiddenTs),
	cur_temp(T, W, _, _),
	W < Wlimit,
	NW is -W,
	nth0(Oper, Operands, [Base|_]),
	Ix is P1-Base,
	nth0(Oper, Class, ClassO),
	transpose(ClassO, ClassOT),
	nth0(Ix, ClassOT, ClassOTP),
	sort(ClassOTP, ClassOTPs),
	ord_intersect(ClassOTPs, Cs).

redefined_operand_nogood(Opnd2Lat, Nogood) :-
	cur_redefiner(O1, P1, Q1),
	cur_operand(P1, _, _, Temps1),
	avl_fetch(Q1, Opnd2Lat, ILats1),
	cur_redefiner(O2, P2, Q2),
	O1 < O2,
	cur_operand(P2, _, _, Temps2),
	ord_intersect(Temps1, Temps2),
	avl_fetch(Q2, Opnd2Lat, ILats2),
	(   foreach(I1-L1,ILats1),
	    fromto(Dis1,Dis2,Dis5,[]),
	    param(ILats2,P1,P2,O1,O2)
	do  (   foreach(I2-L2,ILats2),
		fromto(Dis2,Dis3,Dis4,Dis5),
		param(P1,P2,O1,O2,I1,L1)
	    do  (   L1>0, L2>0
		->  Dis3 = [[[/*JTAG*/0,P1,P2],[/*JTAG*/3,O1,I1],[/*JTAG*/3,O2,I2]]|Dis4]
		;   Dis3 = Dis4
		)
	    )
	),
	normalize_condition(Dis1, Dis6),
	member(Raw, Dis6),
	(   Raw = [[/*JTAG*/0,P3,P4]]
	->  assertz(cur_difftemp([p(P3),p(P4)])), fail
	;   Nogood = Raw
	).

quasi_adjacent(AVL, QuasiAdjacent) :-
	avl_fetch(adjacent, AVL, Adjacent),
	(   foreach([Out1,In1],Adjacent),
	    foreach(Out1-In1,KL1)
	do  true
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(Out2-Ins,KL3),
	    fromto(Imp1,Imp5,Imp6,Imp7)
	do  (   Ins = [In2],
		cur_operand(In2, _, _, [-1|_])
	    ->  Imp5 = [In2-Out2|Imp6] % connected out with single successor implies connected in
	    ;   Imp5 = Imp6
	    )
	),
	% find (in) just passing thru to (out): (in) implies (out)
	(   foreach(Out3-_,KL3),
	    fromto(Imp7,Imp8,Imp9,Imp10)
	do  (   cur_operand(Out3, _, _, [-1,T1|_]), % optional out operand
		cur_temp(T1, _, [In3|Ps], _),
		cur_operand(In3, Def3, _, _),
		cur_operation(Def3, in, _, _, _),
		(   foreach(P,Ps)
		do  cur_operand(P, Oper, _, _),
		    cur_operation(Oper, Ty, _, _, _),
		    memberchk(Ty, [copy,out])
		)
	    ->  Imp8 = [Out3-In3|Imp9]
	    ;   Imp8 = Imp9
	    )
	),
	% now find (out) implies (in) if it inherits incoming value
	(   foreach(Out4-_,KL3),
	    fromto(Imp10,Imp11,Imp12,[])
	do  cur_operand(Out4, _, _, Ts4),
	    (   foreach(T4,Ts4),
		fromto(Ex1,Ex2,Ex3,[]),
		param(In4)
	    do  (   inherits_opt_in(T4, In4) -> Ex2 = Ex3
		;   Ex2 = [T4|Ex3]
		)
	    ),
	    (   Ex1 = [], nonvar(In4) -> Imp11 = [In4-Out4|Imp12]
	    ;   Imp11 = Imp12
	    )
	),
	sort(Imp1, Imp13),	% can contain duplicates
	ord_subtract(Imp13, KL2, Imp14),
	(   foreach(Then-If,Imp14),
	    foreach([Then,If],QuasiAdjacent)
	do  true
	).

inherits_opt_in(-1, _) :- !.
inherits_opt_in(T, Def) :-
	cur_temp(T, _, _, Oper),
	cur_operation(Oper, in, _, _, Defs),
	member(Def, Defs),
	cur_operand(Def, _, _, [-1,T]), !.
inherits_opt_in(T, Def) :-
	cur_temp(T, _, _, O),
	cur_operation(O, copy, _, [U|_], _),
	cur_operand(U, _, _, [-1,T2|_]),
	inherits_opt_in(T2, Def).

bool_to_int(true, 1).
bool_to_int(false, 0).

% dominance constraint for "nv" instructions on Hexagon
% e.g. for
%    o11: [p29{-, t19}] <- {-, TFR, STW, STW_nv} [p28{-, t18}] (reads: [control])
% i[11] = STW_nv -> ls[t[28]] = c[11]
instr_cond(AVL, Cond1) :-
	avl_fetch(lat, AVL, Lat),
	avl_fetch(instructions, AVL, Instructions),
	avl_fetch(operands, AVL, Operands),
	avl_fetch(class, AVL, Class),
	(   foreach(Lato,Lat),
	    foreach(Iso,Instructions),
	    foreach(Opso,Operands),
	    foreach(Classo,Class),
	    fromto(Cond1,Cond2,Cond3,[]),
	    count(O,0,_)
	do  (   Iso = [_,_|_],
		nth0(I, Classo, Classeq),
		nth0(J, Classo, Classeq),
		I=\=J,
		nth0(I, Lato, Latoi),
		nth0(J, Lato, Latoj),
		nth0(K, Latoi, 0, Lateq),
		nth0(K, Latoj, -1, Lateq)
	    ->  Cond2 = [[O,Isoj,Opsok]|Cond3],
		nth0(J, Iso, Isoj),
		nth0(K, Opso, Opsok)
	    ;   Cond2 = Cond3
	    )
	).

% Precs1 are sorted already
normalize_precedences(Precs1, Precs2) :-
	(   foreach([Src,Dest,Dis,Disj], Precs1),
	    foreach([Src,Dest,Dis]-Disj, KL1)
	do  true
	),
	keyclumped(KL1, KL2),
	(   foreach([Src2,Dest2,Dis2]-Cl, KL2),
	    foreach([Src2,Dest2,Dis2,Disj2], Precs2)
	do  append(Cl, Disj1),
	    normalize_condition(Disj1, Disj2)
	).

% remove entailed "i(O)=3" conditions
% convert (FOO & i(O)=3) | ... | (FOO & i(O)=6)
% to      (FOO & i(O)!=4 & i(O)!=5)
normalize_condition([[]], [[]]) :- !. % accelerator
normalize_condition(Disj1, Disj2) :-
	member([[/*JTAG*/2,A]], Disj1), !,
	Disj2 = [[[/*JTAG*/2,A]]].
normalize_condition(Disj1, DisjN) :-
	(   foreach(Conj1,Disj1),
	    foreach(Conj2,Disj2)
	do  (   foreach(Lit,Conj1),
		fromto(Conj2,Conj3,Conj4,[])
	    do  (   Lit = [/*JTAG*/3,O,I],
		    cur_operation(O, _, [I], _, _)
		->  Conj3 = Conj4
		;   Conj3 = [Lit|Conj4]
		)
	    )
	),
	kernel_set(Disj2, [], Disj3),
	(   merge_i_cases(Disj3, Conj) -> DisjN = [Conj]
	;   DisjN = Disj3
	).

% convert (FOO & i(O)=3) | ... | (FOO & i(O)=6)
% to      (FOO & i(O)!=4 & i(O)!=5)
% suppress i(O)!=0 if O is for sure using or defining something that occurs in a literal [1,P,T]
merge_i_cases([Conj], Conj) :- !.
merge_i_cases(Disj, [Lit|Rest]) :-
	(   foreach([K|V],Disj),
	    foreach(K-V,KL1)
	do  true
	),
	keyclumped(KL1, [Lit-Cl]), !,
	merge_i_cases(Cl, Rest).
merge_i_cases(Disj, Conj) :-
	(   foreach([K|V],Disj),
	    foreach(K-V,KL1)
	do  true
	),
	keyclumped(KL1, KL2),
	(   foreach([3,O,I]-Cl,KL2),
	    foreach(I,Is),
	    param(Cl,O)
	do  true
	),
	cur_operation(O, _, Iall, _, _),
	ord_subtract(Iall, [0|Is], Inot),
	(   foreach(J,Inot),
	    foreach([/*JTAG*/7,O,J],Prefix),
	    param(O)
	do  true
	),
	merge_i_cases(Cl, Rest),
	append(Prefix, Rest, Conj).

% Nogoods is a keysorted list of Len-Nogood
filter_condition(Disj1, Nogoods, Disj2) :-
	(   foreach(Conj,Disj1),
	    fromto(Disj2,Disj3,Disj4,[]),
	    param(Nogoods)
	do  (   Conj\==[],
		length(Conj, CL),
		member_of_max_length(Nogood, NL, Nogoods, CL),
		ord_subsumes(Nogood, NL, Conj, CL)
	    ->  Disj3 = Disj4
	    ;   Disj3 = [Conj|Disj4]
	    )
	).

member_of_max_length(Y, YL, [XL-X|Xs], CL) :-
	XL =< CL,
	(Y=X, YL=XL ; member_of_max_length(Y, YL, Xs, CL)).

alt_across_to_json(AVL1, AltAcross, AltAcrossJ) :-
	avl_fetch(callersaved, AVL1, CallerSaved),
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(temps, AVL1, Temps),
	list_to_fdset(CallerSaved, CSSet),
	copy_related_map(Temps, Principal2Class),
	keyclumped(AltAcross, KL),
	(   foreach(I-Clump,KL),
	    foreach([I,Extra,CopySets1],AltAcrossJ),
	    param(Principal2Class, Preassign, CSSet)
	do  collect_at_call(I, Preassign, CSSet, PsAtCall, TsAtCall),
	    across_extra_regs(PsAtCall, Preassign, Extra),
	    (   foreach(Key,Clump),
		fromto(CopySets1,CopySets2,CopySets3,[]),
		param(Principal2Class, TsAtCall)
	    do  (   Key = U-D
		->  avl_fetch(U, Principal2Class, CopySet1),
		    avl_fetch(D, Principal2Class, CopySet2),
		    ord_union(CopySet1, CopySet2, CopySet3)
		;   avl_fetch(Key, Principal2Class, CopySet3)
		),
		% exclude temps used preassigned non-caller-saved in the call
		(   ord_disjoint(CopySet3, TsAtCall)
		->  CopySets2 = [CopySet3|CopySets3]
		;   CopySets2 = CopySets3
		)
	    )
	).

copy_related_map(Temps0, AVL) :-
	sort(Temps0, Temps),
	(   foreach(Set,Temps),
	    fromto(KL1,KL2,KL3,[])
	do  (   Set = [Tc|_],
		Tc \== -1
	    ->  KL2 = [Tc-Set|KL3]
	    ;   KL2 = KL3
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	(   foreach(Key-Sets,KL5),
	    foreach(Key-Union,KL6)
	do  ord_union(Sets, Union)
	),
	ord_list_to_avl(KL6, AVL).

tagged_copy_related_map(Temps0, AVL) :-
	sort(Temps0, Temps),
	(   foreach(Set,Temps),
	    fromto(KL1,KL2,KL3,[])
	do  (   Set = [Tc|_],
		Tc \== -1
	    ->  KL2 = [Tc-Set|KL3]
	    ;   KL2 = KL3
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	(   foreach(_-Sets,KL5),
	    fromto(KL6,KL7,KL9,[])
	do  ord_union(Sets, Class),
	    (   foreach(I,Class),
		foreach(t(I),TClass)
	    do  true
	    ),
	    (   foreach(C,TClass),
		fromto(KL7,[C-TClass|KL8],KL8,KL9),
		param(TClass)
	    do  true
	    )
	),
	keysort(KL6, KL10),
	ord_list_to_avl(KL10, AVL).

% some across items may in fact be nogoods discovered on the fly
% Nogoods is a keysorted list of Len-Nogood
across_to_json(AVL1, Across, Json, Nogoods) :-
	avl_fetch(callersaved, AVL1, CallerSaved),
	avl_fetch(preassign, AVL1, Preassign),
	list_to_fdset(CallerSaved, CSSet),
	(   foreach([K|TDisj],Across),
	    foreach(K-TDisj,KL1)
	do  true
	),
	sort(KL1, KL2),		% sort on <I,T>
	keyclumped(KL2, KL3),
	(   foreach(I-TDisjs,KL3),
	    foreach([I,Extra,Json1],Json),
	    param(Nogoods, Preassign, CSSet)
	do  collect_at_call(I, Preassign, CSSet, PsAtCall, TsAtCall),
	    across_extra_regs(PsAtCall, Preassign, Extra),
	    (   foreach([t(T),Disj],TDisjs),
		fromto(Json1,Json2,Json3,[]),
		param(TsAtCall,Nogoods)
	    do  (   ord_member(T, TsAtCall) % exclude temps
				% used preassigned non-caller-saved in the call
		->  Json2 = Json3
		;   Json2 = [[T,DisjJ2]|Json3],
		    disj_to_json(Disj, DisjJ1),
		    filter_condition(DisjJ1, Nogoods, DisjJ2)
		)
	    )
	).

%% Extra1 = regs preassigned non-caller-saved in call
across_extra_regs(PsAtCall, Preassign, Extra1) :-
	    (   foreach(P,PsAtCall),
		fromto(Extra1,Extra2,Extra4,[]),
		param(Preassign)
	    do  cur_operand(P, _, _, [T|_]),
		cur_temp(T, W, _, _),
		memberchk([P,R0], Preassign),
		(   for(R,R0,R0+W-1),
		    fromto(Extra2,[R|Extra3],Extra3,Extra4)
		do  true
		)
	    ).


% collect opnds and temps used preassigned non-caller-saved in insn ICall
collect_at_call(ICall, Preassign, CSSet, AtPs1, AtTs3) :-
	cur_operation(ICall, _, _, Us, _),
	(   foreach(P,Us),
	    fromto(AtPs1,AtPs2,AtPs3,[]),
	    fromto([],AtTs1,AtTs2,AtTs3),
	    param(Preassign, CSSet)
	do  cur_operand(P, _, 1, Ts),
	    (   maybe_caller_saved(P, Preassign, CSSet, no(_))
	    ->  AtPs2 = [P|AtPs3],
		ord_union(AtTs1, Ts, AtTs2)
	    ;   AtPs2 = AtPs3,
		AtTs1 = AtTs2
	    )
	).

nogoods_or_across(I, T, Cond1, CSTemps, CSOpnds, Singletons,
		  Nogoods0, Nogoods, [[I,t(T),Kern]|Across], Across) :-
	(   foreach(Conj1,Cond1),
	    fromto(Cond2,Cond3,Cond4,[]),
	    fromto(Nogoods0,Nogoods1,Nogoods2,Nogoods),
	    param(T,CSTemps,CSOpnds,Singletons)
	do  ord_subtract(Conj1, Singletons, Conj2),
	    (   ord_member(T, CSTemps)
	    ->  Cond3 = Cond4, Nogoods1 = [Conj2|Nogoods2]
	    ;   member(p(P)=t(T), Conj2),
		ord_member(P, CSOpnds)
	    ->  Cond3 = Cond4, Nogoods1 = [Conj2|Nogoods2]
	    ;   Cond3 = [Conj2|Cond4], Nogoods1 = Nogoods2
	    )
	),
	kernel_set(Cond2, [], Kern).


% [MC] "before": order live ranges for intra-block congruences
% before(P,Q): le[t[P]] <= ls[t[Q]]
gen_before(AVL1, LastUse, UnsafeTemp, Opnd2Lat, Before1) :-
	avl_fetch(tmp, AVL1, Tmp),
	avl_fetch(maxc, AVL1, MaxC), % maximum issue cycle of basic block
	avl_fetch(ops, AVL1, Ops),
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(operands, AVL1, Operands),
	avl_fetch(strictly_congr, AVL1, Congr),
	(   foreach(Ops1,Ops),
	    foreach(Tmp1,Tmp),
	    foreach(M,MaxC),
	    foreach(bb(Isucc..Imax,OPNDsucc..OPNDmax,Tsucc..Tmax,M),BBs),
	    fromto(bb(-1,-1,-1),bb(Iprev,OPNDprev,Tprev),bb(Imax,OPNDmax,Tmax),_),
	    param(Operands)
	do  Isucc is Iprev+1,
	    OPNDsucc is OPNDprev+1,
	    Tsucc is Tprev+1,
	    last(Ops1, Imax),
	    (   Tmp1 = []	% no temps, no opnds?
	    ->  Tmax is Tsucc-1,
		OPNDmax is OPNDsucc-1
	    ;   last(Tmp1, Tmax),
		prefix_length(Operands, OpPartA, Isucc),
		append(OpPartA, OpPartBC, Operands),
		NI is Imax-Iprev,
		prefix_length(OpPartBC, OpPartB, NI),
		append(OpPartB, OpFlatB),
		last(OpFlatB, OPNDmax)
	    )
	),
	(   foreach(Congri,Congr),
	    fromto(Before1,Before2,Before5,Before6),
	    param(BBs,Operands)
	do  findall(P-Q, ord_pairof(Congri,P,Q), Pairs),
	    (   foreach(P5-Q5,Pairs),
		fromto(Before2,Before3,Before4,Before5),
		param(BBs,Operands)
	    do	(   member(bb(Ia..Ib,Ta..Tb,_,_),BBs),
		    P5 >= Ta,
		    P5 =< Tb,
		    Q5 >= Ta,
		    Q5 =< Tb,
		    cur_operand(P5, Ia, _, TempsP5),  % cur_operand(P5, Pi, Pu, TempsP5),
		    cur_operand(Q5, Ib, _, TempsQ5),  % cur_operand(Q5, Qi, Qu, TempsQ5),
		    % (Pi<Qi -> Pi=Ia, Qi=Ib ; Pu>Qu), % (in)/(out), or (use,def) of same insn
		    ord_disjoint(TempsP5, TempsQ5)
		% operands P5 < Q5 are (in)/(out)  congruent, in the same basic block
		% we are not merely passing the value from P5 to Q5
		->  co_bb_before(P5, Q5, Operands, Before3, Before4)
		;   Before3 = Before4
		)
	    )
	),
	(   fromto(Preassign,Preassign1,Preassign2,[]),
	    fromto(KL1,KL2,KL3,[]),
	    count(O0,0,_),
	    param(BBs)
	do  bb_of_operand(BBs, O0, 0, BBi),
	    (   Preassign1 = [[O0,R0]|Preassign2]
	    ->  KL2 = [BBi-Tagged|KL3],
		cur_operand(O0, _, U, Ts),
		delete_null(Ts, [T0|_]),
		cur_temp(T0, W0, _, _),
		tag_opnd(U, O0, R0, W0, Tagged)
	    ;   Preassign1 = Preassign2, KL2 = KL3
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	% KL5 is a list: (BasicBlockNo,RegNo)-ListOfOperand
	% giving the list of co-preassigned operands per basic block and reg
	(   foreach(_-Clump,KL5),
	    fromto(Before6,Before7,Before8,Before9)
	do  emit_before_from_clump(Clump, Precs, []),
	    transitive_reduction(Precs, Reduced),
	    append_reduced(Reduced, Before7, Before8)
	),
	findall([P1,Q1,[[]]], lastuse_to_before(P1, Q1, LastUse, Opnd2Lat), Before9, Before10),
	findall([P1,Q1,[[]]], def_use_to_before(P1, Q1, UnsafeTemp), Before10, Before11),
	findall([P1,P2,[[p(P3)=t(T1)]]], high_combine_before(P1, P2, P3, T1), Before11, []).

%% Pattern:
%% [p4{t1}] <- (high) [p1{...}]
%% [......] <- (combine) [p2{...}, p3{t1,...}]
%%
%% means p1 and p2 are effectively congruent and implies:
%%
%%      p1 before p2 if p3=t1
high_combine_before(P1, P2, P3, T1) :-
	cur_operation(_, combine, _, [P2,P3], _),
	cur_operand(P3, _, 1, [T1|_]),
	cur_temp(T1, _, _, HighO),
	cur_operation(HighO, high, _, [P1], _).

%% 20150427: previous version did not allow P to use an unsafe temporary
% lastuse_to_before(P, Q, LastUse, UnsafeTemp) :-
%	member(P, LastUse),
%	cur_operand(P, Insn, 1, [Tp|_]),
%	ord_nonmember(Tp, UnsafeTemp),
%	cur_operand(Q, Insn, 0, _).
%% this version instead does not allow P to have a -1 latency
lastuse_to_before(P, Q, LastUse, Opnd2Lat) :-
	member(P, LastUse),
	cur_operand(P, Insn, 1, _),
	cur_operation(Insn, _, _, _, Qs),
	member(Q, Qs),
	avl_fetch(P, Opnd2Lat, OpLats),
	nonmember(-(_,-1), OpLats).

def_use_to_before(P, R, UnsafeTemp) :-
	cur_operand(P, _, 0, [Tp]),
	ord_nonmember(Tp, UnsafeTemp),
	findall(Q-I, cur_operand(Q, I, 1, [Tp|_]), Qs),
	Qs = [Q-I],
	cur_operand(R, I, 0, _).

append_reduced([]) --> [].
append_reduced([[P,Q]|Reduced]) --> [[P,Q,[[]]]],
	append_reduced(Reduced).

co_bb_before(P1, Q1, Operands) -->
	{   foreach(Opnds,Operands),
	    fromto(LH1,LH2,LH3,[]),
	    count(I,0,_)
	do  cur_operation(I, TypeName, _, _, _),
	    (   TypeName = low
	    ->  LH2 = [Opnds|LH3]
	    ;   TypeName = high
	    ->  LH2 = [Opnds|LH3]
	    ;   LH2 = LH3
	    )
	},
	{findall(Pd-Cd, lh_descendant(P1,[],Pd,Cd,LH1), Pds)},
	{findall(Qd-Cd, lh_descendant(Q1,[],Qd,Cd,LH1), Qds)},
	(   foreach(P2-PC2,Pds),
	    param(Qds)
	do  (   foreach(Q2-QC2,Qds),
		param(P2,PC2)
	    do  {append(PC2, QC2, PQC)},
		[[P2,Q2,[PQC]]]
	    )
	).

lh_descendant(P , Conj , P, Conj, _).
lh_descendant(P0, Conj0, P, Conj, LH) :-
	cur_operand(P0, _, _, Pt),
	member([Use,Def], LH),
	cur_operand(Use, _, _, Ut),
	ord_intersect(Pt, Ut),
	(Pt==Ut -> Conj = Conj0 ; Conj = [p(Use)=p(P0)|Conj0]),
	lh_descendant(Def, Conj, P, Conj, LH).

before_vs_nogoods(Before0, Before1) --> %nogoods
	{sort(Before0, Before)},
	(   foreach([P,Q,[C]],Before),
	    fromto(Before1,Before2,Before3,[])
	do  (   {C=[]}
	    ->  {Before2 = [[P,Q,[C]]|Before3]}
	    ;   {Before2 = Before3},
		[[overlap(p(P),p(Q))|C]]
	    )
	).

% Clump is a list of d(P,R,W) or u(P,R,W) for a given basic block
% Compute set of precedences:
% [P,Q] for every [... ?(P,R1,W1) ... d(Q,R2,W2) ...]
% [P,Q] for every [... ?(P,R1,W1) ... u(Q,R2,W2) ...] where P and Q have disjoint temps
% where <R1,W1> and <R2,W2> overlap
% Sorted by construction.
emit_before_from_clump(Clump) -->
	(   fromto(Clump,[P|Qs],Qs,[_])
	do  {P =.. [_,P1,R1,W1]},
	    {cur_operand(P1, _, _, TempsP)},
	    (   foreach(Q,Qs),
		param(P1,R1,W1,TempsP)
	    do  {Q =.. [DU,Q1,R2,W2]},
		(   {R1+W1 > R2, R2+W2 > R1, DU = d}
		->  [[P1,Q1]]
		;   {R1+W1 > R2, R2+W2 > R1},
		    {cur_operand(Q1, _, _, TempsQ)},
		    {ord_disjoint(TempsP, TempsQ)}
		->  [[P1,Q1]]
		;   []
		)
	    )
	).

transitive_reduction(Pairs, Reduced) :-
	(   foreach([A,B],Pairs),
	    foreach(A-B,Edges)
	do  true
	),
	reduce_edges(Edges, EdgesH),
	(   foreach(C-D,EdgesH),
	    foreach([C,D],Reduced)
	do  true
	).

tag_opnd(0, X, R, W, d(X,R,W)).
tag_opnd(1, X, R, W, u(X,R,W)).

ord_pairof([X|Ys], X, Y) :-
	member(Y, Ys).
ord_pairof([_|Ys], X, Y) :-
	ord_pairof(Ys, X, Y).

% suppress e.g. ARM STORE insn if STORE_T can do the job
dominating_insns(AVL, Nogoods1, Nogoods6) :-
	avl_fetch(lat, AVL, Lat),
	avl_fetch(instructions, AVL, Instructions),
	avl_fetch(class, AVL, Class),
	avl_fetch(con, AVL, Con),
	avl_fetch(dur, AVL, Dur),
	avl_fetch(atoms, AVL, Atoms),
	(   foreach(Lato,Lat),
	    foreach(Iso,Instructions),
	    foreach(Classo,Class),
	    fromto(KL1,KL2,KL7,[])
	do  length(Iso,N),
	    (   N<2 -> KL2 = KL7
	    ;   (   for(I,0,N-2),
		    fromto(KL2,KL3,KL6,KL7),
		    param(Lato,Iso,Classo,N)
		do  nth0(I, Lato, Latoi),
		    nth0(I, Iso, Isoi),
		    nth0(I, Classo, Classoi),
		    (   Isoi = 0 -> KL3 = KL6
		    ;   (   for(J,I+1,N-1),
			    fromto(KL3,KL4,KL5,KL6),
			    param(Lato,Iso,Classo,Latoi,Isoi,Classoi)
			do  nth0(J, Lato, Latoj),
			    nth0(J, Iso, Isoj),
			    nth0(J, Classo, Classoj),
			    (   Latoi \== Latoj -> KL4 = KL5
			    ;   KL4 = [t(Isoi,Classoi,Isoj,Classoj)|KL5]
			    )
			)
		    )
		)
	    )
	),
	sort(KL1, Tuples),
	(   foreach(t(I1,C1,I2,C2),Tuples),
	    fromto(Rules1,Rules2,Rules3,[]),
	    param(Con,Dur,Atoms)
	do  nth0(I1, Con, Con1),
	    nth0(I2, Con, Con2),
	    nth0(I1, Dur, Dur1),
	    nth0(I2, Dur, Dur2),
	    (   all_lesseq(Con1, Con2),
		all_lesseq(Dur1, Dur2),
		all_subseteq(C1, C2, Atoms)
	    ->  Rules2 = [rule(I1,I2,C1)|Rules3]
	    ;   all_lesseq(Con2, Con1),
		all_lesseq(Dur2, Dur1),
		all_subseteq(C2, C1, Atoms)
	    ->  Rules2 = [rule(I2,I1,C2)|Rules3]
	    ;   Rules2 = Rules3
	    )
	),
	length(Instructions, MAXO),
	(   for(O,0,MAXO-1),
	    fromto(Nogoods1,Nogoods2,Nogoods5,Nogoods6),
	    param(Rules1)
	do  cur_operation(O,_,Insns,Uses,Defs),
	    append(Uses, Defs, Opnds),
	    (   foreach(rule(J1,J2,Conds),Rules1),
		fromto(Nogoods2,Nogoods3,Nogoods4,Nogoods5),
		param(O,Opnds,Insns)
	    do  (   member(J1,Insns),
		    member(J2,Insns)
		->  (   foreach(Cond,Conds),
			foreach(Opnd,Opnds),
			foreach(r(t(Opnd)) in Cond,Rest)
		    do  true
		    ),
		    Nogoods3 = [[i(O)=J2|Rest]|Nogoods4]
		;   Nogoods3 = Nogoods4
		)
	    )
	).




all_lesseq(Xs, Ys) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys)
	do  X =< Y
	).

all_subseteq(Xs, Ys, Atoms) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Atoms)
	do  nth0(X, Atoms, XSet),
	    nth0(Y, Atoms, YSet),
	    ord_subset(XSet, YSet)
	).

gen_dominates(MAXI) -->
	{   for(I,0,MAXI),
	    fromto(KL1,KL2,KL3,[])
	do  (   cur_operation(I, copy, O, [Src|_], [Dest|_]),
		O = [0|_]
	    ->  KL2 = [Primary-iodu(I,O,DestTemps,SrcTemps)|KL3],
		SrcTemps = [-1,Primary|_],
		cur_operand(Src, _, _, SrcTemps),
		cur_operand(Dest, _, _, DestTemps)
	    ;   KL2 = KL3
	    )
	},
	{keysort(KL1, KL4)},
	{keyclumped(KL4, KL5)},
	(   foreach(_-KL6,KL5)
	do  (   fromto(KL6,[IODU|KL7],KL7,[_])
	    do  gen_dominates(KL7, IODU)
	    )
	).

gen_dominates([], _) --> [].
gen_dominates([IODU2|R0], IODU1) --> [[I1,I2,Odiff,Tdiff]],
	{IODU1 = iodu(I1,O1,[-1,T1],SrcTemps1)},
	{IODU2 = iodu(I2,O2,_,SrcTemps2)},
	{ord_subtract(O2, O1, Odiff)},
	{ord_subtract(SrcTemps2, SrcTemps1, Tdiff0)},
	{ord_del_element(Tdiff0, T1, Tdiff)},
	(   {ord_subset(O1, O2)},
	    {ord_subset(SrcTemps2, SrcTemps1)}
	->  {R = []}
	;   {R = R0}
	),
	gen_dominates(R, IODU1).

temp_domination(MAXI, MAXP) -->
	{   for(I,0,MAXI),
	    fromto(KL1,KL2,KL3,[])
	do  (   cur_operation(I, copy, Op, [UseP|_], [DefP|_]),
		cur_operand(UseP, _, 1, [-1|UseTs]),
		cur_operand(DefP, _, 0, [-1,Temp])
	    ->  KL2 = [(UseTs-Op)-Temp|KL3]
	    ;   KL2 = KL3
	    )
	},
	{keysort(KL1, KL4)},
	{keyclumped(KL4, KLIC)}, % list of (UseTs-Op)-Temps, Temps is a symmetry class
	{   for(P,0,MAXP),
	    fromto(KL5,KL6,KL8,[]),
	    param(KLIC)
	do  cur_operand(P, _, U, Temps),
	    (   U=:=1,
		delete_null(Temps, [Key,T1,T2|Ts])
	    ->  findall(Class, class_for_opnd(Key,[T1,T2|Ts],KLIC,Class), Classes),
		(   foreach(Class1,Classes),
		    fromto(KL6,[Class1-P|KL7],KL7,KL8),
		    param(P)
		do  true
		)
	    ;   KL6 = KL8
	    )
	},
	{keysort(KL5, KL9)},
	{keyclumped(KL9, KL10)},
	(   foreach(Class2-Ps,KL10)
	do  [[Ps,Class2]]
	).

class_for_opnd(Key, Rest, KL, Class) :-
	Class = [_,_|_],
	member(([Key|_]-_)-Class, KL),
	ord_subset(Class, Rest).

% [MC] BZ2_indexIntoF contains the following phenomenon
% i58 uses p148 and p150, so they must be distinct regs
% via congruences, we have a possible equality path:
%   [p148,t31,p62,p191,t78,p151,p150]
% so t(148)=31 and t(191)=78 cannot both hold
% this info goes into "nogoods"
gen_infeasible(AVL1, MAXI, Before, Alldiffs1) -->
	{avl_fetch(tmp, AVL1, Tmp)},
	{avl_fetch(temps, AVL1, Temps)},
	{avl_fetch(ops, AVL1, Ops)},
	{avl_fetch(preassign, AVL1, Preassign)},
	{avl_fetch(strictly_congr, AVL1, Congr)},
	{avl_fetch(copyrel, AVL1, Copyrel)},
	{avl_fetch(minlive, AVL1, MinLive)},
	{   foreach(M,MinLive),
	    count(T0,0,_),
	    fromto(ZD1,ZD2,ZD3,[])
	do  (M>0 -> ZD2 = ZD3 ; ZD2 = [T0|ZD3])
	},
	{   foreach(Congr1,Congr),
	    fromto(AKL1,AKL2,AKL4,[])
	do  (   foreach(P5,Congr1),
		fromto(AKL2,[P5-Congr1|AKL3],AKL3,AKL4),
		param(Congr1)
	    do  true
	    )
	},
	{keysort(AKL1, AKL5)},
	{ord_list_to_avl(AKL5, CongrAVL)},
	{   fromto(Edges1,Edges2,Edges4,Edges5),
	    fromto(Alldiffs1,Alldiffs2,Alldiffs3,[]),
	    for(I,0,MAXI),
	    param(CongrAVL,ZD1)
	do  collect_edges_alldiffs(I, CongrAVL, ZD1, Edges2, Edges4, Alldiffs2, Alldiffs3)
	},
	{   foreach([P2,I2],Preassign),
	    foreach(I2,Regs1),
	    fromto(Edges5,[u(p(P2),I2,0)|Edges6],Edges6,Edges7)
	do  true
	},
	{   foreach(Ps,Congr),
	    fromto(Edges7,Edges8,Edges10,[])
	do  (   fromto(Ps,[P3,P4|Ps1],[P4|Ps1],[_]),
		fromto(Edges8,[u(p(P3),p(P4),0)|Edges9],Edges9,Edges10)
	    do  true
	    )
	},
	{empty_avl(AVL0)},
	{   foreach(u(V1,V2,Flag),Edges1),
	    fromto(AVL0,AVL2,AVL4,AVL5),
	    fromto(Cands1,Cands2,Cands3,[])
	do  (   avl_lookup(V1, AVL2, W1, AVL3),
		avl_lookup(V2, AVL3, W2, AVL4),
		(   Flag=0 -> W1 = W2, Cands2 = Cands3
		;   Cands2 = [u(V1,V2,W1,W2)|Cands3]
		)
	    )
	},
	{   foreach(Cand,Cands1),
	    fromto(KL1,KL2,KL3,[])
	do  Cand = u(_,_,W3,W4),
	    (   W3==W4 -> KL2 = KL3
	    ;   KL2 = [W3-Cand,W4-Cand|KL3]
	    )
	},
	{sort(Regs1, Regs2)},
	{keysort(KL1, KL4)},
	{keyclumped(KL4, KL5)},
	{   foreach(Tag-Clump1,KL5),
	    fromto(AVL5,AVL6,AVL7,AVL8)
	do  sort(Clump1, Clump2), % [MC] 20160215
	    avl_store(equiv(Tag), AVL6, Clump2, AVL7)
	},
	{   foreach(Alldiff,[Regs2|Alldiffs1]),
	    fromto(Alldiffs4,Alldiffs5,Alldiffs6,AlldiffsMod)
	do  (   Alldiff = [_,_|_] -> Alldiffs5 = [Alldiff|Alldiffs6]
	    ;   Alldiffs5 = Alldiffs6
	    )
	},
	{modulo_alldiffs(AlldiffsMod, [])},
	{before_edges(Before, Copyrel, BEdges, [])},
	{vertices_edges_to_ugraph([], BEdges, BGraph)},
	{transitive_closure(BGraph, BClosure)},
	{   foreach(Con1,Congr),
	    fromto(ConSet1,ConSet2,ConSet3,[])
	do  (   Con1 = [_]
	    ->  ConSet2 = ConSet3
	    ;   ConSet2 = [Con1|ConSet3]
	    )
	},
	{append(ConSet1, ConSet4)},
	{sort(ConSet4, ConSet5)}, % ConSet5 = set of operands involved in congr
	{   foreach(BB1,Ops),
	    fromto(Alldiffs7,Alldiffs8,Alldiffs13,[]),
	    param(ConSet5)
	do  BB1 = [BBin|_],
	    last(BB1, BBout),
	    cur_operation(BBin, _, _, [], Opsin0),
	    cur_operation(BBout, _, _, Opsout0, []),
	    ord_intersection(Opsin0, ConSet5, Opsin), % Opsin = in-opnds involved in congr
	    ord_intersection(Opsout0, ConSet5, Opsout), % out-opnds involved in congr
	    (   foreach(Opin,Opsin),
		fromto(Alldiffs8,Alldiffs9,Alldiffs12,Alldiffs13),
		param(Opsout)
	    do  cur_operand(Opin, _, _, Tempsin),
	        (   foreach(Opout,Opsout),
		    fromto(Alldiffs9,Alldiffs10,Alldiffs11,Alldiffs12),
		    param(Opin,Tempsin)
		do  (   cur_operand(Opout, _, _, Tempsout),
			ord_disjoint(Tempsin, Tempsout)
		    ->  Alldiffs10 = [[p(Opin),p(Opout)]|Alldiffs11]
		    ;   Alldiffs10 = Alldiffs11
		    )
		)
	    )
	},
	{map_alldiffs(Alldiffs4, Alldiffs4A, Alldiffs4B, AVL8)},
	{map_alldiffs(Alldiffs7, Alldiffs7A, Alldiffs7B, AVL8)},
	single_nogoods(Alldiffs4, Alldiffs4B, true, any),
	double_nogoods(Alldiffs4, Alldiffs4A, Alldiffs4B, true, any),
	single_nogoods(Alldiffs7, Alldiffs7B, cond(BClosure), any), % has effect
	double_nogoods(Alldiffs7, Alldiffs7A, Alldiffs7B, cond(BClosure), any), % has effect
	{tagged_copy_related_map(Temps, T2CR)},
	(   foreach(TmpBB,Tmp),
	    param(AVL8,BClosure,T2CR)
	do  (   {TmpBB = []} -> []
	    ;   {   foreach(T3,TmpBB),
		    foreach(t(T3),Ts7)
		do  true
		},
		{map_alldiffs([Ts7], [Ts7A], [Ts7B], AVL8)},
		single_nogoods([Ts7], [Ts7B], cond(BClosure), no_copy_related(T2CR)),
		double_nogoods([Ts7], [Ts7A], [Ts7B], cond(BClosure), no_copy_related(T2CR))
	    )
	),
	before_in_nogoods(Before),
	xchg_nogoods(AVL1),
	regdomain_nogoods(AVL1).

xchg_nogoods(AVL) -->
	% ITSS5 basically reconstructs "interchangeable", which currently seems inaccurate
	{avl_fetch(instructions, AVL, Instructions)},
	% group operations by insn set and temp sets
	{   foreach(Insns1,Instructions),
	    fromto(ITSS1,ITSS2,ITSS3,[]),
	    count(O,0,_)
	do  (   cur_operation(O, copy, _, Ps, _)
	    ->  ITSS2 = [itss(Insns1,Tss)-O|ITSS3],
		(   foreach(P,Ps),
		    foreach(Ts,Tss)
		do  cur_operand(P, _, _, Ts)
		)
	    ;   ITSS2 = ITSS3
	    )
	},
	{keysort(ITSS1, ITSS4)},
	{keyclumped(ITSS4, ITSS5)},
	(   foreach(_-Os1,ITSS5)
	do  (   {Os1 = [_]} -> []
	    ;   {   foreach(O1,Os1),
		    foreach(X,Xchgable)
		do  cur_operation(O1, _, _, _, [Q|_]),
		    cur_operand(Q, _, _, Xs),
		    last(Xs, X)
		},
		{Xchgable = [Y|Ys1]},
		{cur_temp(Y, _, Ps1, _)},
		(   foreach(P1,Ps1),
		    fromto(Ys1,Ys2,Ys3,_)
		do  (   {Ys2 = [_|Ys3]},
			{cur_operand(P1, _, 1, Ts1)}
		    ->  {ord_intersection(Ts1, Ys2, Ns)},
			(   foreach(N,Ns),
			    param(P1)
			do  [[p(P1)=t(N)]]
			)
		    ;   {Ys2 = Ys3}
		    )
		)
	    )
	).

regdomain_nogoods(AVL) -->
	{avl_fetch(instructions, AVL, Instructions)}, % set of instructions for operation o
	{avl_fetch(preassign, AVL, Preassign)},
	{avl_fetch(class, AVL, Class)},
	{avl_fetch(atoms, AVL, Atoms)},
	% group operations by insn set and use, def counts
	{   foreach(Insns1,Instructions),
	    foreach(iud(Insns1,NU,ND)-O,IO1),
	    count(O,0,_)
	do  cur_operation(O, _, _, Ou, Od),
	    length(Ou, NU),
	    length(Od, ND)
	},
	{keysort(IO1, IO2)},
	{keyclumped(IO2, IO3)},
	% for each group, for each operand, compute its MGRD (most general reg domain) from atoms unless predefined
	% for each temp, get its MGRD from its definer unless predefined
	{   foreach(_-Os,IO3),
	    fromto(UD1,UD2,UD7,[]),
	    fromto(TD1,TD2,TD7,[]),
	    param(Class,Atoms)
	do  mgrd_per_operand(Os, MGRDuse, MGRDdef, Class, Atoms),
	    (   foreach(O2,Os),
		fromto(UD2,UD3,UD6,UD7),
		fromto(TD2,TD3,TD6,TD7),
		param(MGRDuse,MGRDdef)
	    do  cur_operation(O2, _, _, Uses, Defs),
		(   foreach(U,Uses),
		    foreach(MGRDu,MGRDuse),
		    fromto(UD3,UD4,UD5,UD6)
		do  UD4 = [U-MGRDu|UD5]
		),
		(   foreach(D,Defs),
		    foreach(MGRDd,MGRDdef),
		    fromto(TD3,TD4,TD5,TD6)
		do  TD4 = [T-MGRDdn|TD5],
		    cur_operand(D, _, _, Ts),
		    last(Ts, T),
		    ord_del_element(MGRDd, -1, MGRDdn)
		)
	    )
	},
	{keysort(UD1, UD8)},
	{ord_list_to_avl(UD8, P2Dom1)},
	{keysort(TD1, TD8)},
	{ord_list_to_avl(TD8, T2Dom1)},
	{   foreach([P,R],Preassign),
	    fromto(P2Dom1,P2Dom2,P2Dom3,P2Dom4),
	    fromto(T2Dom1,T2Dom2,T2Dom3,T2Dom4)
	do  cur_operand(P, _, Use, Ts1),
	    (   Use = 1
	    ->  avl_store(P, P2Dom2, [R], P2Dom3),
		T2Dom2 = T2Dom3
	    ;   last(Ts1, T1),
		avl_store(T1, T2Dom2, [R], T2Dom3),
		P2Dom2 = P2Dom3
	    )
	},
	% for each use operand p, for each t in temps(p), if MGRD(p) disjoint MGRD(t), then p != t
	(   foreach(Q-_,UD8),
	    param(P2Dom4,T2Dom4)
	do  {avl_fetch(Q, P2Dom4, PDom)},
	    {cur_operand(Q, _, _, Ts2)},
	    (   foreach(T2,Ts2),
		param(Q,PDom,T2Dom4)
	    do  (   {T2 = -1} -> []
		;   {avl_fetch(T2, T2Dom4, TDom)},
		    {ord_intersect(PDom, TDom)} -> []
		;   [[p(Q)=t(T2)]]
		)
	    )
	).

mgrd_per_operand([O|_], MGRDuse, MGRDdef, Class, Atoms) :-
	nth0(O, Class, ClassO),
	cur_operation(O, _, Insns, Uses, Defs),
	(   foreach(I,Insns),
	    foreach(ClassOI,ClassO),
	    foreach(MGRDu,MGRDus),
	    foreach(MGRDd,MGRDds),
	    param(Atoms,Uses,Defs)
	do  same_length(Uses, MGRDu),
	    same_length(Defs, MGRDd),
	    append(MGRDu, MGRDd, MGRDud),
	    (   foreach(Aj,ClassOI),
		foreach(Dd,MGRDud),
		param(I,Atoms)
	    do  (I = 0 -> Dd = [-1] ; nth0(Aj, Atoms, Dd))
	    )
	),
	(   Uses = [] -> MGRDuse = []
	;   transpose(MGRDus, MGRDusT),
	    (   foreach(MGRDusTP,MGRDusT),
		foreach(Uuse,MGRDuse)
	    do  ord_union(MGRDusTP, Uuse)
	    )
	),
	(   Defs = [] -> MGRDdef = []
	;   transpose(MGRDds, MGRDdsT),
	    (   foreach(MGRDdsTP,MGRDdsT),
		foreach(Udef,MGRDdef)
	    do  ord_union(MGRDdsTP, Udef)
	    )
	).


before_in_nogoods(Before) -->
	(   foreach([_,Q,Cond],Before)
	do  (   {Cond = [[]]},
		{cur_operand(Q, Out, 1, [T1|_])},
		{T1 >= 0},
		{cur_temp(T1, _, _, In)},
		{cur_operation(Out, out, _, _, _)},
		{cur_operation(In, in, _, _, _)}
	    ->  [[p(Q)=t(T1)]]
	    ;   []
	    )
	).

modulo_alldiffs -->
	{findall(P, cur_modulo_2(P,0), M0bag)},
	{findall(P, cur_modulo_2(P,1), M1bag)},
	{sort(M0bag, M0set)},
	{sort(M1bag, M1set)},
	{ord_intersection(M0set, M1set, Ambiset)},
	{ord_subtract(M0set, Ambiset, Evenset)},
	{ord_subtract(M1set, Ambiset, Oddset)},
	(   foreach(P0,Evenset),
	    param(Oddset)
	do  {cur_operand(P0, _, _, Ts0)},
	    (   foreach(P1, Oddset),
		param(P0,Ts0)
	    do  {cur_operand(P1, _, _, Ts1)},
		{ord_intersection(Ts0, Ts1, Ts2)},
		({Ts2 = []} -> [] ; [[p(P0),p(P1)]])
	    )
	).

before_edges(Before, Copyrel) -->
	(   foreach([P,Q,[[]]],Before),
	    param(Copyrel)
	do  [P-Q],
	    (   {cur_operand(Q, _, 0, [_])}, % mandatory def operand
		{member([Q|Qs], Copyrel)}
	    ->  (   foreach(R,Qs),
		    param(P)
		do  [P-R]
		)
	    ;   []
	    )
	).

collect_edges_alldiffs(I, CongrAVL, ZD1, Edges2, Edges6, Alldiffs2, Alldiffs3) :-
	cur_operation(I, Ty, _, Us, Ds),
	append(Us, Ds, Opnds),
	(   foreach(P1,Opnds),
	    fromto(Edges2,Edges3a,Edges4,Edges5),
	    fromto(UseAlldiff1,UseAlldiff2,UseAlldiff3,[]),
	    fromto(DefAlldiff1,DefAlldiff2,DefAlldiff3,[]),
	    param(ZD1)
	do  cur_operand(P1, _, U, P1ts),
	    delete_null(P1ts, [T1|R1]),
	    (   U=:=0 ->
		Edges3a = [u(p(P1),t(T1),0)|Edges4],
		bump_alldiff(P1, P1ts, ZD1, UseAlldiff2, UseAlldiff3, DefAlldiff2, DefAlldiff3)
	    ;   R1 = [] ->
		Edges3a = [u(p(P1),t(T1),0)|Edges4],
		bump_alldiff(P1, P1ts, ZD1, DefAlldiff2, DefAlldiff3, UseAlldiff2, UseAlldiff3)
	    ;   true ->
		(   foreach(Tv,[T1|R1]),
		    fromto(Edges3a,[u(p(P1),t(Tv),1)|Edges3b],Edges3b,Edges4),
		    param(P1)
		do  true
		),
		bump_alldiff(P1, P1ts, ZD1, DefAlldiff2, DefAlldiff3, UseAlldiff2, UseAlldiff3)
	    )
	),
	(   I = 0		% initial (in)
	->  Edges5 = Edges6,
	    Alldiffs2 = Alldiffs3
	;   member(Ty, [function,pack,kill])
	->  Edges5 = Edges6,
	    Alldiffs2 = Alldiffs3
	;   Ty = low ->
	    Us = [Q1], Ds = [Q2],
	    Edges5 = [u(p(Q1),p(Q2),0)|Edges6],
	    Alldiffs2 = Alldiffs3
	;   Ty = copy
	->  Us = [Q1|_],
	    Ds = [Q2|_],
	    Edges5 = Edges6,
	    Alldiffs2 = [[p(Q1),p(Q2)]|Alldiffs3]
	;   alldiffs_for_co_use(UseAlldiff1, CongrAVL,
				Edges5, Edges6,
				Alldiffs2, [DefAlldiff1|Alldiffs3])
	).

bump_alldiff(P, Pts, ZD, Other, Other) -->
	(   {Pts = [-1|_]} -> []
	;   {Pts = [T|_], ord_member(T, ZD)} -> []
	;   [p(P)]
	).


% CO-USE-ALLDIFF
alldiffs_for_co_use([], _, Edges, Edges) --> !.
alldiffs_for_co_use(Set, CongrAVL, Edges0, Edges) -->
	{   foreach(p(P),Set),
	    foreach(TP-p(P),KL1)
	do  cur_operand(P, _, _, TP0),
	    delete_null(TP0, TP) % [MC] assert: TP0 does not contain the null temporary
	},
	{keysort(KL1, KL2)},
	{keyclumped(KL2, KL3)},
	{split_chunks_if_alldiff(KL3, L4, CongrAVL)},
	{edges_for_co_use(L4, Edges0, Edges)},
	findall(Subset, one_member_of_each(L4,Subset)).

split_chunks_if_alldiff(KL, L1, CongrAVL) :-
	(   foreach(_-Chunk,KL),
	    fromto(L1,L2,L4,[]),
	    param(CongrAVL)
	do  (   Chunk = [_,_|_],
		chunk_is_alldiff(Chunk, CongrAVL)
	    ->  assertz(cur_difftemp(Chunk)),
		(   foreach(P,Chunk),
		    fromto(L2,[[P]|L3],L3,L4)
		do  true
		)
	    ;   L2 = [Chunk|L4]
	    )
	).

edges_for_co_use(Chunks) -->
	(   foreach(Chunk,Chunks)
	do  findall(u(X,Y,1), ord_pairof(Chunk, X, Y))
	).

strip_p(List0, List) :-
	sort(List0, List1),
	(   foreach(p(X),List1),
	    foreach(X,List)
	do  true
	).

% the chunk of operands is "alldiff" if there is an instruction such that
% - each member of the chunk has a proxy in the instruction
% - the proxies are all "def", or they are "use" with disjoint temps
chunk_is_alldiff(Chunk, CongrAVL) :-
	(   foreach(p(P),Chunk),
	    foreach(Temps,Tempss),
	    param(CongrAVL,Insn,Use)
	do  avl_fetch(P, CongrAVL, Class),
	    member(Proxy, Class),
	    cur_operand(Proxy, Insn, Use, Temps)
	),
	(   Use=:=0 -> true
	;   sort(Tempss, TempsSet),
	    same_length(TempsSet, Chunk)
	).

one_member_of_each(L, Members) :-
	(   foreach(Val,L),
	    foreach(M, Members)
	do  member(M, Val)
	).

map_alldiffs(Lists1, Lists2, Lists3, AVL) :-
	(   foreach(List1,Lists1),
	    foreach(List2,Lists2),
	    foreach(List3,Lists3),
	    param(AVL)
	do  (   foreach(X,List1),
		foreach(Y,List2),
		foreach(Z,List3),
		param(AVL)
	    do  avl_fetch(X, AVL, Y),
		avl_fetch_set(equiv(Y), AVL, Z)
	    )
	).

single_nogoods(Alldiffs1, Alldiffs3, Cond, Filter) -->
	(   foreach(Alldiff1,Alldiffs1),
	    foreach(Alldiff3,Alldiffs3),
	    param(Cond,Filter)
	do  (   fromto(Alldiff1,[V1|V2s],V2s,[_]),
		fromto(Alldiff3,[U1|U2s],U2s,[_]),
		param(Cond,Filter)
	    do  {call(Filter, V1, V2s, V3s, U2s, U3s, U2s, U3s)},
		(   foreach(V2,V3s),
		    foreach(U2,U3s),
		    param(V1,U1,Cond)
% ROBERTO, 30/9/2015: U1 and U2 are not necessarily ordered which affects the
%  result of the intersection.  Fixed, see [MC] 20160215
		do  {ord_intersection(U1, U2, U12)},
		    (   foreach(u(V3,V4,_,_),U12),
			param(V1,V2,Cond)
		    do  emit_nogood(Cond, [V3=V4], V1, V2)
		    )
		)
	    )
	).

double_nogoods(Alldiffs1, Alldiffs2, Alldiffs3, Cond, Filter) -->
	(   foreach(Alldiff1,Alldiffs1),
	    foreach(Alldiff2,Alldiffs2),
	    foreach(Alldiff3,Alldiffs3),
	    param(Cond,Filter)
	do  (   fromto(Alldiff1,[V1|V2s],V2s,[_]),
		fromto(Alldiff2,[W1|W2s],W2s,[_]),
		fromto(Alldiff3,[U1|U2s],U2s,[_]),
		param(Cond,Filter)
	    do  {call(Filter, V1, V2s, V3s, W2s, W3s, U2s, U3s)},
		(   foreach(V2,V3s),
		    foreach(W2,W3s),
		    foreach(U2,U3s),
		    param(V1,W1,U1,Cond)
		do  (   foreach(Cand1,U1),
			param(W1,W2,U2,Cond,V1,V2)
		    do  (   foreach(Cand2,U2),
			    param(W1,W2,Cand1,Cond,V1,V2)
			do  {Cand1 = u(V3,V4,W3,W4)},
			    {Cand2 = u(V5,V6,W5,W6)},
			    (   {V3\==V5},
				{\+(f(W1,W2,W3,W5) = f(this,that,W4,W6))}
			    ->  emit_nogood(Cond, [V3=V4,V5=V6], V1, V2)
			    ;   []
			    )
			)
		    )
		)
	    )
	).

any(_, Vs, Vs, Ws, Ws, Us, Us).

no_copy_related(T2CR, V1, Vs0, Vs1, Ws0, Ws1, Us0, Us1) :-
	avl_fetch(V1, T2CR, Vsdel), !,
	ord_subtract(Vs0, Vsdel, Vs1),
	(   foreach(V0,Vs0),
	    foreach(W0,Ws0),
	    foreach(U0,Us0),
	    fromto(Vs1,Vs2,Vs3,[]),
	    fromto(Ws1,Ws2,Ws3,[]),
	    fromto(Us1,Us2,Us3,[])
	do  (   Vs2 = [V0|Vs3]
	    ->  Ws2 = [W0|Ws3],
		Us2 = [U0|Us3]
	    ;   Vs2 = Vs3, Ws2 = Ws3, Us2 = Us3
	    )
	).
% this happens e.g. for temps being the non-first def of an optional copy
no_copy_related(_, _, Vs, Vs, Ws, Ws, Us, Us).

emit_nogood(true, Nogood, _, _) --> [Nogood].
emit_nogood(cond(Graph), Nogood0, T1, T2) -->
	{nogood_to_ps([T1=T2|Nogood0], Ps)},
	{group_by_temp(Ps, Groups, Nogood0)},
	(   fromto(Groups,[Ts1-Ps1|Grtail],Grtail,[]),
	    param(Graph,Nogood0,T1,T2)
	do  (   foreach(Ts2-Ps2,Grtail),
		param(Ts1,Ps1,Graph,Nogood0,T1,T2)
	    do  (   {Ps1 = [P1-B|_]},
		    {Ps2 = [P2-B|_]},
		    {ord_disjoint(Ts1, Ts2)}
		->  (   {cur_modulo_2(P1, M)},
			\+{cur_modulo_2(P2, M)} % one odd reg, one even
		    ->  {sort(Nogood0, Nogood)}, [Nogood]
		    ;   {some_perm_before_perm(Ps1, Ps2, Graph)} -> []
		    ;   {sort2(P1, P2, P3, P4)},
			{append(Nogood0, [overlap(p(P3),p(P4))], Nogood1)},
			{sort(Nogood1, Nogood)}, [Nogood]
		    )
		;   []
		)
	    )
	).

sort2(X, Y, A, B) :-
	A is min(X,Y),
	B is max(X,Y).

group_by_temp(Ps, KL5, Ass) :-
	(   foreach(P-B,Ps),
	    foreach(Ts-(P-B),KL1),
	    param(Ass)
	do  (   (   member(p(P)=t(T), Ass) -> Ts0 = [T]
		;   cur_operand(P, _, _, T0),
		    delete_null(T0, Ts0)
		),
		(   Ts0 = [_] -> Ts = Ts0 % if single temp, group operands, because they must use same temp
		;   Ts = [_|Ts0] % if multiple temps, keep every operand singleton, because the may not use same temp
		)
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5).

nogood_to_ps(Items, Ps3) :-
	(   foreach(Item,Items),
	    fromto([],Ps1,Ps2,Ps3)
	do  (   Item = (t(T1)=t(T2))
	    ->  cur_temp(T1, _, [P1|_], _),
	        cur_congruence(P1, Inc1),
	        cur_temp(T2, _, [P2|_], _),
		cur_congruence(P2, Inc2),
		ord_union(Inc1, Inc2, Inc)
	    ;   Item = (p(P1)=t(T2))
	    ->  cur_congruence(P1, Inc1),
	        cur_temp(T2, _, [P2|_], _),
		cur_congruence(P2, Inc2),
		ord_union(Inc1, Inc2, Inc)
	    ;   Item = (p(P1)=p(P2))
	    ->  cur_congruence(P1, Inc1),
		cur_congruence(P2, Inc2),
		ord_union(Inc1, Inc2, Inc)
	    ),
	    ord_union(Ps1, Inc, Ps2)
	).

some_perm_before_perm(Ps1, Ps2, Closure) :-
	member(P1-_, Ps1),
	member(P2-_, Ps2),
	sort2(P1, P2, P3, P4),
	perm_before_perm(P3, P4, Closure).

perm_before_perm(P1, P2, Closure) :-
	(   cur_p_before_p(P1, P2, YN) -> YN = yes
	;   (member(P1-Reachable, Closure) -> true),
	    ord_member(P2, Reachable) % live_range(P1) precedes live_range(P2)
	->  assertz(cur_p_before_p(P1, P2, yes))
	;   cur_operand(P1, O, 1, _),
	    cur_operand(P2, O, 0, _),
	    cur_operation(O, Type, _, _, _),
	    member(Type, [low,high,combine]) % def,use operands of the same low/high/combine
				% specific constraints prevent overlap
	->  assertz(cur_p_before_p(P1, P2, yes))
	;   assertz(cur_p_before_p(P1, P2, no)), fail
	).

gen_cur_congruence(AVL) :-
	retractall(cur_congruence(_,_)),
	avl_fetch(tmp, AVL, Tmp),
	avl_fetch(ops, AVL, Ops),
	avl_fetch(operands, AVL, Operands),
	avl_fetch(strictly_congr, AVL, Congr),
	(   foreach(Ops1,Ops),
	    foreach(Tmp1,Tmp),
	    foreach(bb(_,OPNDsucc..OPNDmax,_,_),BBs),
	    fromto(-1,OPNDprev,OPNDmax,_),
	    fromto(-1,Iprev,Imax,_),
	    param(Operands)
	do  Isucc is Iprev+1,
	    OPNDsucc is OPNDprev+1,
	    last(Ops1, Imax),
	    (   Tmp1 = []	% no temps, no opnds?
	    ->  OPNDmax is OPNDsucc-1
	    ;   prefix_length(Operands, OpPartA, Isucc),
		append(OpPartA, OpPartBC, Operands),
		NI is Imax-Iprev,
		prefix_length(OpPartBC, OpPartB, NI),
		append(OpPartB, OpFlatB),
		last(OpFlatB, OPNDmax)
	    )
	),
	(   foreach(Ps,Congr),
	    param(BBs)
	do  (   foreach(P,Ps),
		foreach(P-B,PBs),
		param(BBs)
	    do  bb_of_operand(BBs, P, 0, B)
	    ),
	    (   member(K,Ps),
		assertz(cur_congruence(K, PBs)),
		fail
	    ;   true
	    )
	),
	% discovering rematerialized temps
	avl_fetch(copyrel, AVL, Copyrel),
	(   foreach(Rel,Copyrel)
	do  Rel = [CP1|CPs],
	    cur_operand(CP1, _, 0, Ts),
	    last(Ts, Tprim),
	    (   foreach(CP,CPs),
		param(Tprim)
	    do  (   cur_operand(CP, I, 0, [-1,Tre]),
		    cur_operation(I, linear, _, _, _)
		->  assertz(cur_remat(Tprim,Tre))
		;   true
		)
	    )
	).

% ROBERTO, 30/9/2015: This predicate has actually more reduction power
% (experimentally on two different targets) than "piggy-backing on the
% constraint solver".

non_subsumed_nogoods(Nogoods0, Nogoods) :-
	kernel_set(Nogoods0, [], Nogoods).

kernel_set(Items, SOS0, SOS) :-
	kernel(Items, SOS0, SOS1),
	sort(SOS1, SOS).

kernel([], SOS, SOS).
kernel([X|Xs], SOS0, SOS) :-
	length(X, XL),
	sos_add(SOS0, X, XL, SOS1),
	kernel(Xs, SOS1, SOS).

sos_add([], X, _, [X]).
sos_add([X|Xs], Y, YL, [X|Xs]) :-
	length(X, XL),
	ord_subsumes(X, XL, Y, YL), !. % SOS element subsumes addition
sos_add([X|Xs], Y, YL, Zs) :-
	length(X, XL),
	ord_subsumes(Y, YL, X, XL), !,	% SOS element subsumed by addition
	sos_add(Xs, Y, YL, Zs).
sos_add([X|Xs], Y, YL, [X|Zs]) :-
	sos_add(Xs, Y, YL, Zs).

avl_lookup(Key, AVL0, Val, AVL) :-
	avl_fetch(Key, AVL0, Val), !,
	AVL = AVL0.
avl_lookup(Key, AVL0, Val, AVL) :-
	avl_store(Key, AVL0, Val, AVL).

avl_fetch_set(Key, AVL, Val) :-
	avl_fetch(Key, AVL, Val), !.
avl_fetch_set(_, _, []).


gen_across_call([_,_], _, _, _, CondBefore, CondBefore) --> !.
gen_across_call([_,ICall,INext|Rest], AVL1, I0/In, DDGraph,
		CondBefore0, CondBefore) -->
	{avl_fetch(callersaved, AVL1, CallerSaved)},
	{avl_fetch(preassign, AVL1, Preassign)},
	{list_to_fdset(CallerSaved, CSSet)},
	{collect_before_call(ICall, I0/In, DDGraph,
			     Preassign, CSSet, Before1, [])},
	{keysort(Before1, Before2)},
	{keyclumped(Before2, Before3)}, % singletons only
	{collect_after_call(ICall, DDGraph,
			    Preassign, CSSet, After1, [])},
	{keysort(After1, After2)},
	{keyclumped(After2, After3)}, % non-singletons
	{cond_before_items(Before1, After1, ICall, Preassign, CSSet, DDGraph,
			   CondBefore0, CondBefore1)},
	{across_candidates(ICall, I0/In, DDGraph, Preassign, CSSet, Cands)},
	{   foreach(T2,Cands),
	    foreach(t(T2)-c,Cands3)
	do  true
	},
	{append([Before3, After3, Cands3], Both3)},
	{keysort(Both3, Both4)},
	{keyclumped(Both4, Both5)},
	(   foreach(T1-Clump,Both5),
	    param(ICall)
	do  (   {Clump = [Disj1,Disj2,c]}
	    ->  {merge_disjunctions(Disj1, Disj2, Disj3)},
		[[ICall, T1, Disj3]]
	    ;   {member(c, Clump)}
	    ->  [[ICall, T1, []]]
	    ;   {Clump = [Disj1,Disj2]} % contributes to nogoods? witness snocString
	    ->  {merge_disjunctions(Disj1, Disj2, Disj3)},
		[[ICall, T1, Disj3]]
	    ;   []
	    )
	),
	gen_across_call([ICall,INext|Rest], AVL1, I0/In, DDGraph,
			CondBefore1, CondBefore).

alt_across_call([_,_], _) --> !.
alt_across_call([IPrev,ICall,INext|Rest], DDGraph) -->
	{transpose_ugraph(DDGraph, DDGraphT)},
	{reachable(ICall, DDGraphT, Predecessors0)},
	{ord_del_element(Predecessors0, ICall, Predecessors)},
	{   foreach(P1,Predecessors),
	    fromto(DefBefore0,DefBefore2,DefBefore5,[])
	do  cur_operation(P1, _, _, _, Ops1),
	    (   foreach(Op1,Ops1),
		fromto(DefBefore2,[T1|DefBefore4],DefBefore4,DefBefore5)
	    do  cur_operand(Op1, _, 0, [T1|_])
	    )
	},
	{reachable(ICall, DDGraph, Successors0)},
	{ord_del_element(Successors0, ICall, Successors)},
	{   foreach(S1,Successors), % proper successors only
	    fromto(UseAfter0,UseAfter2,UseAfter5,[])
	do  cur_operation(S1, _, _, Ops2, _),
	    (   foreach(Op2,Ops2),
		fromto(UseAfter2,UseAfter3,UseAfter4,UseAfter5)
	    do  cur_operand(Op2, _, 1, [T2|T2s]),
		(   cur_remat(T2, T2r), % if T2r rematerializes T2
		    ord_member(T2r, T2s)
		->  UseAfter3 = UseAfter4 % then neither T2 nor any of its copies needs to be live across the call
		;   UseAfter3 = [T2|UseAfter4] % else T2 or one of its copies needs to be live across the call
		)
	    )
	},
	{sort(DefBefore0, DefBefore1)},
	{sort(UseAfter0, UseAfter1)},
	{ord_intersection(DefBefore1, UseAfter1, Across0)},
	{ord_del_element(Across0, -1, Across)}, % optional operands can occur
	(   foreach(T3,Across),
	    param(ICall)
	do  [ICall-T3]
	),
	{alt_across_pair_candidates(DefBefore1, UseAfter1, Across,
				    IPrev, ICall, INext, Cands)},
	(   foreach([ICall1,U-D],Cands), % every U and D can only be used once
	    fromto([],Seen1,Seen2,_)
	do  (   {ord_disjoint([U,D], Seen1)}
	    ->  {ord_union(Seen1, [U,D], Seen2)},
		[ICall1-(U-D)]
	    ;   {Seen1 = Seen2}
	    )
	),
	alt_across_call([ICall,INext|Rest], DDGraph).

alt_across_pair_candidates(DefBefore1, UseAfter1, Across, IPrev, ICall, INext, Cands0) :-
	ord_subtract(DefBefore1, Across, Before),
	ord_subtract(UseAfter1, Across, After),
	(   for(I,IPrev+1,INext-1),
	    fromto(Cands0,Cands1,Cands2,[]),
	    param(ICall, Before, After)
	do  cur_operation(I, _, Opcodes, Us, Ds),
	    (   Opcodes = [0|_] -> Cands1 = Cands2
	    ;   I=:=ICall -> Cands1 = Cands2
	    ;   findall([ICall, U-D],
			alt_across_pair_candidates(Us, Ds, Before, After, U-D),
			Cands1, Cands2)
	    )
	).

alt_across_pair_candidates(Us, Ds, Before, After, Tuse-Tdef) :-
	member(Puse, Us),
	cur_operand(Puse, _, 1, [Tuse|_]),
	ord_member(Tuse, Before),
	member(Pdef, Ds),
	cur_operand(Pdef, _, 0, [Tdef|_]),
	ord_member(Tdef, After).

merge_disjunctions(Disj1, Disj2, Disj7) :-
	(   foreach(Conj1,Disj1),
	    fromto(Disj3,Disj4,Disj6,[]),
	    param(Disj2)
	do  (   foreach(Conj2,Disj2),
		fromto(Disj4,[Conj4|Disj5],Disj5,Disj6),
		param(Conj1)
	    do  append(Conj1, Conj2, Conj3),
		sort(Conj3, Conj4)
	    )
	),
	kernel_set(Disj3, [], Disj7).

collect_before_call(ICall, I0/In, DDGraph, Preassign, CSSet) -->
				% 4. all defs of a proper predecessor of ICall
	{transpose_ugraph(DDGraph, DDGraphT)},
	{reachable(ICall, DDGraphT, Predecessors0)},
	{ord_del_element(Predecessors0, ICall, Predecessors1)},
	{cur_operation(ICall, _, _, Us, _)},
	{extend_predecessors(Predecessors1, Us, Predecessors)},
	{   for(I1,I0,In),
	    foreach(I1,I0n)
	do  true
	},
	{reachable(ICall, DDGraph, Successors)},
	{ord_subtract(I0n, Successors, NonSuccessors)},
	(   foreach(IP4-Cond4,Predecessors)
	do  {cur_operation(IP4, _, _, _, Ops4)},
	    (   foreach(Op4,Ops4),
		param(Cond4)
	    do  {cur_operand(Op4, _, 0, T4s)},
		{delete_null(T4s, [T4])},
		[t(T4)-Cond4]
	    )
	),
				% 5. all defs of NON-CALL insns using a caller-saved T5
				% that was defined by a proper predecessor of ICall
	(   foreach(IP5-Cond5,Predecessors),
	    param(NonSuccessors,Preassign,CSSet)
	do  {cur_operation(IP5, _, _, _, Ops5)},
	    (   foreach(Op5,Ops5),
		param(Cond5,NonSuccessors,Preassign,CSSet)
	    do  {cur_operand(Op5, _, 0, T5s)},
		(   {delete_null(T5s, [T5])},
		    {cond_caller_saved(Op5, T5, Cond5, ICST5, Preassign, CSSet)}
		->  (   foreach(I6,NonSuccessors),
			param(T5,ICST5)
		    do  {cur_operation(I6, Ty6, _, Us6, Ds6)},
			{append(Us6, Ds6, Ops6)},
			{   foreach(Op6,Ops6),
			    fromto(Defs6,Defs7,Defs8,[]),
			    fromto(Uses6,Uses7,Uses8,[]),
			    param(ICST5)
			do  cur_operand(Op6, _, U6, T6s),
			    delete_null(T6s, Ts6),
			    (   U6=:=0
			    ->  Defs7 = [Op6-Ts6|Defs8], Uses7 = Uses8
			    ;   Defs7 = Defs8, Uses7 = [Op6-Ts6|Uses8]
			    )
			},
			(   foreach(Use6-UseTs6,Uses6),
			    param(Defs6,Ty6,T5,ICST5)
			do  {Ty6\==function},
			    {member(T5, UseTs6)}
			->  (   foreach(_-DefTs6,Defs6),
				param(Use6,T5,ICST5)
			    do  (   foreach(D6,DefTs6),
				    param(Use6,T5,ICST5)
				do  [t(D6)-[p(Use6)=t(T5)|ICST5]]
				)
			    )
			;   []
			)
		    )
		;   []
		)
	    )
	).

% include all definers of used temps that are copies
extend_predecessors(Predecessors0, Us, Predecessors1) :-
	(   foreach(P1,Predecessors0),
	    fromto(Predecessors1,[P1-[]|Predecessors2],Predecessors2,Predecessors3)
	do  true
	),
	(   foreach(P,Us),
	    fromto(Predecessors3,Predecessors4,Predecessors6,[])
	do  cur_operand(P, _, 1, [_|Ts]),
	    (   foreach(T,Ts),
		fromto(Predecessors4,[IDef-[p(P)=t(T)]|Predecessors5],Predecessors5,Predecessors6),
		param(P)
	    do  cur_temp(T, _, _, IDef)
	    )
	).


collect_after_call(ICall, DDGraph, Preassign, CSSet) -->
				% 4. all uses of a successor of ICall
	{reachable(ICall, DDGraph, Successors0)},
	{ord_del_element(Successors0, ICall, Successors)},
	(   foreach(IP4,Successors) % proper successors only
	do  {cur_operation(IP4, _, _, Ops4, _)},
	    (   foreach(Op4,Ops4)
	    do  {cur_operand(Op4, _, 1, Ts4)},
		{delete_null(Ts4, T4s)},
		(   foreach(T4,T4s),
		    param(Op4)
		do  [t(T4)-[p(Op4)=t(T4)]]
		)
	    )
	),
				% 5. forall caller-saved temps used by a PROPER successor of ICall
				% the uses of the NON-CALL definer of such a temp
	(   foreach(IP5,Successors),
	    param(Preassign,CSSet)
	do  {cur_operation(IP5, _, _, Ops5, _)},
	    (   foreach(Op5,Ops5),
		param(Preassign,CSSet)
	    do  {cur_operand(Op5, _, 1, Ts5)},
		(   {maybe_caller_saved(Op5, Preassign, CSSet, ICS5)},
		    {\+(ICS5=no(_))}
		->  {delete_null(Ts5, T5s)},
		    (   foreach(T5,T5s),
			param(Op5,Preassign,CSSet)
		    do  % assuming p(Op5)=t(T5)
		        {cond_caller_saved(Op5, T5, [], ICST5, Preassign, CSSet)},
			{cur_temp(T5, _, _, IDef)},
			{cur_operation(IDef, Ty5, _, OpsDef, _)},
			(   foreach(OpDef,OpsDef),
			    param(Op5,T5,Ty5,ICST5)
			do  {cur_operand(OpDef, _, 1, TDefs0)},
			    (   {Ty5\==function}
			    ->  {delete_null(TDefs0, TDefs)},
				(   foreach(TDef,TDefs),
				    param(OpDef,Op5,T5,ICST5)
				do  [t(TDef)-[p(OpDef)=t(TDef),p(Op5)=t(T5)|ICST5]] %here
				)
			    ;   []
			    )
			)
		    )
		;   []
		)
	    )
	).

across_candidates(ICall, I0/In, DDGraph, Preassign, CSSet, Cands) :-
	transpose_ugraph(DDGraph, DDGraphT),
	reachable(ICall, DDGraphT, Predecessors0),
	ord_del_element(Predecessors0, ICall, Predecessors),
	reachable(ICall, DDGraph, Successors),
	(   for(I,I0,In),
	    fromto(Cands0,Cands1,Cands4,[]), % temps def by non-successor not ICall
	    fromto(Use0,Use1,Use4,[]),	     % temps use by non-copy non-predecessor not ICall
				% plus temps use by ICall not in caller-saved position
	    param(ICall,Preassign,CSSet,Predecessors,Successors)
	do  cur_operation(I, _, _, Us, Ds),
	    (   ord_member(I, Successors)
	    ->  Cands1 = Cands4
	    ;   (   foreach(P,Ds),
		    fromto(Cands1,Cands2,Cands3,Cands4),
		    param(Preassign,CSSet)
		do  cur_operand(P, _, 0, Ts),
		    (   \+maybe_caller_saved(P, Preassign, CSSet, yes(_))
		    ->  delete_null(Ts, [T]),
			Cands2 = [T|Cands3]
		    ;   Cands2 = Cands3
		    )
		)
	    ),
	    (   ord_member(I, Predecessors)
	    ->  Use1 = Use4
	    ;   (   foreach(P2,Us),
		    fromto(Use1,Use2,Use3,Use4),
		    param(I,ICall,Preassign,CSSet)
		do  cur_operand(P2, _, 1, Ts2),
		    (   \+ (Ts2=[-1|_]),
			(   I=\=ICall
			;   maybe_caller_saved(P2, Preassign, CSSet, no(_))
			)
		    ->  Use2 = [Ts2|Use3]
		    ;   Use2 = Use3
		    )
		)
	    )
	),
	append(Use0, Use5),
	sort(Use5, Use),
	ord_intersection(Cands0, Use, Cands).


maybe_caller_saved(Op, Preassign, CSSet, Cond) :-
	(   memberchk([Op,R], Preassign)
	->  (fdset_member(R, CSSet) -> Cond = yes(R) ; Cond = no(R))
	;   Cond = maybe
	).

cond_caller_saved(P, T, Cond0, Cond, Preassign, CSSet) :-
	(Q=P ; member(p(Q)=t(T), Cond0)),
	maybe_caller_saved(Q, Preassign, CSSet, YN),
	YN \== maybe, !,
	YN = yes(_),
	Cond = Cond0.
cond_caller_saved(_, T, Cond, [cs(T)|Cond], _, _).

data_dependency_graph(OpsBB, DepBB, DDGraph) :-
	(   foreach([I,J],DepBB),
	    fromto(Edges1,Edges2,Edges3,Edges4)
	do  (   I < J, % forward in the .ext.uni, ruling out (fun) to (call) dep
	        \+cur_operation(I, _, [0|_], _, _),
		\+cur_operation(J, _, [0|_], _, _) % none is optional
	    ->  Edges2 = [I-J|Edges3]
	    ;   Edges2 = Edges3
	    )
	),
	(   foreach(I2,OpsBB),
	    fromto(Edges4,Edges5,Edges8,[])
	do  cur_operation(I2, _, _, OpndsI, _),
	    (   foreach(P,OpndsI),
		fromto(Edges5,[D-I2|Edges7],Edges7,Edges8),
		param(I2)
	    do  cur_operand(P, _, 1, Ts),
		delete_null(Ts, [T|_]),
		cur_temp(T, _, _, D)
	    )
	),
	sort(Edges1, Edges9),	% get rid of copies
	findall(X-Y, (member(X-Y,Edges9),
			 cur_operation(X, function, _, _, _),
			 cur_operation(Y, kill, _, _, _)),
		FunKillEdges),
	(   foreach(C1-K1,FunKillEdges),
	    fromto(Edges10,Edges11,Edges12,Edges9),
	    param(Edges9)
	do  findall(K1-Succ, (member(C1-Succ,Edges9), Succ=\=K1), Edges11, Edges12)
	),
	vertices_edges_to_ugraph([], Edges10, DDGraph).

caller_saved_temps(AVL1, CSTemps1) :-
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(callersaved, AVL1, CallerSaved),
	list_to_fdset(CallerSaved, CSSet),
	(   foreach([Pi,Ri],Preassign),
	    fromto(CSTemps1,CSTemps2,CSTemps3,[]),
	    param(CSSet)
	do  (   fdset_member(Ri, CSSet),
		cur_operand(Pi, _, _, [Ti])
	    ->  CSTemps2 = [Ti|CSTemps3]
	    ;   CSTemps2 = CSTemps3
	    )
	).

caller_saved_opnds(AVL1, CSOpnds1) :-
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(callersaved, AVL1, CallerSaved),
	list_to_fdset(CallerSaved, CSSet),
	(   foreach([Pi,Ri],Preassign),
	    fromto(CSOpnds1,CSOpnds2,CSOpnds3,[]),
	    param(CSSet)
	do  (   fdset_member(Ri, CSSet)
	    ->  CSOpnds2 = [Pi|CSOpnds3]
	    ;   CSOpnds2 = CSOpnds3
	    )
	).

singletons(AVL1, Singletons1) :-
	avl_fetch(temps, AVL1, Temps),
	(   foreach(TempsI,Temps),
	    fromto(Singletons1,Singletons2,Singletons3,[]),
	    count(I,0,_)
	do  (   TempsI = [T]
	    ->  Singletons2 = [p(I)=t(T)|Singletons3]
	    ;   TempsI = [-1,T]
	    ->  Singletons2 = [p(I)=t(T)|Singletons3]
	    ;   Singletons2 = Singletons3
	    )
	).

disj_to_json(Disj, Json2) :-
	(   foreach(D,Disj),
	    foreach(J,Json1)
	do  conj_to_json(D, J)
	),
	sort(Json1, Json2).

% postcond: Json is a _set_, for subsumption checks etc.
% this is a good place to remove subsumed p(_)=t(_) too.
conj_to_json(Conj, Json) :-
	(   foreach(Cond,Conj),
	    fromto(C2,C3,C4,[])
	do  lit_to_json(Cond, C3, C4)
	),
	sort(C2, Json).

lit_to_json(p(P1)=p(P2)) --> !,
	[[/*JTAG*/0,P1,P2]].
lit_to_json(p(P2)=t(T2)) -->
	{cur_operand(P2, _, _, [T2])}, !.
lit_to_json(p(P2)=t(T2)) --> !,
	[[/*JTAG*/1,P2,T2]].
lit_to_json(a(O)) --> !,
	[[/*JTAG*/2,O]].
lit_to_json(i(O)=I) --> !,
	[[/*JTAG*/3,O,I]].
lit_to_json(overlap(p(P1),p(P2))) --> !,
	[[/*JTAG*/4,P1,P2]].
lit_to_json(overlap(t(T1),t(T2))) --> !,
	[[/*JTAG*/5,T1,T2]].
lit_to_json(cs(T2)) --> !,
	[[/*JTAG*/6,T2]].
lit_to_json(i(O)\=I) --> !,
	[[/*JTAG*/7,O,I]].
lit_to_json(r(t(P)) in C) --> !,
	[[/*JTAG*/8,P,C]].

temp_to_operand(T, P, Nogood) :-
	(   member(p(P)=t(T), Nogood) -> true
	;   cur_temp(T, _, [P|_], _) % get defining operand
	).

last_use(AVL1, MAXI, MAXT, DDGraphs, LastUse) :-
	avl_fetch(callersaved, AVL1, CallerSaved),
	avl_fetch(ops, AVL1, Ops),
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(strictly_congr, AVL1, Congr),
	list_to_fdset(CallerSaved, CSSet),
	(   for(T,0,MAXT),
	    fromto(OptionalTs1,OptionalTs2,OptionalTs3,[])
	do  cur_temp(T, _, [P4|_], _),
	    (   cur_operand(P4, _, 0, [-1|_])
	    ->  OptionalTs2 = [T|OptionalTs3]
	    ;   OptionalTs2 = OptionalTs3
	    )
	),
	(   foreach(Congr1,Congr),
	    fromto(AKL1,AKL2,AKL4,[])
	do  (   foreach(P5,Congr1),
		fromto(AKL2,[P5-Congr1|AKL3],AKL3,AKL4),
		param(Congr1)
	    do  true
	    )
	),
	list_to_avl(AKL1, CongrAVL),
	(   for(I,0,MAXI),
	    fromto(LastUse1,LastUse2,LastUse5,[]),
	    fromto(KL1,KL2,KL5,[]),
	    param(CongrAVL,Preassign,CSSet,OptionalTs1)
	do  cur_operation(I, TypeName, Opcodes, Uses, Defs),
	    (   Opcodes = [0|_]
	    ->  KL2 = KL5
	    ;   (   foreach(P2,Uses),
		    fromto(KL2,KL3,KL4,KL5),
		    param(OptionalTs1)
		do  cur_operand(P2, _, 1, [T2|_]), % use operand HERE: T2 can be -1 or optional
		    (   T2 =\= -1,
			ord_nonmember(T2, OptionalTs1)
		    ->  KL3 = [T2-P2|KL4]
		    ;   KL3 = KL4
		    )
		)
	    ),
	    (   member(TypeName, [tail,out,kill])
	    ->  (   foreach(P1,Uses),
		    fromto(LastUse2,LastUse3,LastUse4,LastUse5)
		do  cur_operand(P1, _, 1, _) % use operand HERE: should optional opnd really be last use?
		->  LastUse3 = [P1|LastUse4]
		;   LastUse3 = LastUse4
		)
	    ;   TypeName = function
	    ->  (   foreach(P,Uses),
		    fromto(LastUse2,LastUse3,LastUse4,LastUse5),
		    param(Preassign, CSSet)
		do  cur_operand(P, _, 1, _), % use operand
		    memberchk([P,R], Preassign),
		    fdset_member(R, CSSet) % caller-saved
		->  LastUse3 = [P|LastUse4]
		;   LastUse3 = LastUse4
		)
	    ;   TypeName = linear, % regular instruction "redefining" one of its operands
		member(Use, Uses),
		avl_fetch(Use, CongrAVL, Class),
		member(Def, Defs),
		avl_fetch(Def, CongrAVL, Class)
	    ->  LastUse2 = [Use|LastUse5],
		assertz(cur_redefiner(I, Use, Def))
	    ;   LastUse2 = LastUse5
	    )
	),
	% now add use operands that are the only obligatory ones for given temp
	keysort(KL1, KL6),
	keyclumped(KL6, KL7),
	(   foreach(_-Ps,KL7),
	    fromto(LastUse6,LastUse7,LastUse8,[]),
	    param(Ops,DDGraphs,LastUse1)
	do  (   \+ord_intersect(Ps, LastUse1),
		last_use1(Ps, Ops, DDGraphs, P3)
	    ->  LastUse7 = [P3|LastUse8]
	    ;   LastUse7 = LastUse8
	    )
	),
	sort(LastUse6, LastUse9),
	ord_union(LastUse1, LastUse9, LastUse).

% Opnds are the use operands for some temp.
% Find one that is for sure a successor of the others
% N.B. Jump instructions are considered unsafe for now, due to delayed branches
last_use1([Opnd], _, _, Opnd) :- !,
	cur_operand(Opnd, L, _, _),
	cur_operation(L, TypeName, _, _, _),
	TypeName\==branch.
last_use1(Opnds, Ops, DDGraphs, Last) :-
	select(Last, Opnds, Rest),
	cur_operand(Last, L, _, _),
	cur_operation(L, TypeName, _, _, _),
	TypeName\==branch,
	select_ddgraph(Ops, DDGraphs, L, DDGraph),
	(   foreach(Pred,Rest),
	    param(L,DDGraph)
	do  cur_operand(Pred, I, _, _),
	    reachable(I, DDGraph, Reachable),
	    ord_member(L, Reachable)
	).

select_ddgraph([_|BBs], [DDGraph|_], I, DDGraph) :-
	\+ (BBs = [[I1|_]|_], I1 =< I), !.
select_ddgraph([_|BBs], [_|DDGraphs], I, DDGraph) :-
	select_ddgraph(BBs, DDGraphs, I, DDGraph).

% This version generates cliques of all different [t(_),...,t(_)]
% Provenance:
% (1) operands of same insn with identical temps whose regs must be alldiff,
%     see cur_difftemp/1
% (2) Op1 is caller-saved, Op2 comes later, identical temps
% (3) Op1 is self-modifying, Op2 comes later, identical temps
difftemps(AVL, MAXI, LastUse, Cliques) :-
	(   for(I,0,MAXI),
	    fromto(KL1,KL2,KL5,[])
	do  cur_operation(I, TypeName, Opcodes, Opnds, _),
	    (   TypeName = in -> KL2 = KL5 % (in)
	    ;   TypeName = kill -> KL2 = KL5 % (kill)
	    ;   TypeName = pack -> KL2 = KL5 % (pack)
	    ;   Opcodes = [0|_] -> KL2 = KL5 % optional
	    ;   (   foreach(P,Opnds),
		    fromto(KL2,[Ptemps-ip(I,P)|KL4],KL4,KL5),
		    param(I)
		do  cur_operand(P, _, 1, Ptemps)
		)
	    )
	),
	keysort(KL1, KL6),
	keyclumped(KL6, KL7),
	avl_fetch(precs, AVL, Precs),
	(   foreach([X,Y],Precs),
	    foreach(X-Y,PEdges)
	do  true
	),
	vertices_edges_to_ugraph([], PEdges, PGraph),
	transitive_closure(PGraph, PClosure),
	(   foreach(_-Clump,KL7),
	    fromto(DiffTemps1,DiffTemps2,DiffTemps5,[]),
	    param(LastUse,PClosure)
	do  findall(Q-R, ord_pairof(Clump,Q,R), Pairs),
	    (   foreach(ip(I1,P1)-ip(I2,P2),Pairs),
		fromto(DiffTemps2,DiffTemps3,DiffTemps4,DiffTemps5),
		param(LastUse,PClosure)
	    do  (   member(P1, LastUse),
		    neighbors(I1, PClosure, Neighbors),
		    ord_member(I2, Neighbors)
		->  DiffTemps3 = [P1-P2,P2-P1|DiffTemps4]
		;   DiffTemps3 = DiffTemps4
		)
	    )
	),
	vertices_edges_to_ugraph([], DiffTemps1, DGraph),
	max_cliques(DGraph, Cliques).

max_cliques([], []) :- !.
max_cliques(Graph, Cliques) :-
	vertices(Graph, P),
	bron_kerbosch2([], P, [], Graph, Bag, []),
	sort(Bag, Cliques).

bron_kerbosch2(R, [], [], _) --> !, [Cl],
	{sort(R, Cl)}.
bron_kerbosch2(_, [], _, _) --> !.
bron_kerbosch2(R, P, X, G) -->
	{P = [U|_]},
	{neighbors(U, G, UNs)},
	{ord_subtract(P, UNs, Vs)},
	(   foreach(V,Vs),
	    fromto(P,P1,P2,_),
	    fromto(X,X1,X2,_),
	    param(G,R)
	do  {neighbors(V, G, VNs)},
	    {ord_intersection(P1, VNs, P3)},
	    {ord_intersection(X1, VNs, X3)},
	    {ord_del_element(P1, V, P2)},
	    {ord_add_element(X1, V, X2)},
	    bron_kerbosch2([V|R], P3, X3, G)
	).

difftemp_binary_nogoods(DiffTemps) -->
	{findall(P0-Q0, (member(Clique,DiffTemps), ord_pairof(Clique,P0,Q0)), Pairs)},
	(   foreach(P-Q,Pairs)
	do  [[[/*JTAG*/0,P,Q]]]
	).

difftemp_binary_nogoods_redundant(DiffTemps) -->
	{findall(P0-Q0, (member(Clique,DiffTemps), ord_pairof(Clique,P0,Q0)), Pairs)},
	(   foreach(P-Q,Pairs)
	do  {cur_operand(P, _, _, Ts)},
	    (   foreach(T,Ts),
		param(P,Q)
	    do  [[[/*JTAG*/1,P,T],[/*JTAG*/1,Q,T]]]
	    )
	).

bb_of_operand([bb(_,_..Obase,_,_)|_], O0, I, K) :-
	O0 =< Obase, !, K = I.
bb_of_operand([_|BBs], O0, I, K) :-
	J is I+1,
	bb_of_operand(BBs, O0, J, K).

gen_before_precedences(Before, KL6) :-
	(   foreach([P,Q,Cond],Before),
	    fromto(KL1,KL2,KL3,[])
	do  cur_operand(P, _PI, PU, PTs0),
	    cur_operand(Q, QI, QU, QTs0),
	    delete_null(PTs0, PTs),
	    delete_null(QTs0, QTs),
	    gen_before_precedences(PU, QU, QI, PTs, QTs, P, Q, Cond, KL2, KL3)
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	(   foreach([Pred,Succ,N]-DisjBag,KL5),
	    foreach([Pred,Succ,N,DisjSet],KL6)
	do  kernel_set(DisjBag, [], DisjSet) % between given instructions, there is disjunction of before cond
	).

gen_before_precedences(0, 0, QI, [PT], _, _, _, Cond) --> !, % [def,def]
	{cur_temp(PT, _, [_|Rs], _)},
	(   foreach(R1,Rs),
	    param(PT,QI,Cond)
	do  {cur_operand(R1, I1, 1, _)},
	    (   foreach(Conj,Cond),
		param(R1,PT,I1,QI)
	    do  before_rule([p(R1) = t(PT)], I1, QI, Conj)
	    )
	).
gen_before_precedences(0, 1, _, [PT], QTs, _, Q, Cond) --> !, % [def,use]
	{cur_temp(PT, _, [_|Rs], _)},
	(   foreach(R1,Rs),
	    param(PT,Q,QTs,Cond)
	do  {cur_operand(R1, I1, 1, _)},
	    (   foreach(T2,QTs),
		param(R1,PT,Q,I1, Cond)
	    do  {cur_temp(T2, _, _, IT2)},
		(   foreach(Conj,Cond),
		    param(R1,PT,Q,T2,I1,IT2)
		do  before_rule([p(R1) = t(PT), p(Q) = t(T2)], I1, IT2, Conj)
		)
	    )
	).
gen_before_precedences(1, 0, QI, PTs, _, P, _, Cond) --> !, % [use,def]
	{PTs = [Key|_]},
	{cur_temp(Key, _, [_|Rs], _)},
	(   foreach(R1,Rs),
	    param(P,PTs,QI,Cond)
	do  {cur_operand(R1, I1, 1, R1Ts)},
	    {ord_intersection(PTs, R1Ts, PRTs)},
	    (   foreach(T1,PRTs),
		param(R1,P,I1,QI,Cond)
	    do  (   foreach(Conj,Cond),
		    param(R1,T1,P,I1,QI)
		do  before_rule([p(R1) = t(T1), p(P) = t(T1)], I1, QI, Conj)
		)
	    )
	).
gen_before_precedences(1, 1, _, PTs, QTs, P, Q, Cond) --> !, % [def,use]
	{PTs = [Key|_]},
	{cur_temp(Key, _, [_|Rs], _)},
	(   foreach(R1,Rs),
	    param(P,PTs,Q,QTs,Cond)
	do  {cur_operand(R1, I1, 1, R1Ts)},
	    {ord_intersection(PTs, R1Ts, PRTs)},
	    (   foreach(T2,QTs),
		param(R1,PRTs,P,Q,I1,Cond)
	    do  {cur_temp(T2, _, _, IT2)},
		(   foreach(T1,PRTs),
		    param(R1,P,I1,Q,T2,IT2,Cond)
		do  (   foreach(Conj,Cond),
			param(R1,T1,P,I1,Q,T2,IT2)
		    do  before_rule([p(R1) = t(T1), p(P) = t(T1), p(Q) = t(T2)], I1, IT2, Conj)
		    )
		)
	    )
	).

before_rule(Conj1, I, J, Json2) -->
	(   {I=:=J} -> []
	;   {cur_operation(I, pack, _, _, _)} -> []
	;   {conj_to_json(Conj1, Json1)},
	    {ord_union(Json1, Json2, Json)},
	    [[I, J, 0]-Json]
	).

cond_before_items(Before1, After1, ICall, Preassign, CSSet, DDGraph) -->
	{transpose_ugraph(DDGraph, DDGraphT)},
	{reachable(ICall, DDGraph, Reachable)},
	{reachable(ICall, DDGraphT, ReachableT)},
	{caller_saved_opnd(Reachable, CSOpnd)},
	{cond_before_filter(Before1, Before1b, Preassign, CSSet, [])},
	{cond_before_filter(After1, After1b, Preassign, CSSet, Reachable)},
	{keysort(Before1b, Before2)},
	{keyclumped(Before2, Before3)},
	{keysort(After1b, After2)},
	{keyclumped(After2, After3)},
	{   foreach(t(T1)-Disj1,Before3),
	    fromto(BA1,BA2,BA3,BA4),
	    param(ICall,Preassign,CSSet)
	do  temp_to_operand(T1, P1, []),
	    maybe_caller_saved(P1, Preassign, CSSet, YN1),
	    kernel_set(Disj1, [], Kern1),
	    (   YN1 = maybe
	    ->  BA2 = [t(T1)-before(Kern1)|BA3]
	    ;   BA2 = BA3
	    )
	},
	{   foreach(t(T2)-Disj2,After3),
	    fromto(BA4,BA5,BA6,[]),
	    param(ICall,Preassign,CSSet)
	do  kernel_set(Disj2, [], Kern2),
	    temp_to_operand(T2, P2, []),
	    (Kern2 = [Conj], member([/*JTAG*/1,PCS,T2],Conj) -> true ; PCS = P2),
	    maybe_caller_saved(P2, Preassign, CSSet, YN2),
	    maybe_caller_saved(PCS, Preassign, CSSet, YN3),
	    (   YN2 = maybe,
		YN3 = maybe
	    ->  BA5 = [t(T2)-after(Kern2)|BA6]
	    ;   BA5 = BA6
	    )
	},
	{keysort(BA1, BA7)},
	{keyclumped(BA7, BA8)},
	(   foreach(t(T3)-Clump,BA8),
	    param(CSOpnd, Reachable, ReachableT)
	do  {temp_to_operand(T3, TOpnd, [])},
	    {cur_temp(T3, _, _, ITemp)},
	    cond_before_item(Clump, ITemp, TOpnd, CSOpnd, Reachable, ReachableT)
	).

cond_before_item([before(Before)], ITemp, TOpnd, CSOpnd, Reachable, _) --> !,
	(   {ord_member(ITemp, Reachable)} % after is entailed
	->  (   foreach(Nogood,Before)
	    do  {assertz(more_nogood(Nogood))}
	    )
	;   [[TOpnd,CSOpnd,Before]]
	).
cond_before_item([after(After)], ITemp, TOpnd, CSOpnd, Reachable, ReachableT) --> !,
	(   {ord_member(ITemp, ReachableT)} % after is impossible
	->  (   foreach(Nogood,After)
	    do  {assertz(more_nogood(Nogood))}
	    )
	;   {ord_member(ITemp, Reachable)} -> [] % after is entailed
	;   [[CSOpnd,TOpnd,After]]
	).
cond_before_item([before(Before),after(After)], ITemp, TOpnd, CSOpnd, Reachable, ReachableT) --> !,
	(   {ord_member(ITemp, Reachable)} % after is entailed
	->  (   foreach(Nogood1,Before)
	    do  {assertz(more_nogood(Nogood1))}
	    )
	;   [[TOpnd,CSOpnd,Before]]
	),
	(   {ord_member(ITemp, ReachableT)} % after is impossible
	->  (   foreach(Nogood2,After)
	    do  {assertz(more_nogood(Nogood2))}
	    )
	;   {ord_member(ITemp, Reachable)} -> [] % after is entailed
	;   [[CSOpnd,TOpnd,After]]
	).

cond_before_filter(TempCond0, TempCond, Preassign, CSSet, Reachable) :-
	(   foreach(t(T1)-C1,TempCond0),
	    fromto(TempCond,TempCond1,TempCond2,[]),
	    param(Preassign,CSSet,Reachable)
	do  conj_to_json([cs(T1)|C1], Json),
	    (   cur_temp(T1, _, _, I),
		ord_member(I, Reachable)
	    ->  TempCond1 = [t(T1)-[]|TempCond2]
	    ;   member(p(P)=t(T1), C1),
		maybe_caller_saved(P, Preassign, CSSet, no(_))
	    ->  TempCond1 = TempCond2
	    ;   TempCond1 = [t(T1)-Json|TempCond2]
	    )
	).

caller_saved_opnd(Reachable, CSOpnd) :-
	member(I, Reachable),
	cur_operation(I, TypeName, _, [CSOpnd|_], _),
	member(TypeName, [kill,out]), !.

gen_fixed_precedences(AVL1) -->
	{avl_fetch(dist, AVL1, Dist)},
	{avl_fetch(dep, AVL1, Dep)},
	(   foreach(Dep2,Dep),
	    foreach(Dist2,Dist)
	do  (   foreach([I,J],Dep2),
		foreach(DistIJ,Dist2)
	    do			% constraint: fixed precedences
		{cur_operation(I, Tyi, InstructionsI, _, _)},
		{cur_operation(J, Tyj, InstructionsJ, _, _)},
		{gen_is_opt(InstructionsI, I, Cond1, Cond2)},
		{gen_is_opt(InstructionsJ, J, Cond2, [])},
		{   InstructionsI = [0|InstructionsI2] % assuming not null operation here
		->  DistIJ = [_|DistIJ2]
		;   InstructionsI = InstructionsI2,
		    DistIJ = DistIJ2
		},
		(   {Tyi = pack} -> []
		;   {Tyj = pack} -> []
		;   {Tyi = branch, sort(DistIJ2, [DIJK])} % (branch) -- fixed offset
		->  {NDIJK is -DIJK},
		    [[I, J, DIJK, [[]]], % [dis(I,J) #= DIJK]
		     [J, I, NDIJK, [[]]]] % [dis(I,J) #= DIJK]
		;   {sort(DistIJ2, [DIJK])}
		->  [[I, J, DIJK, [Cond1]]]
		    % [a(I) #/\ a(J) #=> dis(I,J) #>= DIJK]
		;   (   foreach(DIJK,DistIJ2),
			foreach(OIK,InstructionsI2),
			param(I,J,Cond1)
		    do  [[I, J, DIJK, [Cond3]]],
			{ord_union(Cond1, [[/*JTAG*/3,I,OIK]], Cond3)}
		      % [a(I) #/\ a(J) #/\ i(I)#=OIK #=> dis(I,J) #>= DIJK]
		    )
		)
	    )
	).

gen_is_opt([0|_], I) --> !, [[/*JTAG*/2,I]]. % a(I)
gen_is_opt(_, _) --> [].

% lat(i,op,opnd) represented as mapping opnd -> [op1-lat1,...,opn-latn]
compute_opnd2lat(AVL, Opnd2Lat) :-
	avl_fetch(lat, AVL, Lat),
	avl_fetch(operands, AVL, Operands),
	avl_fetch(instructions, AVL, Instructions),
	(   foreach(Opnds,Operands),
	    foreach(Ops,Instructions),
	    foreach(LatI,Lat),
	    fromto(KL1,KL2,KL4,[])
	do  (   LatI = [[]|_] -> KL2 = KL4
	    ;   transpose(LatI, LatIT),
		(   foreach(Opnd,Opnds),
		    foreach(LatITP, LatIT),
		    fromto(KL2,[Opnd-Row1|KL3],KL3,KL4),
		    param(Ops)
		do  (   foreach(O,Ops),
			foreach(L,LatITP),
			fromto(Row1,Row2,Row3,[])
		    do  (   O=:=0 -> Row2 = Row3 % null operation not useful here
			;   Row2 = [O-L|Row3]
			)
		    )
		)
	    )
	),
	ord_list_to_avl(KL1, Opnd2Lat).

% A temp is unsafe if its minlive is greater than its shortest latency
% i.e. if its live range can extend beyond its latest use
gen_unsafe_temp(AVL1, Opnd2Lat, UnsafeTemp1) :-
	avl_fetch(minlive, AVL1, MinLive),
	(   foreach(MinL,MinLive),
	    count(T1,0,_),
	    fromto(UnsafeTemp1,UnsafeTemp2,UnsafeTemp3,[]),
	    param(Opnd2Lat)
	do  cur_temp(T1, _, [Pd|Pus], D),
	    cur_operation(D, TypeName, _, _, _),
	    (   member(TypeName, [in,define,function])
	    ->  MinLat = MinL
	    ;   avl_fetch(Pd, Opnd2Lat, DLat),
		min_value(DLat, 10, L1),
		(   foreach(Pu,Pus),
		    fromto(10,L2,L3,L4),
		    param(Opnd2Lat)
		do  avl_fetch(Pu, Opnd2Lat, ULat),
		    min_value(ULat, L2, L3)
		),
		MinLat is L1+L4
	    ),
	    (   MinL > MinLat
	    ->  UnsafeTemp2 = [T1|UnsafeTemp3]
	    ;   UnsafeTemp2 = UnsafeTemp3
	    )
	).

copyrel_star(AVL, CopyRelStar) :-
	avl_fetch(strictly_congr, AVL, Congr),
	avl_fetch(copyrel, AVL, CopyRel), % sets of operands
	(   foreach(Class,CopyRel),
	    fromto(KL1,KL2,KL5,[])
	do  (   foreach(P,Class),
		fromto(KL2,KL3,KL4,KL5),
		param(Tag)
	    do  cur_operand(P, _, U, Ts),
		(   U =:= 1 -> KL3 = KL4
		;   delete_null(Ts, [T]),
		    KL3 = [T-Tag|KL4]
		)
	    )
	),
	keysort(KL1, KL6),
	ord_list_to_avl(KL6, Temp2Tag),
	(   foreach(Congr1,Congr),
	    param(Temp2Tag)
	do  (   foreach(P1,Congr1),
		param(Temp2Tag,Tag2)
	    do  cur_operand(P1, _, _, Ts1),
		delete_null(Ts1, [T1|_]),
		avl_fetch(T1, Temp2Tag, Tag2)
	    )
	),
	avl_to_list(Temp2Tag, KL7),
	(   foreach(K-V,KL7),
	    foreach(V-K,KL8)
	do  true
	),
	keysort(KL8, KL9),
	keyclumped(KL9, KL10),
	(   foreach(_-Clump,KL10),
	    foreach(Clump, CopyRelStar)
	do  true
	).

gen_congr(AVL0, AVL1) :-
	avl_fetch(aligned, AVL0, Aligned),
	avl_fetch(adist, AVL0, ADist),
	avl_fetch(preassign, AVL0, Preassign),
	avl_fetch(adjacent, AVL0, Adjacent),
	findall(Ts-P, cur_operand(P,_,_,Ts), KL1),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(Ts1-Ps,KL3), % find operands that have the same, singleton domain of temps
	    fromto(Edges1,Edges2,Edges4,Edges5)
	do  (   Ts1 = [_,_|_] -> Edges2 = Edges4
	    ;   Ps = [P1] -> Edges2 = Edges4
	    ;   Ps = [P1|Ps1],
		(   foreach(P2,Ps1),
		    fromto(Edges2,[P1-P2|Edges3],Edges3,Edges4),
		    param(P1)
		do  true
		)
	    )
	),
	(   foreach([P3,I3,P4,I4],Aligned), % find zero-aligned operands with the given insns
	    foreach(D,ADist),
	    fromto(Edges5,Edges6,Edges7,Edges8)
	do  cur_operand(P3, O3, _, _),
	    cur_operand(P4, O4, _, _),
	    cur_operation(O3, _, Is3, _, _),
	    cur_operation(O4, _, Is4, _, _),
	    (   D = 0,
		Is3 = [I3],
		Is4 = [I4]
	    ->  Edges6 = [P3-P4|Edges7]
	    ;   Edges6 = Edges7
	    )
	),
	(   foreach([Q,R],Preassign), % find operands that are preassigned to the same reg
	    foreach(R-Q,KL4)
	do  true
	),
	keysort(KL4, KL5),
	keyclumped(KL5, KL6),
	(   foreach(_-Qs,KL6),
	    fromto(Edges8,Edges9,Edges11,Edges12)
	do  (   Qs = [P5] -> Edges9 = Edges11
	    ;   Qs = [P5|Ps5],
		(   foreach(P6,Ps5),
		    fromto(Edges9,[P5-P6|Edges10],Edges10,Edges11),
		    param(P5)
		do  true
		)
	    )
	),
	(   foreach([P7,P8],Adjacent), % find adjacent, for sure connected operands
	    fromto(Edges12,Edges13,Edges14,[])
	do  cur_operand(P7, _, _, Ts7),
	    cur_operand(P8, _, _, Ts8),
	    (   Ts7 = [-1|_] -> Edges13 = Edges14
	    ;   Ts8 = [-1|_] -> Edges13 = Edges14
	    ;   Edges13 = [P7-P8|Edges14]
	    )
	),			% now build equivalence relation
	findall(P-_, cur_operand(P,_,_,_), KL7),
	(   foreach(K-V,KL7),
	    foreach(V-K,KL8)
	do  true
	),
	ord_list_to_avl(KL7, P2Key),
	(   foreach(A-B,Edges1),
	    param(P2Key)
	do  avl_fetch(A, P2Key, Key),
	    avl_fetch(B, P2Key, Key)
	),
	keysort(KL8, KL9),
	keyclumped(KL9, KL10),
	(   foreach(_-Con,KL10),
	    foreach(Con,Congr0)
	do  true
	),
	sort(Congr0, Congr),	% sort for readability and debugging
	avl_store(strictly_congr, AVL0, Congr, AVL1).

gen_modulo_2(AVL) :-
	retractall(cur_modulo_2(_,_)),
	avl_fetch(preassign, AVL, Preassign),
	avl_fetch(strictly_congr, AVL, CongrLH),
	(   foreach([Pa,Ra],Preassign),
	    foreach(Pa-Ra,KL9)
	do  true
	),
	list_to_avl(KL9, P2Ass),
	(   foreach(ConLH,CongrLH),
	    param(P2Ass)
	do  (   foreach(P1,ConLH),
		param(Mod,P2Ass)
	    do  cur_operand(P1, O, _, Ts),
		(   delete_null(Ts, [T0|_]),
		    cur_temp(T0, W0, _, _),
		    W0 > 1
		->  Mod = 0
		;   avl_fetch(P1, P2Ass, R)
		->  Mod is R /\ 1
		;   cur_operation(O, Ty1, _, Ps, _),
		    modulo_2_type(Ty1, Ps, P1, M)
		->  Mod = M
		;   true
		)
	    ),
	    (   foreach(P2,ConLH),
		param(Mod)
	    do  assertz(cur_modulo_2(P2,Mod))
	    )
	).

modulo_2_type(low, _, _, 0).
modulo_2_type(high, [U|_], P, Mod) :-
	Mod is P-U.
modulo_2_type(combine, [UL|_], P, Mod) :-
	Mod is (P-UL) /\ 1.

presolve_dur(AVL1, AVL3) :-
	avl_delete(dur, AVL1, Dur1, AVL2),
	avl_fetch(con, AVL2, Con),
	(   foreach(DurOp1,Dur1),
	    foreach(DurOp2,Dur2),
	    foreach(ConOp,Con)
	do  (   foreach(DurOpR1,DurOp1),
		foreach(DurOpR2,DurOp2),
		foreach(ConOpR,ConOp)
	    do  (ConOpR=0 -> DurOpR2 = 1 ; DurOpR2 = DurOpR1)
	    )
	),
	avl_store(dur, AVL2, Dur2, AVL3).

min_value(KL, Min1, Min4) :-
	(   foreach(_-V,KL),
	    fromto(Min1,Min2,Min3,Min4)
	do  Min3 is min(Min2,V)
	).

gen_data_precedences(AVL1, MAXI, Opnd2Lat, LastUse) -->
	{avl_fetch(minlive, AVL1, MinLive)},
	(   for(I,0,MAXI),
	    param(Opnd2Lat,MinLive, LastUse)
	do  {cur_operation(I, Tyi, _, QOpnds, _)},
	    (   foreach(Q,QOpnds),
		param(I,Tyi,Opnd2Lat,MinLive, LastUse)
	    do  {cur_operand(Q, _, 1, TempsQ)},
	        {delete_null(TempsQ, TempsQNoNull)},
		(   foreach(T,TempsQNoNull),
		    param(Q,I,Tyi,TempsQ,Opnd2Lat,MinLive, LastUse)
		do  {cur_temp(T, _, [P|_], D)},
		    {cur_operation(D, Tyd, _, _, _)},
		    (   {Tyi = pack}, % I is (pack)
			{nth0(T, MinLive, MinL)},
			{NMinL is -MinL}
		    ->  [[D, I, MinL, [[[/*JTAG*/2,D]]]], [I, D, NMinL, [[[/*JTAG*/2,D]]]]] % [dis(D,I) #= MinL]
		    ;   {Tyi = kill}, % I is (kill)
			{nth0(T, MinLive, MinL)},
			{NMinL is -MinL}
		    ->  [[D, I, MinL, [[]]], [I, D, NMinL, [[]]]] % [dis(D,I) #= MinL]
		    ;   {Tyd = define}, % D is (define) and last use
			{member(Q, LastUse)},
			{nth0(T, MinLive, MinL)},
			{NMinL is -MinL}
		    ->  [[D, I, MinL, [[]]], [I, D, NMinL, [[]]]] % [dis(D,I) #= MinL]
		    ;   {   TempsQ=[T|_] % primary temp - no assumption needed
			->  TCond = []
			;   TCond = [[/*JTAG*/1,Q,T]] % explicit assumption P=Q
			},
			gen_data_precedences1(D, I, P, Q, TCond, Opnd2Lat)
		    )
		)
	    )
	).

gen_data_precedences1(D, I, P, Q, TCond, Opnd2Lat) -->
	{avl_fetch(P, Opnd2Lat, LatP)},
	{avl_fetch(Q, Opnd2Lat, LatQ)},
	{   foreach(_-Lp,LatP),
	    foreach(Lp,Lps)
	do  true
	},
	{   foreach(_-Lq,LatQ),
	    foreach(Lq,Lqs)
	do  true
	},
	{min_member(Lp0, Lps)},
	{min_member(Lq0, Lqs)},
	{Lat0 is Lp0+Lq0},
	[[D, I, Lat0, [TCond]]],
	(   foreach(Op-Latp,LatP),
	    param(LatQ,D,I,TCond,Lat0)
	do  (   foreach(Oq-Latq,LatQ),
		param(D,I,Op,Latp,TCond,Lat0)
	    do  {Lat is Latp+Latq},
		(   {Lat=:=Lat0} -> []
		;   {ord_add_element(TCond, [/*JTAG*/3,D,Op], TCondp)},
		    {ord_add_element(TCondp, [/*JTAG*/3,I,Oq], TCondpq)},
		    [[D, I, Lat, [TCondpq]]]
		)
	    )
	).

gen_region_precedences(AVL, Precedences1) :-
	avl_fetch(ops, AVL, Ops),
	avl_fetch(precs, AVL, Precs),
	(   foreach(Op,Ops),
	    foreach(Last,Lasts)
	do  last(Op, Last)
	),
	(   foreach([A,B],Precs),
	    fromto(KL1,KL2,KL3,[]),
	    param(Lasts)
	do  cur_operation(A, Atype, _, _, _),
	    cur_operation(B, Btype, _, _, _),
	    (   edge_for_region(Atype, Btype, A, B, A1, B1)
	    ->  (member(L,Lasts), L >= B1 -> true),
		KL2 = [L-(A1-B1)|KL3]
	    ;   KL2 = KL3
	    )
	),
	keysort(KL1, KL4),
	keyclumped(KL4, KL5),
	(   foreach(_-Edges,KL5),
	    fromto(Precedences1,Precedences2,Precedences8,[]),
	    param(AVL)
	do  reduce_edges(Edges, EdgesH),
	    vertices_edges_to_ugraph([], EdgesH, Hasse),
	    dag_partition_nodes(Hasse, PartNodes, []),
	    dag_partitions(Hasse, PartNodes, Partitions),
	    (   foreach(I-J,EdgesH),
		fromto(Precedences2,Precedences3,Precedences4,Precedences5)
	    do  Precedences3 = [[I,J,1,[[]]] | Precedences4]
	    ),
	    (   foreach(Partition,Partitions),
		fromto(Precedences5,Precedences6,Precedences7,Precedences8),
		param(AVL)
	    do  dag_regions(AVL, Partition, Precedences6, Precedences7)
	    )
	).

edge_for_region(function, function, _, _, _, _) :- !, fail.
edge_for_region(define, _, _, _, _, _) :- !, fail.
edge_for_region(_, kill, _, _, _, _) :- !, fail.
edge_for_region(_, pack, _, _, _, _) :- !, fail.
edge_for_region(call, _, A, B, A1, B) :- A1 is A+1, A1<B, !.
edge_for_region(_, _, A, B, A, B).

dag_partition_nodes(G) -->
	{G = [Source-_|_]},
	(   foreach(V-Ns,G),
	    fromto(Source,Latest1,Latest2,_)
	do  ({V = Latest1} -> [V] ; []),
	    {   Ns = [] -> Latest2 = Latest1
	    ;   last(Ns, N),
		Latest2 is max(Latest1,N)
	    }
	).

dag_partitions(G, [_,_], [G]) :- !.
dag_partitions(G, [_|Sinks], Parts) :-
	edges(G, Edges),
	(   foreach(A-B,Edges),
	    foreach(L-(A-B),KL1),
	    param(Sinks)
	do  (member(L,Sinks), L >= B -> true)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(_-Edges2,KL3),
	    foreach(Part,Parts)
	do  vertices_edges_to_ugraph([], Edges2, Part)
	).

dag_regions(InputAVL, G, Precs1, Precs6) :-
	transpose_ugraph(G, H),
	empty_avl(AVL0),
	H = [_|H2],
	(   foreach(Vi-Psi,H2),
	    fromto(AVL0,AVL1,AVL4,_),
	    fromto(Precs1,Precs2,Precs5,Precs6),
	    param(G,H,InputAVL)
	do  (   fromto(H,[Vj-_|H3],H3,[Vi-_|_]),
		fromto(AVL1,AVL2,AVL3,AVL4),
		fromto(Precs2,Precs3,Precs4,Precs5),
		param(Vi,Psi,InputAVL,G,H)
	    do  r_of_predecessors(Vj, Psi, AVL2, RS1),
		sort(RS1, RS2),
		(   RS2 = []
		->  (   member(Vj, Psi)
		    ->  avl_store(r(Vj,Vi), AVL2, Vi, AVL3)
		    ;   AVL2 = AVL3
		    ),
		    Precs3 = Precs4
		;   RS2 = [R]
		->  avl_store(r(Vj,Vi), AVL2, R, AVL3),
		    Precs3 = Precs4		    
		;   avl_store(r(Vj,Vi), AVL2, Vi, AVL3),
		    emit_region(InputAVL, Vj, Vi, G, H, Precs3, Precs4)
		)
	    )
	).

r_of_predecessors(Vj, Ps, AVL, RS1) :-
	(   foreach(Vk,Ps),
	    fromto(RS1,RS2,RS3,[]),
	    param(AVL,Vj)
	do  (   avl_fetch(r(Vj,Vk), AVL, R)
	    ->  RS2 = [R|RS3]
	    ;   RS2 = RS3
	    )
	).

emit_region(AVL, Source, Sink, G, H, Ps0, Ps) :-
	avl_fetch(cap, AVL, Cap),
	avl_fetch(con, AVL, Con),
	reachable(Source, G, Down),
	reachable(Sink, H, Up),
	ord_intersection(Down, Up, Region),
	ord_subtract(Region, [Source,Sink], Inside),
	(   foreach(O,Inside),
	    foreach(Inc,Incs),
	    param(Con)
	do  op_min_inc(O, Con, Inc)
	),
	transpose(Incs, IncsT),
	region_lbs(IncsT, Cap, LBs),
	max_member(LB, LBs),
	longest_path(Region, G, LP),
	LB > LP, !,
	Ps0 = [[Source,Sink,LB,[[]]] | Ps],
	print_message(warning, region(Region, LB)), !.
emit_region(_, _, _, _, _, Ps, Ps).

longest_path(Region, G, LP) :-
	(   foreach(I,Region),
	    foreach(I-0,KL)
	do  true
	),
	ord_list_to_avl(KL, AVL1),
	(   foreach(J,Region),
	    fromto(AVL1,AVL2,AVL5,AVL6),
	    param(G)
	do  neighbors(J, G, Ns),
	    avl_fetch(J, AVL2, Jlen),
	    Update is Jlen+1,
	    (   foreach(N,Ns),
		fromto(AVL2,AVL3,AVL4,AVL5),
		param(Update)
	    do  (   avl_fetch(N, AVL3, Nlen),
		    Nlen < Update
		->  avl_store(N, AVL3, Update, AVL4)
		;   AVL3 = AVL4
		)
	    )
	),
	last(Region, Sink),
	avl_fetch(Sink, AVL6, LP).	

op_min_inc(O, Con, Inc) :-
	cur_operation(O, _, Insns, _, _),
	(   foreach(I,Insns),
	    foreach(Coni,Conis),
	    param(Con)
	do  nth0(I, Con, Coni)
	),
	transpose(Conis,ConisT),
	(   foreach(Is,ConisT),
	    foreach(Ismin,Inc)
	do  min_member(Ismin, Is)
	).

region_lbs(Vecs, Caps, LBs) :-
	(   foreach(Vec,Vecs),
	    foreach(Cap,Caps),
	    foreach(LB,LBs)
	do  sumlist(Vec, Sum),
	    LB is (Sum-1)//Cap+2
	).


gen_predecessors_successors(AVL, Predecessors1, Successors1) :-
	avl_fetch(precs, AVL, Precs),
	(   foreach([X,Y],Precs),
	    foreach(X-Y,Edges)
	do  true
	),
	reduce_edges(Edges, EdgesH),
	vertices_edges_to_ugraph([], EdgesH, Hasse),
	transpose_ugraph(Hasse, HasseT),
	(   foreach(Y1-Os1,HasseT),
	    fromto(Predecessors1,Predecessors2,Predecessors3,[]),
	    param(AVL)
	do  (   Os1 = [_,_|_],
		makespan(Os1, Span1, AVL),
		Span1 > 1
	    ->  Predecessors2 = [[Os1,Y1,Span1]|Predecessors3]
	    ;   Predecessors2 = Predecessors3
	    )
	),
	(   foreach(Y2-Os2,Hasse),
	    fromto(Successors1,Successors2,Successors3,[]),
	    param(AVL)
	do  (   Os2 = [_,_|_],
		makespan(Os2, Span2, AVL),
		Span2 > 1
	    ->  Successors2 = [[Y2,Os2,Span2]|Successors3]
	    ;   Successors2 = Successors3
	    )
	).

reduce_edges(Edges, EdgesH) :-
	vertices_edges_to_ugraph([], Edges, G1),
	transitive_closure(G1, G2),
	compose(G1, G2, G3),
	edges(G3, Edges3),
	ord_subtract(Edges, Edges3, EdgesH).

makespan(Os, Span, AVL) :-
	findall(Span, makespan1(Os,Span,AVL), [Span]).

makespan1(Os, Span, AVL) :-
	avl_fetch(dur, AVL, Dur),
	avl_fetch(con, AVL, Con),
	avl_fetch(cap, AVL, Cap),
	avl_fetch('R', AVL, ResSet),
	transpose(Dur, DurT),
	length(Os, N),
	Span in 0..N,
	(   foreach(I,Os),
	    fromto(KL1,[c(I)-CI,i(I)-II|KL2],KL5,[]),
	    fromto(Lab1,[CI,II|Lab2],Lab2,[]),
	    param(N,Span,ResSet)
	do  cur_operation(I, _, Insns, _, _),
	    list_to_fdset(Insns, InsnsFD),
	    CI in 0..N,
	    II in_set InsnsFD,
	    CI+1 #=< Span,
	    (   foreach(R,ResSet),
		fromto(KL2,KL3,KL4,KL5),
		param(I)
	    do  KL3 = [dur(R,i(I))-DRI,con(R,i(I))-CRI|KL4],
		DRI in 0..16,	% dummy
		CRI in 0..16	% dummy
	    )
	),
	keysort(KL1, KL6),
	ord_list_to_avl(KL6, Map),
	makespan_dur_con(Os, Dur, Con, ResSet, Constraints1, Constraints2),
	resources([], [Os], ResSet, DurT, Cap, Constraints2, []), % common.pl
	(   foreach(Constraint,Constraints1),
	    param(Map)
	do  interpret(Constraint, Map, _)
	),
	labeling([], [Span|Lab1]), !.

% ripped off from dur_con/5 in common.pl
makespan_dur_con(Os, Dur, Con, ResSet) -->
	{   foreach(O,Os),
	    foreach([i(O)|DursCons1],VarTuples),
	    param(ResSet)
	do  (   foreach(R,ResSet),
		fromto(DursCons1,[dur(R,i(O)),con(R,i(O))|DursCons2],DursCons2,[]),
		param(O)
	    do  true
	    )
	},
	{   foreach(Dur1,Dur),
	    foreach(Con1,Con),
	    foreach([I|DursCons3],RelTuples),
	    count(I,0,_)
	do  (   foreach(Durr,Dur1),
		foreach(Conr,Con1),
		fromto(DursCons3,[Durr,Conr|DursCons4],DursCons4,[])
	    do  true
	    )
	},
	[table(VarTuples,RelTuples)].

detect_cycles(Precedences1, Nogoods4) :-
	findall([J3,I3,NK3,[[]]], % (kill)/(define) fixed precedences
		(member([I3,J3,K3,[[]]],Precedences1), I3<J3, NK3 is -K3),
		FixRevPrecs1a),
	findall([J3,I3,NK3,[[[/*JTAG*/2,I3]]]], % (pack) fixed precedences
		(member([I3,J3,K3,[[[/*JTAG*/2,I3]]]],Precedences1), I3<J3, NK3 is -K3),
		FixRevPrecs1b),
	sort(FixRevPrecs1a, FixRevPrecs2a),
	sort(FixRevPrecs1b, FixRevPrecs2b),
	ord_union(FixRevPrecs2a, FixRevPrecs2b, FixRevPrecs2),
	ord_subtract(Precedences1, FixRevPrecs2, Precedences2),
	(   foreach([I,J,K,D],Precedences2),
	    foreach(I-J,Edges1),
	    foreach((I-J)-kd(K,D),KL1)
	do  true
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	ord_list_to_avl(KL3, Edge2P),
	sort(Edges1, Edges2),
	vertices_edges_to_ugraph([], Edges2, Graph),
	reduce(Graph, Reduced),
	vertices(Reduced, SCCs),
	(   foreach(SCC,SCCs),
	    fromto([],Nogoods2,Nogoods3,Nogoods4),
	    param(Edges2,Edge2P)
	do  (   SCC = [_] -> Nogoods2 = Nogoods3
	    ;   print_message(warning,format('cycle detected: ~w',[SCC])),
		break_cycle(SCC, Edges2, Edge2P, Nogoods2, Nogoods3)
	    )
	).

break_cycle(SCC, Edges, Edge2P, Nogoods1, Nogoods8) :-
	(   foreach(I-J,Edges),
	    fromto(Fwd1,Fwd2,Fwd3,[]),
	    fromto(Bwd1,Bwd2,Bwd3,[]),
	    param(SCC)
	do  (   I<J, ord_subset([I,J],SCC)
	    ->  Fwd2 = [I-J|Fwd3], Bwd2 = Bwd3
	    ;   I>J, ord_subset([J,I],SCC)
	    ->  Bwd2 = [I-J|Bwd3], Fwd2 = Fwd3
	    ;   Fwd2 = Fwd3, Bwd2 = Bwd3
	    )
	),
	(   foreach(J1-I1,Bwd1),
	    fromto(Nogoods1,Nogoods2,Nogoods7,Nogoods8),
	    param(Fwd1,Edge2P)
	do  (   print_message(warning, format(' breaking cycle from ~d to ~d and back',[I1,J1])),
		avl_fetch(J1-I1, Edge2P, KDs),
		(   foreach(kd(_Offset,Disj), KDs),
		    fromto(Nogoods2,Nogoods3,Nogoods6,Nogoods7),
		    param(Edge2P,Fwd1,I1,J1)
		do  (   foreach(Conj, Disj),
			fromto(Nogoods3,Nogoods4,Nogoods5,Nogoods6),
			param(Edge2P,Fwd1,I1,J1)
		    do  dfs(I1, J1, Fwd1, Conj, Edge2P, Nogoods4, Nogoods5)
		    )
		)
	    )
	).

dfs(_, _, _, Nogood,  _, Nogoods, Nogoods) :-
	subsumed_nogood(Nogood, Nogoods), !.
dfs(V, V, _, Nogood,  _, Nogoods1, Nogoods2) :- !,
	length(Nogood, Len),
	sos_add_new(Nogoods1, Nogood, Len, Nogoods2).
dfs(U, V, [R-_|Edges], Nogood, Edge2P, Nogoods1, Nogoods2) :-
	U > R, !,
	dfs(U, V, Edges, Nogood, Edge2P, Nogoods1, Nogoods2).
dfs(U, V, [U-S|Edges], Nogood, Edge2P, Nogoods1, Nogoods7) :-
	S =< V, !,
	avl_fetch(U-S, Edge2P, KDs),
	(   foreach(kd(_Offset,Disj), KDs),
	    fromto(Nogoods1,Nogoods2,Nogoods5,Nogoods6),
	    param(S,V,Edges,Nogood,Edge2P)
	do (   foreach(Conj, Disj),
	       fromto(Nogoods2,Nogoods3,Nogoods4,Nogoods5),
	       param(S,V,Edges,Nogood,Edge2P)
	   do  ord_union(Nogood, Conj, Nogood1),
	       dfs(S, V, Edges, Nogood1, Edge2P, Nogoods3, Nogoods4)
	   )
	),
	dfs(U, V, Edges, Nogood, Edge2P, Nogoods6, Nogoods7).
dfs(_, _, _, _, _, Nogoods, Nogoods).

subsumed_nogood([_,_,_,_,_,_,_|_], _). % allow max 6 literals
subsumed_nogood(Cand, _) :-
	suffix(Cand, [[/*JTAG*/1,P,_],[/*JTAG*/1,P,_]|_]). % contradictory assignment to opnd P
subsumed_nogood(Cand, Nogoods) :-
	length(Cand, CL),
	member(Nogood, Nogoods),
	length(Nogood, NL),
	ord_subsumes(Nogood, NL, Cand, CL).

sos_add_new([], X, _, [X]).
sos_add_new([X|Xs], Y, YL, Zs) :-
	length(X, XL),
	ord_subsumes(Y, YL, X, XL), !,	% SOS element subsumed by addition
	sos_add_new(Xs, Y, YL, Zs).
sos_add_new([X|Xs], Y, YL, [X|Zs]) :-
	sos_add_new(Xs, Y, YL, Zs).

ord_subsumes([], _, _, _).
ord_subsumes([X|Xs], XL, [Y|Ys], YL) :-
	compare(K, X, Y),
	ord_subsumes(K, XL, Ys, YL, X, Xs).

ord_subsumes(=, XL, Ys, YL, _, Xs) :-
	XL =< YL,
	ord_subsumes(Xs, XL, Ys, YL).
ord_subsumes(>, XL, Ys, YL, X, Xs) :-
	XL < YL,
	YL1 is YL-1,
	ord_subsumes([X|Xs], XL, Ys, YL1).

sort_by_length(Xs, KL2) :-
	(   foreach(X,Xs),
	    foreach(L-X,KL1)
	do  length(X, L)
	),
	keysort(KL1, KL2).

%%% VALID AI COMBINATIONS

gen_active_tables(AVL1, MAXI, MAXP, MAXR, MAXT,
		  AllJNogoods2, DiffTemps, DiffRegs, Dominates1, DomOps, LastUse, QuasiAdjacent) :-
	avl_fetch(class, AVL1, Class),
	avl_fetch(aligned, AVL1, Aligned),
	avl_fetch(adist, AVL1, ADist),
	avl_fetch(adjacent, AVL1, Adjacent),
	avl_fetch(atoms, AVL1, Atoms),
	avl_fetch(minlive, AVL1, MinLive),
	avl_fetch(domuses, AVL1, Domuses),
	avl_fetch(callersaved, AVL1, CallerSaved),
	avl_fetch(preassign, AVL1, Preassign),
	avl_fetch(strictly_congr, AVL1, Congr),
	avl_fetch(memassign, AVL1, MemAssign),
	avl_fetch(copyrel, AVL1, CopyRel),
	retractall(tmp_table(_,_,_)),
	retractall(copy_table(_,_)),
	core_constraints(CallerSaved, Atoms, Congr, Adjacent, Dominates1, Aligned, ADist, AllJNogoods2,
			 Preassign, DiffTemps, DiffRegs, DomOps, QuasiAdjacent,
			 MAXI, MAXP, Core, Core1),
	instruction_constraints(Atoms, Class, Domuses, LastUse, MAXR,
				Preassign, MinLive, MemAssign, Core1, []),
	gen_active_tables(Core, AllJNogoods2, MAXP, MAXR, MAXT, CopyRel),
	fail.
gen_active_tables(_, _, _, _, _, _, _, _, _, _, _, _).

gen_active_tables(Core, Nogoods, MAXP, MAXR, MAXT, CopyRel) :-
	Rlen is MAXT+2,
	length(Rs, Rlen),
	Rs = [-1|Rt],		% r(-1) = -1
	(   foreach(RT,Rs),	% build r(_) vector
	    count(I,-1,_),
	    foreach(r(I)-RT,KV1),
	    param(MAXR)
	do  RT in -1..MAXR
	),
	(   for(P1,0,MAXP),
	    foreach(r(t(P1))-RTP,KV2),
	    foreach(t(P1)-TP,KV3),
	    param(Rs,Rt)
	do  cur_operand(P1, _, U, Ts),
	    list_to_fdset(Ts, TPset),
	    TP in_set TPset,
	    (   U=1 -> element_r(TP, Rs, RTP)
	    ;   last(Ts, Tl),
		nth0(Tl, Rt, RTP)
	    )
	),
	append([KV1,KV2,KV3], KV),
	/***
	(   foreach(Tmp1,Tmp),	% per basic block
	    fromto(KL1,KL2,KL5,KL6) % PrincipalTemp-CopyTemp pairs
	do  (   foreach(T1,Tmp1),
		fromto(KL2,KL3,KL4,KL5)
	    do  cur_temp(T1, _, _, Insn),
		(   cur_operation(Insn, copy, [0|_], [P|_], [Q|_]),
		    cur_operand(P, _, 1, [-1,T0|_]),
		    cur_operand(Q, _, 0, [-1,T1])
		->  KL3 = [T0-T1|KL4]
		;   KL3 = KL4
		)
	    )
	),
	***/
	(   foreach(CopyRel1,CopyRel),
	    fromto(KL1,KL2,KL5,KL6)
	do  (   foreach(P,CopyRel1),
		count(J,1,_),
		fromto(Copies1,Copies2,Copies3,[])
	    do  (   cur_operand(P, Op, 0, PTs),
		    (J=1 -> true ; cur_operation(Op, _, [0|_], _, _)), % all Tail operands must be optional
		    delete_null(PTs, [T1|_])
		->  Copies2 = [T1|Copies3]
		;   Copies2 = Copies3
		)
	    ),
	    Copies1 = [Head|Tail],
	    (   foreach(Tl1,Tail),
		fromto(KL2,KL3,KL4,KL5),
		param(Head)
	    do  KL3 = [Head-Tl1|KL4]
	    )
	),
	ord_list_to_avl(KV, Avl0),
	(   foreach(Constraint,Core), % post all constraints
	    fromto(Avl0,Avl1,Avl2,Avl3) % symbolic var -> dvar,
	do  (   interpret(Constraint, Avl1, Avl2) -> true
	    ;   raise_exception(interpret(Constraint))
	    )
	),
	findall(Pair, binary_nogood_temp(Nogoods,Pair), KL6),
	sort(KL1, KL7),		% there can be copies
	keyclumped(KL7, KL8),	% Principal-CopyTemps pairs
	assert_active_tables(KL8, Avl3),
	assert_tmp_tables(KL8, Avl3).

binary_nogood_temp(Nogoods, (T1-T2)-T3) :-
	member([[/*JTAG*/1,P1,_],[/*JTAG*/1,P2,_]], Nogoods),
	cur_operand(P1, _, _, Ts10),
	cur_operand(P2, _, _, Ts20),
	delete_null(Ts10, [T1|Ts1]),
	delete_null(Ts20, [T2|Ts2]),
	T1 =\= T2,
	ord_union(Ts1, Ts2, Ts3),
	member(T3, Ts3).

/***
%% breaking gross symmetry among memory locations

%% ROBERTO, 23/11/2015: disabled as it sometimes removes non-dominated
%% solutions. Should be rethought so that congruent temporaries are potentially
%% assigned to the same memory location (see epic.huffman.free_tree_nodes for an
%% example where memassign disallows a good solution).
memassign(AVL, MemAssign) :-
	avl_fetch(infinite, AVL, Inf),
	avl_fetch(range, AVL, Range),
	avl_fetch(atoms, AVL, Atoms),
	avl_fetch(class, AVL, Class),
	avl_fetch(operands, AVL, Operands),
	nth1(Infth, Inf, true), !,	% deal with one range only
	nth1(Infth, Range, [IMin,IMax]),
	(   count(C,0,_),
	    foreach(Atom,Atoms),
	    fromto(IAtoms1,IAtoms2,IAtoms3,[]),
	    param(IMin,IMax)
	do  Atom = [Afirst|_],
	    last(Atom, Alast),
	    (Afirst>=IMin, Alast=<IMax -> IAtoms2 = [C|IAtoms3] ; IAtoms2 = IAtoms3)
	),
	(   foreach(ClassO,Class),
	    foreach(OperandsO,Operands),
	    fromto(MemTs1,MemTs2,MemTs7,[]),
	    param(IAtoms1)
	do  (   foreach(ClassOI,ClassO),
		fromto(MemTs2,MemTs3,MemTs6,MemTs7),
		param(IAtoms1,OperandsO)
	    do  (   foreach(A,ClassOI),
		    foreach(P,OperandsO),
		    fromto(MemTs3,MemTs4,MemTs5,MemTs6),
		    param(IAtoms1)
		do  (   ord_member(A, IAtoms1),
			cur_operand(P, _, 0, [-1,T]),
			cur_temp(T, W, _, _),
			NW is -W
		    ->  MemTs4 = [NW-T|MemTs5]
		    ;   MemTs4 = MemTs5
		    )
		)
	    )
	),
	sort(MemTs1, MemTs8),
	(   foreach(NW1-T1,MemTs8),
	    foreach([T1,Cur1],MemAssign),
	    fromto(IMin,Cur1,Cur2,_)
	do  Cur2 is Cur1-NW1
	).
***/

callee_saved_spill(AVL, CopyRelStar, Spill) :-
	avl_fetch(calleesaved, AVL, CalleeSaved),
	avl_fetch(preassign, AVL, Preassign),
	callee_saved_temps(CalleeSaved, Preassign, Temps),
	(   foreach(T,Temps),
	    foreach([I1|I2s1],Spill),
	    param(CopyRelStar)
	do  memberchk([T,T1|Ts], CopyRelStar),
	    cur_temp(T1, _, _, I1),
	    (   foreach(T2,Ts),
		fromto(I2s1,I2s2,I2s3,[]),
		param(I1)
	    do  cur_temp(T2, _, _, I2),
		cur_operation(I2, Type, _, _, _),
		(Type = copy -> I2s2 = [I2|I2s3] ; I2s2 = I2s3)
	    )
	).

callee_saved_temps(CalleeSaved, Preassign, Temps1) :-
	preassignment_clash_set(Preassign, ClashSet),
	ord_subtract(CalleeSaved, ClashSet, SafeSet),
	(   foreach([P,R],Preassign),
	    fromto(Temps1,Temps2,Temps3,[]),
	    param(SafeSet)
	do  (   cur_operand(P, 0, 0, [T]),
		cur_temp(T, W, _, _),
		(   for(Reg,R,R+W-1),
		    foreach(Reg,RegSet)
		do  true
		),
		ord_subset(RegSet, SafeSet)
	    ->  Temps2 = [T|Temps3]
	    ;   Temps2 = Temps3
	    )
	), !.
callee_saved_temps(_, _, []).

preassignment_clash_set(Preassign, ClashSet) :-
	(   foreach([P,R],Preassign),
	    fromto(CS1,CS2,CS4,[])
	do  cur_operand(P, Op, _, [T|_]),
	    cur_operation(Op, Ty, _, _, _),
	    cur_temp(T, W, _, _),
	    (   Ty = in -> CS2 = CS4
	    ;   Ty = out -> CS2 = CS4
	    ;   (   for(Reg,R,R+W-1),
		    fromto(CS2,[Reg|CS3],CS3,CS4)
		do  true
		)
	    )
	),
	sort(CS1, ClashSet).

assert_tmp_tables(Principal2Tmps, Avl) :-
	principal_used_by(Prince2Users),
	% sort them by increasing size
	(   foreach(T2-C2,Principal2Tmps),
	    foreach(N2-(T2-C2),KL1),
	    param(Principal2Tmps)
	do  length(C2, N2)
	),
	keysort(KL1, KL2),
	member(_-(Pivot2-Clump), KL2),
	(   foreach(Tj,Clump),
	    foreach(I,CopySet),
	    foreach(A,As),
	    foreach(O,Os),
	    param(Avl)
	do  cur_temp(Tj, _, _, I),
	    avl_fetch(a(I), Avl, A),
	    avl_fetch(i(I), Avl, O)
	),
	avl_fetch(Pivot2, Prince2Users, Users),
	(   foreach(P,Users),
	    fromto(Ps1,Ps2,Ps3,[]),
	    fromto(Ts1,Ts2,Ts3,[]),
	    param(Avl)
	do  (   cur_operand(P, _, _, [-1,_]) % exclude opt copies of p___{-, t___}
	    ->  Ps2 = Ps3,
		Ts2 = Ts3
	    ;   Ps2 = [P|Ps3],
		Ts2 = [T|Ts3],
		avl_fetch(t(P), Avl, T)
	    )
	),
	append(As, Ts1, ATs),
	findall(As-Ts1, (labeling([],ATs), once(labeling([],Os))), Table1),
	trim_tmp_table(Table1, Pivot2, Table2),
	assertz(tmp_table(CopySet, Ps1, Table2)),
	% detect_neq(CopySet, Ps1, Table2),
	fail.
assert_tmp_tables(_, _).

detect_neq(As, Ps, Table) :-
	transpose(Table, Transpose),
	length(As, NA),
	length(Bs, NA),
	append(Bs, Rows, Transpose),
	(   foreach(P,Ps),
	    foreach(Row,Rows)
	do  sort(Row, Set),
	    cur_operand(P, _, _, Dom),
	    ord_subtract(Dom, Set, Notin),
	    (   Notin = [] -> true
	    ;   print_message(error, notin(P,Notin))
	    )
	).

% tmp_table row i subsumes row j if:
% - the A pattern is the same
% - the principal temp occurs in the same positions
% - the T pattern is a permutation
trim_tmp_table(Table1, Principal, Table2) :-
	keyclumped(Table1, KL1),
	(   foreach(As-Clump1,KL1),
	    fromto(Table2,Table3,Table5,[]),
	    param(Principal)
	do  trim_clump(Clump1, Principal, [], Clump2),
	    (   foreach(Ts,Clump2),
		fromto(Table3,[AsTs|Table4],Table4,Table5),
		param(As)
	    do  append(As, Ts, AsTs)
	    )
	).


trim_clump([], _, Stack, Clump) :-
	reverse(Stack, Clump).
trim_clump([X|Queue], Principal, Stack, Clump) :-
	member(Y, Stack),
	tmp_subsumes(Y, X, Principal), !,
	trim_clump(Queue, Principal, Stack, Clump).
trim_clump([X|Queue], Principal, Stack, Clump) :-
	trim_clump(Queue, Principal, [X|Stack], Clump).

tmp_subsumes(Ts1, Ts2, P) :-
	(   foreach(T1,Ts1),
	    foreach(T2,Ts2),
	    foreach(T1-0,Ps1),
	    foreach(T2-0,Ps2),
	    param(P)
	do  (T1=:=P -> T2=:=P ; T2=\=P)
	),
	keysort(Ps1, Pairs),
	keysort(Ps2, Pairs).

principal_used_by(Prince2Users) :-
	findall(T0-P,
		(cur_operand(P, _, 1, Ts), delete_null(Ts, [T0|_])),
		KL1),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	ord_list_to_avl(KL3, Prince2Users).

assert_active_tables(Principal2Tmps, Avl) :-
	member(_-Clump, Principal2Tmps),
	(   foreach(T,Clump),
	    foreach(I,CopySet),
	    foreach(X,Xs),
	    foreach(O,Os),
	    param(Avl)
	do  cur_temp(T, _, _, I),
	    avl_fetch(a(I), Avl, X),
	    avl_fetch(i(I), Avl, O)
	),
	findall(Xs, (labeling([],Xs), once(labeling([],Os))), Table),
	decompose_copy_table(CopySet, Table, CopySet1, Table1),
	assertz(copy_table(CopySet1,Table1)),
	fail.
assert_active_tables(_, _).

% decompose_copy_table(CopySet, Table, CopySet, Table).
% case 1: the 1st variable is irrelevant, very common
decompose_copy_table([_|CopySet1], Table0, CopySet, Table) :-
	length(Table0, Nrows0),
	Nrows is Nrows0>>1,
	Nrows0 /\ 1 =:= 0,
	prefix_length(Table0, Prefix, Nrows),
	append(Prefix, Suffix, Table0),
	(   foreach([0|Row],Prefix),
	    foreach([1|Row],Suffix),
	    foreach(Row,Table1)
	do  true
	), !,
	decompose_copy_table(CopySet1, Table1, CopySet, Table).
% case 2: the ith var is stuck at one
decompose_copy_table(CopySet0, Table0, CopySet, Table) :-
	transpose(Table0, TableT0),
	nth0(I, TableT0, Col, TableT),
	nonmember(0, Col), !,	% found stuck-at-one
	nth0(I, CopySet0, Ai, CopySet1),
	(   CopySet = [Ai], Table = [[1]]
	;   transpose(TableT, Table1),
	    decompose_copy_table(CopySet1, Table1, CopySet, Table)
	).
% case 3: the last var implies the other vars, where >= 3 vars
decompose_copy_table(CopySet0, Table0, CopySet, Table) :-
	CopySet0 = [_,_,_|_],
	last(Table0, Last),
	nonmember(0, Last),
	transpose(Table0, TableT),
	last(TableT, LastT),
	sumlist(LastT, 1), !,
	last(CopySet0, Pivot),
	(   CopySet = [Var,Pivot], Table = [[0,0],[1,0],[1,1]],
	    member(Var, CopySet0),
	    Var =\= Pivot
	;   append(CopySet1, [_], CopySet0),
	    append(TableT1, [_], TableT),
	    transpose(TableT1, Table1),
	    append(Table2, [_], Table1),
	    decompose_copy_table(CopySet1, Table2, CopySet, Table)
	).
% case 4: otherwise
decompose_copy_table([A|As], Table, [A|As], Table).

filter_active_tables(ActiveTables1, ActiveTables2, Dominates1, Dominates9) :-
	(   foreach([Set,Table],ActiveTables1),
	    fromto(ActiveTables2,ActiveTables3,ActiveTables4,[]),
	    fromto(Forced1,Forced2,Forced3,[]),
	    fromto(Dom1,Dom2,Dom3,[])
	do  (   Set = [AI]
	    ->  ActiveTables3 = [[Set,Table]|ActiveTables4],
		Forced2 = [AI|Forced3],
		Dom2 = Dom3
	    ;   Table = [[0,0],[1,0],[1,1]]
	    ->  ActiveTables3 = ActiveTables4,
		Forced2 = Forced3,
		Dom2 = [Set|Dom3]
	    ;   ActiveTables3 = [[Set,Table]|ActiveTables4],
		Forced2 = Forced3,
		Dom2 = Dom3
	    )
	),
	sort(Dom1, DomSet),
	(   foreach(DomItem,Dominates1),
	    fromto(Dominates2,Dominates3,Dominates4,Dominates5),
	    param(Forced1,DomSet)
	do  DomItem = [I,J,_,_],
	    (   member(I, Forced1)
	    ->  Dominates3 = Dominates4
	    ;   ord_member([I,J], DomSet)
	    ->  Dominates3 = Dominates4
	    ;   Dominates3 = [DomItem|Dominates4]
	    )
	),
	transitive_reduction(DomSet, DomSetReduced),
	(   foreach([I1,J1],DomSetReduced),
	    fromto(Dominates5,[[I1,J1,[],[]]|Dominates6],Dominates6,[])
	do  true
	),
	sort(Dominates2, Dominates9).

activator_active_tables(AVL1, CT, CT1) :-
	avl_fetch(activators, AVL1, Activators),
	avl_fetch(instructions, AVL1, Instructions),
	avl_fetch(ops, AVL1, Ops),
	(   foreach(Ibag,Activators),
	    foreach(Insn,Instructions),
	    foreach(Iset-O,KL1),
	    fromto(Insns1,Insns2,Insns3,[]),
	    count(O,0,_)
	do  (   Insn = [0|_] -> Insns2 = Insns3
	    ;   Insns2 = [Insn|Insns3]
	    ),
	    sort(Ibag, Iset)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	sort(Insns1, InsnSet),
	(   foreach(Iset1-Os1,KL3),
	    fromto(CT1,CT2,CT5,CT),
	    param(InsnSet,Ops)
	do  (   Iset1 \== [],
		member(Is, InsnSet),
		ord_subset(Is, Iset1)
	    ->  (   foreach(OpsBB,Ops),
		    fromto(Os1,Os2,Os3,[]),
		    fromto(CT2,CT3,CT4,CT5)
		do  ord_intersection(OpsBB, Os2, OsBB, Os3),
		    (   OsBB \== []
		    ->  CT3 = [[OsBB,[Ones]]|CT4],
			(   foreach(_,OsBB),
			    foreach(1,Ones)
			do  true
			)
		    ;   CT3 = CT4
		    )
		)
	    ;   CT2 = CT5
	    )
	).

optional_min_active_tables(AVL1, ActiveTables1, OptionalMin) :-
	avl_fetch(ops, AVL1, Ops),
	(   foreach(Ops1,Ops),
	    foreach(OptionalMin4,OptionalMin),
	    fromto(ActiveTables1,ActiveTables2,ActiveTables3,_)
	do  (   foreach(Item,ActiveTables2),
		fromto(ActiveTables3,Rest2,Rest3,[]),
		fromto(0,OptionalMin2,OptionalMin3,OptionalMin4),
		fromto(Ops1,Ops2,Ops3,_)
	    do  Item = [Set,Rows],
		(   ord_intersection(Set,Ops2,Set,Ops3) % Set is subset of Ops2, Ops3 = Ops2\Set
		->  (   foreach(Row,Rows),
			foreach(Sum,Sums)
		    do  sumlist(Row, Sum)
		    ),
		    min_member(Min, Sums),
		    OptionalMin3 is OptionalMin2+Min,
		    Rest2 = Rest3
		;   OptionalMin3 is OptionalMin2,
		    Rest2 = [Item|Rest3],
		    Ops2 = Ops3
		)
	    )
	).

interpret(#\P, Avl0, Avl) :- !,
	interpret_reif(P, 0, Avl0, Avl).
interpret(Key in Domain, Avl0, Avl) :- !,
	dic_lookup(Key, Avl0, Val, Avl),
	Val in Domain.
interpret(c(_) #>= _ #<=> _, Avl, Avl) :- !. % relaxed away
interpret(ld(_) #>= _, Avl, Avl) :- !. % relaxed away
interpret(le(_) #>= _, Avl, Avl) :- !. % relaxed away
interpret(le(_) #= _, Avl, Avl) :- !. % relaxed away
interpret(minimum(_,_), Avl, Avl) :- !. % relaxed away
interpret(X #= Y + Offset, Avl0, Avl) :- !,
	dic_lookup(X, Avl0, V, Avl1),
	dic_lookup(Y, Avl1, W, Avl),
	V #= W + Offset.
interpret(X #= Y, Avl0, Avl) :- !,
	dic_lookup(X, Avl0, V, Avl1),
	dic_lookup(Y, Avl1, V, Avl).
interpret(X #\= Y, Avl0, Avl) :- !,
	dic_lookup(X, Avl0, V, Avl1),
	dic_lookup(Y, Avl1, W, Avl),
	V #\= W.
interpret(X #< Y, Avl0, Avl) :- !,
	dic_lookup(X, Avl0, V, Avl1),
	dic_lookup(Y, Avl1, W, Avl),
	V #< W.
interpret(P #<=> Q, Avl0, Avl) :- !,
	interpret_reif(P, B, Avl0, Avl1),
	interpret_reif(Q, B, Avl1, Avl).
interpret(P #=> Q, Avl0, Avl) :- !,
	interpret_reif(P, B, Avl0, Avl1),
	interpret_reif(Q, C, Avl1, Avl),
	B #=> C.
interpret(P #\/ Q, Avl0, Avl) :- !,
	interpret_reif(P, B, Avl0, Avl1),
	interpret_reif(Q, C, Avl1, Avl),
	B #\/ C.
interpret(disjoint1(Items1), Avl0, Avl) :- !,
	(   foreach(item(Sym,W),Items1),
	    foreach(item(Var,W),Items2),
	    fromto(Avl0,Avl1,Avl2,Avl)
	do  dic_lookup(Sym, Avl1, Var, Avl2)
	),
	disjoint1(Items2).
interpret(all_different(Syms), Avl0, Avl) :- !,
	(   foreach(Sym,Syms),
	    foreach(X,Xs),
	    fromto(Avl0,Avl1,Avl2,Avl)
	do  dic_lookup(Sym, Avl1, X, Avl2)
	),
	all_distinct(Xs).
interpret(table(Varss1,Extension), Avl, Avl) :- !,
	(   foreach(Vars1,Varss1),
	    foreach(Vars2,Varss2),
	    param(Avl)
	do  (   foreach(V1,Vars1),
		foreach(V2,Vars2),
		param(Avl)
	    do  dic_lookup(V1, Avl, V2, _)
	    )
	),
	table(Varss2, Extension).
interpret(cumulative(Tasks1,Cap,_), Avl, Avl) :- !,
	(   foreach(task(S1,D1,H1),Tasks1),
	    foreach(task(S2,D2,_,H2,1),Tasks2),
	    param(Avl)
	do  dic_lookup(S1, Avl, S2, _),
	    dic_lookup(D1, Avl, D2, _),
	    dic_lookup(H1, Avl, H2, _)
	),
	cumulative(Tasks2, [limit(Cap)]).
% interpret(all_eq_or_joker(ITEs,RKey), Avl, Avl) :-
% 	print_message(warning,all_eq_or_joker(ITEs,RKey)),
% 	avl_fetch(RKey, Avl, RVar),
% 	(   foreach(if(TKey#=T,RTKey,-1),ITEs),
% 	    foreach(ITEVar,ITEVars),
% 	    param(Avl)
% 	do  avl_fetch(TKey, Avl, TVar),
% 	    avl_fetch(RTKey, Avl, RTVar),
% 	    TVar #= T #<=> TVarEq,
% 	    if_then_else(TVarEq, RTVar, -1, ITEVar)
% 	),
% 	element(_, ITEVars, RVar).
% interpret(value_precede_chain(SymmValues, AllValues, Vars), Avl0, Avl) :- !,
% 	(   foreach(Var,Vars),
% 	    foreach(X,Xs),
% 	    fromto(Avl0,Avl1,Avl2,Avl)
% 	do  dic_lookup(Var, Avl1, X, Avl2)
% 	),
% 	value_precede_chain(SymmValues, AllValues, Xs).
% interpret(temp_cardinality(TempSyms,ValueASymList), Avl0, Avl) :- !,
% 	(   foreach(Value-ASym,ValueASymList),
% 	    foreach(Value-C,ValueCList),
% 	    fromto(Avl0,Avl1,Avl2,Avl3)
% 	do  dic_lookup(ASym, Avl1, A, Avl2),
% 	    temp_count(A, C)
% 	),
% 	(   foreach(TSym,TempSyms),
% 	    foreach(T,TempVars),
% 	    fromto(Avl3,Avl4,Avl5,Avl)
% 	do  dic_lookup(TSym, Avl4, T, Avl5)
% 	),
% 	global_cardinality(TempVars, [-1-_|ValueCList]).

temp_count(Ai, Count) +:
	Count in ((1..max(Ai)) ? (1..sup)) \/
	         ((min(Ai)..0) ? (0..0)),
	Ai    in ((1..max(Count)) ? (1..1)) \/
	         ((min(Count)..0) ? (0..0)).

interpret_reif(a(I), B, Avl0, Avl) :- !,
	dic_lookup(a(I), Avl0, B, Avl).
interpret_reif(#\a(I), C, Avl0, Avl) :- !,
	dic_lookup(a(I), Avl0, B, Avl),
	B #\ C.
interpret_reif(Sym in R, B, Avl0, Avl) :- !,
	dic_lookup(Sym, Avl0, Var, Avl),
	Var in R #<=> B.
interpret_reif(Sym1 #= Sym2, B, Avl0, Avl) :- !,
	dic_lookup(Sym1, Avl0, V1, Avl1),
	dic_lookup(Sym2, Avl1, V2, Avl),
	V1 #= V2 #<=> B.
interpret_reif(Sym1 #\= Sym2, B, Avl0, Avl) :- !,
	dic_lookup(Sym1, Avl0, V1, Avl1),
	dic_lookup(Sym2, Avl1, V2, Avl),
	V1 #\= V2 #<=> B.
interpret_reif(Sym1 #=< Sym2, B, Avl0, Avl) :- !,
	dic_lookup(Sym1, Avl0, V1, Avl1),
	dic_lookup(Sym2, Avl1, V2, Avl),
	V1 #=< V2 #<=> B.
interpret_reif(Sym1 #>= Sym2, B, Avl0, Avl) :- !,
	dic_lookup(Sym1, Avl0, V1, Avl1),
	dic_lookup(Sym2, Avl1, V2, Avl),
	V1 #>= V2 #<=> B.
interpret_reif(P #\/ Q, B, Avl0, Avl) :- !,
	interpret_reif(P, B1, Avl0, Avl1),
	interpret_reif(Q, B2, Avl1, Avl),
	B1 #\/ B2 #<=> B.
interpret_reif(P #/\ Q, B, Avl0, Avl) :- !,
	interpret_reif(P, B1, Avl0, Avl1),
	interpret_reif(Q, B2, Avl1, Avl),
	B1 #/\ B2 #<=> B.
interpret_reif(ls(_) #< le(_), B, Avl, Avl) :- !, % ignore these
	B in 0..1.

dic_lookup(Key, AVL, Val, AVL) :- integer(Key), !,
	Val = Key.
dic_lookup(Key#>=0, AVL, Val, AVL) :- !,
	avl_fetch(Key, AVL, Var),
	Var#>0 #<=> Val.
dic_lookup(Key, AVL0, Val, AVL) :-
	avl_fetch(Key, AVL0, Val), !,
	AVL = AVL0.
dic_lookup(Key, AVL0, Val, AVL) :-
	avl_store(Key, AVL0, Val, AVL).

