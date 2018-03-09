/*
 *  Main authors:
 *    Mats Carlsson <mats.carlsson@ri.se>
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
 *
 *  Contributing authors:
 *    Erik Ekstrom <eeks@sics.se>
 *    Noric Couderc <noric@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, RISE SICS AB
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
 */


#include "presolve.hpp"

void presolve(Parameters & input, PresolverOptions & options) {

  // Global timer
  Support::Timer t;
  t.start();

  // Timer for each individual pass
  Support::Timer t0;
  t0.start();

  PresolverAsserts PA; // for holding some "global variables"

  // Precompute data-dependency graphs and their transposes
  vector<edge> noedges;
  Digraph emptyg = Digraph(noedges);
  init_vector(PA.dd_graph, input.B.size(), emptyg);
  init_vector(PA.dd_graph_transpose, input.B.size(), emptyg);
  for(block b : input.B) {
    Digraph dd = dd_graph(input, b);
    PA.dd_graph[b] = dd;
    PA.dd_graph_transpose[b] = dd.transpose();
  }

  // Precompute set of rematerialized temporaries
  for(const vector<operand>& copy_rels : input.copyrel)  {
    operand pprim = copy_rels[0];
    for(operand p : copy_rels) {
      operation o = input.oper[p];
      vector<temporary> ts = input.temps[p];
      if(p != pprim && !input.use[p] && ts[0] == NULL_TEMPORARY && input.type[o] != COPY && !input.delimiter[o]) {
	vector_insert(PA.remat, ts[1]);
      }
    }
  }

  // Clear all fields that will be generated.
  vector<PresolverBeforeJSON> before_ref = input.before;
  vector<UnisonConstraintExpr> nogoods_ref = input.nogoods;
  vector<vector<operand> > congr_ref = input.strictly_congr;
  vector<operand> last_use_ref = input.last_use;
  vector<vector<operand> > difftemps_ref = input.difftemps;
  vector<vector<operand> > diffregs_ref = input.diffregs;
  vector<vector<vector<int> > > domops_ref = input.domops;
  vector<PresolverAcrossJSON> across_ref = input.across;
  vector<PresolverSetAcross> set_across_ref = input.set_across;
  vector<PresolverBeforeJSON> before2_ref = input.before2;
  vector<UnisonConstraintExpr> nogoods2_ref = input.nogoods2;
  vector<UnisonConstraintExpr> precedences_ref = input.precedences;
  vector<UnisonConstraintExpr> precedences2_ref = input.precedences2;
  vector<vector<operation>> calleesaved_spill_ref = input.calleesaved_spill;
  vector<PresolverValuePrecedeChain> value_precede_chains_ref = input.value_precede_chains;
  vector<PresolverPred> predecessors_ref = input.predecessors;
  vector<PresolverSucc> successors_ref = input.successors;
  vector<vector<operand> > quasi_adjacent_ref = input.quasi_adjacent;
  vector<vector<vector<int> > > long_latency_index_ref = input.long_latency_index;
  vector<vector<operand> > long_latency_def_use_ref = input.long_latency_def_use;
  vector<vector<resource> > subsumed_resources_ref = input.subsumed_resources;
  vector<PresolverActiveTable> active_tables_ref = input.active_tables;
  vector<PresolverCopyTmpTable> tmp_tables_ref = input.tmp_tables;
  vector<PresolverDominates> dominates_ref = input.dominates;
  vector<int> optional_min_ref = input.optional_min;

  input.before.clear();
  input.nogoods.clear();
  input.strictly_congr.clear();
  input.last_use.clear();
  input.difftemps.clear();
  input.diffregs.clear();
  input.domops.clear();
  input.across.clear();
  input.set_across.clear();
  input.before2.clear();
  input.nogoods2.clear();
  input.precedences.clear();
  input.precedences2.clear();
  input.calleesaved_spill.clear();
  input.value_precede_chains.clear();
  input.predecessors.clear();
  input.successors.clear();
  input.quasi_adjacent.clear();
  input.long_latency_index.clear();
  input.long_latency_def_use.clear();
  input.active_tables.clear();
  input.tmp_tables.clear();
  input.dominates.clear();

  // Abort if the problem is trivially unfeasible
  
  t0.start();
  ModelOptions moptions;
  GlobalModel * base = new GlobalModel(&input, &moptions, IPL_DOM);
  base->post_upper_bound(input.maxf);
  Gecode::SpaceStatus ss1 = base->status();
  delete base;
  if (ss1 == SS_FAILED) {
    if (options.verbose())
      cerr << pre()
           << "proven absence of solutions with cost less or equal than "
           << show(input.maxf, ", ", "", "{}") << endl;
    // ensure infeasible MiniZinc model
    input.nogoods.push_back(UnisonConstraintExpr(OR_EXPR, {}, {}));
    return;
  }

  if (timeout(t, options, "trivial unfeasibility", t0))
    return;

  // 1: JSON.strictly_congr <- GENCONGR()

  t0.start();
  gen_congr(input);
  if (timeout(t, options, "congr", t0))
    return;

  // 2: JSON.last_use <- LASTUSE()

  t0.start();
  last_use(PA, input);
  if (timeout(t, options, "last_use", t0))
    return;

  // 4: <JSON.before, Nogoods0> <- BEFOREVSNOGOODS(GENBEFORE())

  t0.start();
  vector<presolver_conj> Nogoods;
  BeforePresolver::presolve(input, Nogoods);
  if (timeout(t, options, "before", t0))
    return;

  // 5: <Alldiffs, Nogoods1> <- GENINFEASIBLE()

  t0.start();
  vector<temporand_set > Alldiffs;
  vector<presolver_conj> Nogoods1;
  InfeasiblePresolver ip = InfeasiblePresolver(PA, input, t, options);
  ip.setup();
  ip.pass1(Alldiffs, Nogoods1);

  if (timeout(t, options, "gen_infeasible pass 1", t0))
    return;

  // 6: JSON.difftemps <- DIFFTEMPS() UNION Assert.cur_difftemp

  t0.start();
  diff_temps(input);
  input.difftemps = ord_union(input.difftemps, PA.cur_difftemp);
  if (timeout(t, options, "difftemps", t0))
    return;

  // 7: JSON.diffregs <- {STRIP(D) | D IN Alldiffs}

  t0.start();
  for (const temporand_set& D : Alldiffs)
    input.diffregs.push_back(strip(D));
  if (timeout(t, options, "diffregs", t0))
    return;

  // 8: JSON.domops <- TEMPDOMINATION()

  t0.start();
  temp_domination(input);
  if (timeout(t, options, "domops", t0))
    return;

  // 9, 10: JSON.set_across + <Across,AltAcross,CondBefore,Nogoods2>

  t0.start();
  vector<PresolverAcrossTuple> Across;
  vector<PresolverSetAcrossTuple> AltAcross;
  vector<PresolverBefore> CondBefore;
  vector<presolver_conj> Nogoods2;
  presolve_across(PA, input, Across, AltAcross, CondBefore, Nogoods2);
  alt_across_to_json(input, AltAcross);
  if (timeout(t, options, "set_across", t0))
    return;

  // 11: Nogoods3 <- KERNELSET (Nogoods UNION Nogoods1 UNION Nogoods2, {})

  t0.start();
  vector<presolver_conj> n0, n1;

  set_union(Nogoods.begin(), Nogoods.end(),
	    Nogoods1.begin(), Nogoods1.end(),
	    back_inserter(n0));
  set_union(n0.begin(), n0.end(),
	    Nogoods2.begin(), Nogoods2.end(),
	    back_inserter(n1));
  Nogoods = kernel_set(n1, {}, -1);

  if (timeout(t, options, "Nogoods3", t0))
    return;

  // 12: DNogoods <-
  //     {{eq(p(p), p(q))}, {eq(p(p), t(t)), eq(p(q), t(t))}
  //            | t in OPNDTEMPS(p) /\ {p, q} SUBSETOF c /\ c IN JSON.difftemps}

  t0.start();
  vector<presolver_conj> DNogoods;
  for(const vector<operand>& c : input.difftemps) {
    if(c.size() >= 2) {
      for(unsigned i = 0; i < c.size(); i++) {
	for(unsigned j = i+1; j < c.size(); j++) {
	  operand p = c[i];
	  operand q = c[j];
	  for(temporary t : input.temps[p]) {
	    UnisonConstraintExpr e1(SHARE_EXPR, {p,q}, {});
	    UnisonConstraintExpr e2(CONNECTS_EXPR, {p,t}, {});
	    UnisonConstraintExpr e3(CONNECTS_EXPR, {q,t}, {});
 	    vector_insert(DNogoods, {e1});
	    vector_insert(DNogoods, {e2,e3});
	  }
	}
      }
    }
  }

  if (timeout(t, options, "DNogoods", t0))
    return;

  // 13: Nogoods <- KERNELSET(DNogoods, Nogoods3)

  t0.start();
  Nogoods = kernel_set(DNogoods, Nogoods, -1);
  if (timeout(t, options, "Nogoods", t0))
    return;

  // 14: JSON.across <- AcrossToJson(Across, Nogoods)

  t0.start();
  across_to_json(input, Across, Nogoods);
  if (timeout(t, options, "across", t0))
    return;

  // 15: MoreNogoods <- KernelSet(Assert.more_nogood, Nogoods)

  t0.start();
  vector<presolver_conj> MoreNogoods = kernel_set(PA.more_nogoods, Nogoods, -1);
  if (timeout(t, options, "MoreNogoods", t0))
    return;

  // 16: JSON.nogoods2 <- MoreNogoods \ Nogoods

  t0.start();
  for (const presolver_conj& ng : ord_difference(MoreNogoods, Nogoods)) {
    input.nogoods2.push_back(conj_to_expr(ng));
  }
  sort(input.nogoods2.begin(), input.nogoods2.end()); // canonicalize
  if (timeout(t, options, "nogoods2", t0))
    return;

  // 17: JSON.before2 <- {<p,q,d'> | <p,q,d> in CondBefore, d' = FilterCondition(d,MoreNogoods) where d' != {}}

  t0.start();
  for(PresolverBefore& bef : CondBefore) {
    PresolverBeforeJSON befJSON(bef.p, bef.q, disj_to_expr(filter_condition(bef.d, MoreNogoods)));
    if(befJSON.e.id!=OR_EXPR || !befJSON.e.children.empty()) // not "false"
      vector_insert(input.before2, befJSON);
  }
  if (timeout(t, options, "before2", t0))
    return;

  // 18: Precedences <- GenPrecsPrecedences() U GenBeforePrecedences() U GenFixedPrecedences()

  precedence_set precedences;
  vector<vector<vector<int>>> min_con_erg = vector<vector<vector<int>>>(input.O.size());
  t0.start();
  gen_min_con_erg(input, min_con_erg);
  gen_precs_precedences(input, min_con_erg, precedences);
  gen_before_precedences(input, options, input.before, min_con_erg, precedences, t);
  if (timeout(t, options, "Precedences - gen_before_precedences", t0, false))
    return;
  gen_fixed_precedences(input, precedences);
  if (timeout(t, options, "Precedences - gen_fixed_precedences", t0, false))
    return;
  sort(precedences.begin(), precedences.end());
  precedences.erase(unique(precedences.begin(), precedences.end()), precedences.end());

  // 19: JSON.precedences <- NormalizePrecedences()

  t0.start();
  precedence_set temp;
  for(const PresolverPrecedence& pred : precedences) {
    operation p = pred.i;
    operation q = pred.j;
    int n = pred.n;
    presolver_disj b = pred.d;
    presolver_disj b1 = filter_condition(b, Nogoods);
    if(!b1.empty()) {
      PresolverPrecedence elem(p, q, n, b1);
      vector_insert(temp, elem);
    }
  }
  normalize_precedences(input, temp, input.precedences);
  if (timeout(t, options, "precedences", t0))
    return;

  // 20: JSON.precedences2 <- NormalizePrecedences()

  t0.start();
  temp.clear();
  precedence_set precedences2;
  gen_before_precedences(input, options, input.before2, min_con_erg, precedences2, t);
  if (timeout(t, options, "precedences2 - gen_before_precedences", t0, false))
    return;
  for(const PresolverPrecedence& pred : precedences2) {
    operation p = pred.i;
    operation q = pred.j;
    int n = pred.n;
    presolver_disj b = pred.d;
    presolver_disj b1 = filter_condition(b, Nogoods);
    if(!b1.empty()) {
      PresolverPrecedence elem(p, q, n, b1);
      vector_insert(temp, elem);
    }
  }
  normalize_precedences(input, temp, input.precedences2);
  if (timeout(t, options, "precedences2", t0))
    return;

  // 21: JSON.calleesaved_spill

  t0.start();
  gen_calleesaved_spill(input);
  if (timeout(t, options, "calleesaved_spill", t0))
    return;

  // 22: JSON.dur

  t0.start();
  for (instruction i : input.I)
    for (resource r : input.R)
      if (input.con[i][r]==0)
	input.dur[i][r] = 1;
  if (timeout(t, options, "dur", t0))
    return;

  // 23: JSON.long_latency_index, JSON.long_latency_def_use: identify global operands that may need nonzero slack

  t0.start();
  gen_long_latency(input);
  if (timeout(t, options, "long_latency", t0))
    return;

  // 24: JSON.value_precede_chains

  t0.start();
  input.value_precede_chains = value_precede_chains(input, NULL, true, -1);
  if (timeout(t, options, "value_precede_chains", t0))
    return;

  // 25: JSON.quasi_adjacent

  t0.start();
  quasi_adjacent(input);
  if (timeout(t, options, "quasi_adjacent", t0))
    return;

  // 26: JSON.dominates

  t0.start();
  gen_dominates(input);
  if (timeout(t, options, "gen_dominates", t0))
    return;

  // 27: DetectCycles(Nogoods)

  t0.start();
  vector<presolver_conj> Nogoods3, n3;
  int cutoff = (options.timeout() - t.stop()) / 2;

  ip.pass2(Nogoods3);
  n3 = ord_difference(Nogoods3, Nogoods);
  Nogoods = kernel_set(n3, Nogoods, cutoff);
  if (timeout(t, options, "gen_infeasible pass 2", t0))
    return;

  ip.detect_cycles();

  if (timeout(t, options, "detect_cycles", t0))
    return;

  // 28: JSON.nogoods <- KERNELSET(Assert.new_nogood, Nogoods) \ DNogoods

  t0.start();
  cutoff = (options.timeout() - t.stop()) / 2;
  for (const presolver_conj& ng : ord_difference(kernel_set(PA.new_nogood, Nogoods, cutoff), DNogoods)) {
    input.nogoods.push_back(conj_to_expr(ng));
  }
  sort(input.nogoods.begin(), input.nogoods.end()); // canonicalize
  if (timeout(t, options, "nogoods", t0))
    return;

  if (options.tabling()) {

  // 29: GenActiveTables(), JSON.tmp_tables
    
    t0.start();
    input.compute_derived();	// refresh for Model:: methods
    gen_active_tables(input, t, options);
    if (timeout(t, options, "gen_active_tables", t0))
      return;

  // 30: FilterActiveTables(), JSON.active_tables, JSON.dominates

    t0.start();
    filter_active_tables(input);	// sets input.active_tables, input.dominates
    if (timeout(t, options, "filter_active_tables", t0))
      return;

  // 31: for all b ∈ JSON.B do
  // 32:   JSON.optional_min[b] <- OptionalminActiveTables(b)
  // 33: end for

    t0.start();
    for(block b : input.B)
      input.optional_min[b] = optional_min_active_tables(input, b);
    if (timeout(t, options, "optional_min", t0))
      return;
  } // end of options.tabling()
  
  // 34: JSON.subsumed_resources
  
  subsumed_resources(input);
  
  // 35: JSON.precedences <- JSON.precedences U NormalizePrecedences(GenRegionPrecedences())

  if (options.regions()) {
    t0.start();
    precedence_set region_precedences;
    gen_region_precedences(input, min_con_erg, precedences, region_precedences); 
    normalize_precedences(input, region_precedences, input.precedences);
    if (timeout(t, options, "region_precedences", t0))
      return;
  }

  // 36: Tidy()
  tidy(input);

  // these can cause huge printouts and should be protected from timeouts

  // input.compute_derived();	// refresh for Model:: methods
  // GlobalModel * gm = new GlobalModel(&input, &moptions, IPL_DOM);
  // test_redundancy(input, gm);
  // delete gm;

  if (options.test()) {
    run_test("nogoods", nogoods_ref, input.nogoods);
    run_test("congr", congr_ref, input.strictly_congr);
    run_test("last_use", last_use_ref, input.last_use);
    run_test("before", before_ref, input.before);
    run_test("difftemps", difftemps_ref, input.difftemps);
    run_test("diffregs", diffregs_ref, input.diffregs);
    run_test("domops", domops_ref, input.domops);
    run_test("across", across_ref, input.across);
    run_test("set_across", set_across_ref, input.set_across);
    run_test("before2", before2_ref, input.before2);
    run_test("nogoods2", nogoods2_ref, input.nogoods2);
    run_test("precedences", precedences_ref, input.precedences);
    run_test("precedences2", precedences2_ref, input.precedences2);
    run_test("calleesaved_spill", calleesaved_spill_ref, input.calleesaved_spill);
    run_test("value_precede_chains", value_precede_chains_ref, input.value_precede_chains);
    run_test("predecessors", predecessors_ref, input.predecessors);
    run_test("successors", successors_ref, input.successors);
    run_test("quasi_adjacent", quasi_adjacent_ref, input.quasi_adjacent);
    run_test("long_latency_index", long_latency_index_ref, input.long_latency_index);
    run_test("long_latency_def_use", long_latency_def_use_ref, input.long_latency_def_use);
    run_test("subsumed_resources", subsumed_resources_ref, input.subsumed_resources);
    run_test("dominates", dominates_ref, input.dominates);
    run_test("optional_min", optional_min_ref, input.optional_min);
    run_test("active_tables", active_tables_ref, input.active_tables);
    run_test("tmp_tables", tmp_tables_ref, input.tmp_tables);
  }
}

