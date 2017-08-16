/*
 *  Main authors:
 *    Roberto Castaneda Lozano <roberto.castaneda@ri.se>
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


#include "value.hpp"

int most_effective_connection_decision(
    const Space& s, BoolVar, global_cluster gc) {
  const GlobalModel& m = static_cast<const GlobalModel&>(s);

  // Agressiveness factor (between 0 - least aggresive and 1 - most aggressive)
  double af = m.af;

  // cost((!)x(p)) == average cycles spent in "spilling" due to increased
  // register pressure
  double connect_cost = 0.0, disconnect_cost = 0.0;
  {
    operand p = m.input->clusters[gc][0];
    IntVarRanges gregs(m.ry(p));
    Region region;
    RangeListIter tA = extend(region, gregs, m.input->operand_width[p]);

    register_space rs = m.input->home[p];
    Singleton rsA(m.input->range[rs][0], m.input->range[rs][1]);
    NaryInter A(region, tA, rsA);

    connect_cost    = m.cluster_allocation_cost(gc, A);
    disconnect_cost = m.cluster_remat_cost(gc, A);
  }

  // benefit((!)x(p)) == pontential cycles saved by avoiding optional operations
  double connect_benefit    = m.connection_benefit(gc, true),
         disconnect_benefit = m.connection_benefit(gc, false);

  double connect_e    = (connect_benefit * af) - (connect_cost * (1 - af)),
         disconnect_e = (disconnect_benefit * af) - (disconnect_cost * (1 - af));

  return connect_e > disconnect_e ? 1 : 0;
}

register_space most_effective(const Space& s, SetVar pals, global_congruence g) {
  const GlobalModel& m = static_cast<const GlobalModel&>(s);

  // Agressiveness factor (between 0 - least aggresive and 1 - most aggressive)
  double af = m.af;

  // Undecided register spaces
  vector<register_space> urss;

  // Compute undecided register spaces and their allocatable atom sets
  operand p = m.input->representative[m.input->regular[g]];
  IntVarRanges gregs(m.ry(p));

  for (SetVarUnknownValues rsi(pals); rsi(); ++rsi) {
    register_space rs = rsi.val();
    if (m.input->candidate_spaces[g].count(rs)) urss.push_back(rs);
  }

  // maximum effectiveness
  double maxe = INT_MIN;

  // register space space to which allocating g maximizes the effectiveness
  register_space maxers = -1;

  {
    Region region;
    RangeListIter tA = extend(region, gregs, m.input->operand_width[p]);

    for (register_space rs : urss) {

      Singleton rsA(m.input->range[rs][0], m.input->range[rs][1]);
      NaryInter A(region, tA, rsA);

      // Compute cost for g and all allocatable atom sets
      //
      // cost(g, A) == average cycles spent in copying temporaries out of A due
      //               to increased register pressure

      double cost = m.allocation_cost(g, A);

      // Compute benefit for g and all allocatable atom sets
      //
      // benefit(g, A) == potential cycles saved by allocating g to A

      double benefit = m.allocation_benefit(g, A);

      // Compute effectiveness of allocating g to register space rs
      //
      // effectiveness(g, A) == benefit(g, A) * af - cost(g, A) * (1 - af)

      double e = (benefit * af) - (cost * (1 - af));

      if (e > maxe) {
        maxe = e;
        maxers = rs;
      }

    }
  }

  return maxers;

}

register_atom least_assigned(const Space& s, IntVar pr, global_congruence g) {

  const GlobalModel& m = static_cast<const GlobalModel&>(s);
  int w = m.input->operand_width[m.input->representative[m.input->regular[g]]];

  int minas = numeric_limits<int>::max();
  register_atom minasa = -1;
  for (IntVarValues ra(pr); ra(); ++ra) {
    register_atom a = ra.val();
    int as = 0;
    // for all blocks b in which g has some aligned operand
    for (SetVarGlbValues scs(m.ali(g)); scs(); ++scs) {
      global_congruence g1 = scs.val();
      for (operand p : m.input->congr[m.input->regular[g1]]) {
        block b = m.input->pb[p];
        // for all operands in block b
        for (operand q : m.input->ope[b])
          if (m.ry(q).assigned() &&
              overlap(a, w, m.ry(q).val(), m.input->operand_width[q]))
            as++;
      }
    }

    if (as == 0) return a;

    if (as < minas) {
      minas  = as;
      minasa = a;
    }

  }

  return minasa;

}

// Value that is closest to zero
int closest_to_zero(const Space&, IntVar x, unsigned int) {
  int closest = numeric_limits<int>::max();
  for (IntVarValues v(x); v(); ++v) {
    if (abs(v.val()) < abs(closest)) closest = v.val();
  }
  return closest;
}
