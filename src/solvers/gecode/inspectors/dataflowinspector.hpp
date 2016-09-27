/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@sics.se>
 *
 *  This file is part of Unison, see http://unison-code.github.io
 *
 *  Copyright (c) 2016, SICS Swedish ICT AB
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


#ifndef __DATAFLOW_INSPECTOR__
#define __DATAFLOW_INSPECTOR__

#include <QtGui>
#include <gecode/gist.hh>
#include <graphviz/gvc.h>

#include "modelinspector.hpp"
#include "models/localmodel.hpp"
#include "dot.hpp"

enum NodeType {INACTIVE, UNDECIDEDACTIVENESS, ACTIVE};
enum EdgeType {FIXED, DISCARDED, POSSIBLE, ASSIGNED};
enum EdgeLabelType {DEAD, UNDECIDEDLIVENESS, LIVE};

class DataFlowGraph {
public:
  Dot * graph;
  map<EdgeId, EdgeType > edgeType;
  map<EdgeId, EdgeLabelType > edgeLabelType;
  map<EdgeId, QString > edgeLabel;
  map<operation, NodeType > nodeType;
  map<operation, vector<QString> > initialInstructions;
  map<operation, vector<QString> > instructions;
  DataFlowGraph() {
    graph = new Dot();
  }
  ~DataFlowGraph() {}
  QString nodeLabel(operation o, bool ini = false) {
    return "o" + QString::number(o) + ": " + showDomain(ops(o, ini));
  }

  QSize nodeLabelSize(operation o, double DPI) {
    QFont f;
    // TODO: set f to the right default font dynamically
    f.setPointSize(28);
    QFontMetrics fm(f);
    QRect labRect = fm.boundingRect(nodeLabel(o, true));
    double w = std::max((double)labRect.width(), DPI),
           h = std::max((double)labRect.height(), DPI / 2);
    return QSize(w, h);
  }

  vector<QString> ops(operation o, bool ini = false) {
    return ini ? initialInstructions[o] : instructions[o];
  }

};

class DataFlowInspector : public ModelInspector {
public:
  std::string name(void);
  DataFlowGraph dataFlowGraph(const Model& m, block b);
  void draw(const Model& m, DataFlowGraph& dfg, const QPointF& topLeft);
};

class GlobalDataFlowInspector : public DataFlowInspector {
  void inspect(const Space& s);
};

class LocalDataFlowInspector : public DataFlowInspector {
  void inspect(const Space& s);
};

#endif
