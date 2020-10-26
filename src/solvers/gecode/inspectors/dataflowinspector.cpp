/*
 *  Main authors:
 *    Roberto Castaneda Lozano <rcas@acm.org>
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


#include "dataflowinspector.hpp"

std::string DataFlowInspector::name(void) { return "Data flow"; }

DataFlowGraph DataFlowInspector::dataFlowGraph(const Model& m, block b) {

  DataFlowGraph dfg;

  //cerr << "digraph b" << b << "{\n";

  for (operation o : m.input->ops[b]) {
    //cerr << "\tnode " << i << "\n";

    NodeType type;
    if (m.a(o).assigned() && m.a(o).val()) type = ACTIVE;
    else if (m.a(o).in(1)) type = UNDECIDEDACTIVENESS;
    else type = INACTIVE;
    dfg.nodeType[o] = type;

    for (instruction i : m.input->instructions[o])
      dfg.initialInstructions[o].push_back(show_instruction(i, o, m.input).c_str());

    for (IntVarValues o1(m.i(o)); o1(); ++o1) {
      instruction i = m.input->instructions[o][o1.val()];
      dfg.instructions[o].push_back(show_instruction(i, o, m.input).c_str());
    }

    QString in = QString::number(o);
    dfg.graph->insert(in);

    QSize labSize = dfg.nodeLabelSize(o, DPI);
    dfg.graph->setNodeSize(in, labSize.width() / DPI, labSize.height() / DPI);

  }

  // Add fixed getEdges()

  int instance = 0;

  for (unsigned int e = 0; e < m.input->dep[b].size(); e++) {
    operation o1 = m.input->dep[b][e][0],
              o2 = m.input->dep[b][e][1];
    //cerr << "\t" << i << " -> " << j << "\n";

    EdgeId key = edgeId(o1, o2, instance);

    dfg.graph->insert(key);
    dfg.edgeType[key] = FIXED;

    instance++;
  }

  // Add data getEdges()
  for (operation o : m.input->ops[b])
    for (operand p : m.input->operands[o])
      if (m.input->use[p])
        for (unsigned int ti = 0; ti < m.input->temps[p].size(); ti++) {
          temporary t = m.input->temps[p][ti];
          if (t != NULL_TEMPORARY) {
            operation d = m.input->def_opr[t];
            QString source = QString::number(d),
                    target = QString::number(o);

            EdgeType type;
            if (m.y(p).assigned() && m.y(p).val() == (int)ti) type = ASSIGNED;
            else if (m.y(p).in(ti)) type = POSSIBLE;
            else type = DISCARDED;

            EdgeId key = edgeId(source, target, instance);

            if (dfg.edgeType.count(key) && (dfg.edgeType[key] > type))
              continue;

            QString label = "t" + QString::number(t);

            dfg.graph->insert(key);
            dfg.graph->setEdgeAttribute(key, "label", label);
            dfg.graph->setEdgeAttribute(key, "fontsize", "20");
            dfg.edgeLabel[key] = label;
            dfg.edgeType[key] = type;

            EdgeLabelType labType;
            if (m.l(t).assigned() && m.l(t).val()) labType = LIVE;
            else if (m.l(t).in(1)) labType = UNDECIDEDLIVENESS;
            else labType = DEAD;
            dfg.edgeLabelType[key] = labType;

            instance++;
          }
        }

  //cerr << "}\n";

  // Compute graph layout
  dfg.graph->draw();

  return dfg;

}

void DataFlowInspector::
draw(const Model& m, DataFlowGraph& dfg, const QPointF& topLeft) {

  for (DotEdge edge : dfg.graph->getEdges()) {

    operation o = get<0>(edge.key).toInt();

    EdgeType edgeType = dfg.edgeType[edge.key];
    EdgeLabelType edgeLabelType = dfg.edgeLabelType[edge.key];

    if (edgeType == FIXED || edgeType == DISCARDED) continue;

    QColor fillcolor = type_color(m.input->type[o]);
    if (edgeType == ASSIGNED) fillcolor = fillcolor.darker(110);

    QPen pen(fillcolor);
    pen.setWidth(edgeType == ASSIGNED ? 6 : 2);

    QBrush brush(fillcolor);
    QColor labColor = edgeLabelType == LIVE ? Qt::black : Qt::gray;

    QGraphicsTextItem *l = scene->addText(dfg.edgeLabel[edge.key]);
    QGraphicsPathItem * e = new QGraphicsPathItem(edge.path);
    QGraphicsPathItem * a = new QGraphicsPathItem(edge.arrow);

    e->setTransform(QTransform::fromTranslate(topLeft.x(), topLeft.y()), true);
    a->setTransform(QTransform::fromTranslate(topLeft.x(), topLeft.y()), true);

    e->setPen(pen);
    a->setBrush(brush);
    a->setPen(pen);

    scene->addItem(e);
    scene->addItem(a);

    QPointF labelTopLeft = topLeft + edge.topLeftPos;
    l->setPos(labelTopLeft);
    l->setDefaultTextColor(labColor);
    QFont f = l->font();
    f.setPointSize(20);
    l->setFont(f);

  }

  for (DotNode node : dfg.graph->getNodes()) {

    operation o = node.name.toInt();

    NodeType nodeType = dfg.nodeType[o];

    if (nodeType == INACTIVE) continue;

    QColor textcolor = nodeType == ACTIVE ? Qt::black : Qt::gray;
    QColor fillcolor = type_color(m.input->type[o]);
    if (!(nodeType == ACTIVE)) fillcolor = fillcolor.lighter(120);

    QPen pen(fillcolor.darker(110));
    pen.setWidth(nodeType == ACTIVE ? 6 : 2);

    QBrush brush(fillcolor);

    QPointF nodeTopLeft = topLeft + node.topLeftPos + (QPointF(0, 0) * DPI);

    QSize labSize = dfg.nodeLabelSize(o, DPI);
    QGraphicsRectItem *r =
      scene->addRect(nodeTopLeft.x(), nodeTopLeft.y(),
                     labSize.width(), labSize.height());
    r->setPen(pen);
    r->setBrush(brush);

    QGraphicsTextItem *l = scene->addText(dfg.nodeLabel(o));
    l->setPos(nodeTopLeft);
    setFontPointSize(l, FONTSIZE);
    l->setDefaultTextColor(textcolor);

  }

}

void GlobalDataFlowInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 1,
      yoff = 1;

  Dot cfg;

  vector<DataFlowGraph> dfgs;

  // Build data-flow graphs
  for (block b : m.input->B) dfgs.push_back(dataFlowGraph(m, b));

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    QString bn = QString::number(b);
    cfg.insert(bn);
    double w = (dfgs[b].graph->box().width() / DPI) + xoff,
           h = (dfgs[b].graph->box().height() / DPI) + yoff;
    cfg.setNodeSize(bn, w, h);
  }

  for (vector<block> e : m.input->cfg) cfg.insert(edgeId(e[0], e[1]));

  // Compute graph layout
  cfg.draw();

  QPointF off = QPointF(xoff, yoff) * DPI;

  QPen thickPen(Qt::black);
  thickPen.setWidth(3);

  for (DotNode node : cfg.getNodes()) {
    block b = node.name.toInt();
    QPointF topLeft = node.topLeftPos + off;
    draw(m, dfgs[b], topLeft);

    QRectF br = dfgs[b].graph->box();
    br.moveTopLeft(topLeft);
    QGraphicsRectItem *r = scene->addRect(br);
    r->setPen(thickPen);
  }

  for (DotEdge edge : cfg.getEdges()) {

    QGraphicsPathItem * e = new QGraphicsPathItem(edge.path);
    QGraphicsPathItem * a = new QGraphicsPathItem(edge.arrow);
    e->setPen(thickPen);

    QBrush brush(Qt::black);
    a->setBrush(brush);
    a->setPen(thickPen);

    scene->addItem(e);
    scene->addItem(a);

  }

  scene->setSceneRect (cfg.box());
  view->fitInView (cfg.box(), Qt::KeepAspectRatio);
  mw->show();

}

void LocalDataFlowInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 0,
      yoff = 0;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;

  DataFlowGraph dfg = dataFlowGraph(m, m.b);
  draw(m, dfg, topLeft);

  show_window(dfg.graph->box());

}
