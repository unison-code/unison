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


#include "precedenceinspector.hpp"

std::string PrecedenceInspector::name(void) { return "Precedence"; }

PrecedenceGraph PrecedenceInspector::precedenceGraph(const LocalModel& m0, block b) {

  // Do noy worry, m will not be written to via p(o1, o2) since disabling
  // precedence variables disables this inspector
  LocalModel& m = const_cast<LocalModel&> (m0);

  PrecedenceGraph pg;

  // Add getNodes() and set their size
  for (operation o : m.input->mandatory[b]) {

    assert(m.input->instructions[o].size() == 1);
    instruction i = m.input->instructions[o][0];
    pg.instruction[o] = show_instruction(i, o, m.input).c_str();

    QString in = QString::number(o);
    pg.graph->insert(in);

    QSize labSize = pg.nodeLabelSize(o, DPI);
    pg.graph->setNodeSize(in, labSize.width() / DPI, labSize.height() / DPI);

  }

  // Add precedence getEdges()

  int instance = 0;

  for (operation o1 : m.input->mandatory[b])
    for (operation o2 : m.input->mandatory[b]) {
      QString source = QString::number(o1),
              target = QString::number(o2);

      PrecedenceType type;
      if (m.p(o1, o2).assigned() && m.p(o1, o2).val()) {
        type = PRECEDE;
      }
      else if (!m.p(o1, o2).assigned()) {
        type = MAYPRECEDE;
      } else continue;

      EdgeId key = edgeId(source, target, instance);

      pg.graph->insert(key);
      pg.edgeType[key] = type;
      instance++;

    }

  // Compute graph layout
  pg.graph->draw();

  return pg;

}

void PrecedenceInspector::
draw(const LocalModel& m, PrecedenceGraph& pg, const QPointF& topLeft) {

  for (DotNode node : pg.graph->getNodes()) {

    operation o = node.name.toInt();

    QColor textcolor = Qt::black;
    QColor fillcolor = type_color(m.input->type[o]);

    QPen pen(fillcolor.darker(110));

    QBrush brush(fillcolor);

    QPointF nodeTopLeft = topLeft + node.topLeftPos + (QPointF(0, 0) * DPI);

    QSize labSize = pg.nodeLabelSize(o, DPI);
    QGraphicsRectItem *r =
      scene->addRect(nodeTopLeft.x(), nodeTopLeft.y(),
                     labSize.width(), labSize.height());
    r->setPen(pen);
    r->setBrush(brush);

    QGraphicsTextItem *l = scene->addText(pg.nodeLabel(o));
    l->setPos(nodeTopLeft);
    setFontPointSize(l, FONTSIZE);
    l->setDefaultTextColor(textcolor);

  }

  for (DotEdge edge : pg.graph->getEdges()) {

    operation o = get<0>(edge.key).toInt();

    PrecedenceType edgeType = pg.edgeType[edge.key];

    QColor fillcolor = type_color(m.input->type[o]);
    if (edgeType == PRECEDE) fillcolor = fillcolor.darker(110);

    QPen pen(fillcolor);
    pen.setWidth(edgeType == PRECEDE ? 6 : 2);

    QBrush brush(fillcolor);

    QGraphicsPathItem * e = new QGraphicsPathItem(edge.path);
    QGraphicsPathItem * a = new QGraphicsPathItem(edge.arrow);

    e->translate(topLeft.x(), topLeft.y());
    a->translate(topLeft.x(), topLeft.y());

    e->setPen(pen);
    a->setBrush(brush);
    a->setPen(pen);

    scene->addItem(e);
    scene->addItem(a);

  }


}

void LocalPrecedenceInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 0,
      yoff = 0;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;

  PrecedenceGraph pg = precedenceGraph(m, m.b);
  draw(m, pg, topLeft);

  show_window(pg.graph->box());

}
