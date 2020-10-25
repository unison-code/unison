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


#include "issuecycleinspector.hpp"

std::string IssueCycleInspector::name(void) { return "Issue cycles"; }

void IssueCycleInspector::inspectb(const Model& m, block b, QPointF& topLeft) {

  QPen spen;
  spen.setWidth(2);

  operation out = m.input->out[b];

  vector<operation> os = operations(m, b);

  QPointF gridTopLeft = topLeft + (QPointF(0, 1) * DPI);
  draw_grid(os.size(), m.c(out).max(), gridTopLeft, "cycle");

  // x label (operations)
  for (unsigned int oi = 0; oi < os.size(); oi++) {
    operation o = os[oi];

    stringstream opd;
    opd << showInstructions(o, m.i(o), m.input);
    QPointF opp = (QPointF(oi + 0.2, -0.4) * DPI) + topLeft;
    QGraphicsTextItem *ol = scene->addText(QString(opd.str().c_str()));
    ol->setPos(opp);
    setFontPointSize(ol, FONTSIZE);
    ol->setRotation(300);
    if (m.is_inactive(o)) ol->setDefaultTextColor(gridcolor);

    QPointF ip = (QPointF(oi + 0.2, 0) * DPI) + topLeft;
    QGraphicsTextItem *l = scene->addText(QString("o%1").arg(o));
    l->setPos(ip);
    setFontPointSize(l, FONTSIZE);

  }

  // Rows (cycles)
  for (int c = 0; c < m.c(out).max(); c++) {
    QPointF clp = (QPointF(-0.6, c + 1.2) * DPI) + topLeft;
    QGraphicsTextItem *cl = scene->addText(QString("%1").arg(c));
    cl->setPos(clp);
    setFontPointSize(cl, FONTSIZE);
  }

  // Columns (operations)
  for (unsigned int oi = 0; oi < os.size(); oi++) {
    operation o = os[oi];
    QColor arrowcolor = m.is_inactive(o) ? gridcolor : Qt::black;
    IntVarRanges ranges(m.c(o));
    Gecode::Region r;
    for (; ranges(); ++ranges) {
      QPointF top = (QPointF(oi + 0.5, ranges.min()) * DPI) + gridTopLeft;
      QPointF bottom = (QPointF(oi + 0.5, ranges.max()) * DPI) + gridTopLeft;
      draw_vertical_double_arrow(top, bottom, arrowcolor);
    }
  }
}

void GlobalIssueCycleInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 2,
      yoff = 3;

  Dot cfg;

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    int cycles = m.c(m.input->out[b]).max();
    int h = cycles + yoff + 1;
    int w = operations(m, b).size() + xoff;
    QString bn = QString::number(b);
    cfg.insert(bn);
    cfg.setNodeSize(bn, w, h);
  }

  // Add getEdges()
  for (vector<block> e : m.input->cfg) cfg.insert(edgeId(e[0], e[1]));

  // Compute graph layout
  cfg.draw();

  QPointF off = QPointF(xoff, yoff) * DPI;

  for (DotNode node : cfg.getNodes()) {
    block b = node.name.toInt();
    QPointF topLeft = node.topLeftPos + off;
    inspectb(m, b, topLeft);
    scene->addRect(node.rect);
  }

  draw_cfg_edges(cfg);

  show_window(cfg.box());

}

void LocalIssueCycleInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 2,
      yoff = 3;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;
  inspectb(m, m.b, topLeft);

  int cycles = m.c(m.input->out[m.b]).max();
  int h = cycles + yoff + 1;
  int w = operations(m, m.b).size() + xoff;
  QRectF boundRect = QRectF(QPointF(0, 0), (QSizeF(w, h) * DPI));

  show_window(boundRect);

}
