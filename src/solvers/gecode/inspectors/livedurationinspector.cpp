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


#include "livedurationinspector.hpp"

std::string LiveDurationInspector::name(void) { return "Temporary live durations"; }

int LiveDurationInspector::maxduration(const Model& m, block b) {
  int maxld = 0;
  for (temporary t : m.input->tmp[b])
    if (m.ld(t).max() > maxld) maxld = m.ld(t).max();
  return maxld;
}

void LiveDurationInspector::inspectb(const Model& m, block b, QPointF& topLeft) {

  int maxld = maxduration(m, b);

  vector<temporary> ts = temporaries(m, b);
  draw_grid(ts.size(), maxld, topLeft, "duration (cycles)");

  // x label (temporaries)
  for (unsigned int ti = 0; ti < ts.size(); ti++) {
    QPointF clp = (QPointF(ti + 0.2, -0.8) * DPI) + topLeft;
    QGraphicsTextItem *l = scene->addText(QString("t%1").arg(ts[ti]));
    l->setPos(clp);
    setFontPointSize(l, FONTSIZE);
  }

  // Rows (cycles)
  for (int c = 0; c < maxld; c++) {
    QPointF clp = (QPointF(-0.6, c + 0.2) * DPI) + topLeft;
    QGraphicsTextItem *cl = scene->addText(QString("%1").arg(c));
    cl->setPos(clp);
    setFontPointSize(cl, FONTSIZE);
  }

  // Columns (temporaries)
  for (unsigned int ti = 0; ti < ts.size(); ti++) {
    temporary t = ts[ti];
    int ldmin = m.ld(t).min(),
        ldmax = m.ld(t).max();
    // Draw domain arrows
    QColor arrowcolor = m.is_dead(t) ? gridcolor : Qt::black;
    QPointF top = (QPointF(ti + 0.5, ldmin) * DPI) + topLeft;
    QPointF bottom = (QPointF(ti + 0.5, ldmax) * DPI) + topLeft;
    draw_vertical_double_arrow(top, bottom, arrowcolor);
  }
}

void GlobalLiveDurationInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 2,
      yoff = 1;

  Dot cfg;

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    int h = maxduration(m, b) + yoff;
    int w = temporaries(m, b).size() + xoff;
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
  }

  draw_cfg_edges(cfg);

  show_window(cfg.box());

}

void LocalLiveDurationInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 2,
      yoff = 1;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;
  inspectb(m, m.b, topLeft);

  int w = temporaries(m, m.b).size() + xoff;
  int h = maxduration(m, m.b) + yoff;

  QRectF boundRect = QRectF(QPointF(0, 0), (QSizeF(w, h) * DPI));

  show_window(boundRect);

}
