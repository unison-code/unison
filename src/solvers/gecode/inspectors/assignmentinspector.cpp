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


#include "assignmentinspector.hpp"

std::string AssignmentInspector::name(void) { return "Temporary assignment"; }

void AssignmentInspector::inspectb(const Model& m, block b, QPointF& topLeft) {

  QPen spen;
  spen.setWidth(2);

  QPointF gridTopLeft = topLeft + QPointF(1 * DPI, 0);
  draw_horizontal_register_array_label(
    m, m.input->RA.size(), gridTopLeft, true);

  vector<temporary> ts = temporaries(m, b);

  draw_grid(m.input->RA.size() + 1, ts.size(), topLeft);

  // Rows (temporaries)
  for (unsigned int ti = 0; ti < ts.size(); ti++) {
    temporary t = ts[ti];
    int w = m.input->width[t];

    // Show temporary name to the left
    QPointF tlp = (QPointF(-0.9, ti + 0.2) * DPI) + topLeft;
    QGraphicsTextItem *l = scene->addText(QString("t%1").arg(t));
    l->setPos(tlp);
    setFontPointSize(l, FONTSIZE);

    if (m.r(t).assigned()) { // Draw rectangles
      register_atom ra = m.r(t).val();
      if (ra == NULL_REGISTER) {
        l->setDefaultTextColor(gridcolor);
        QPen spen(gridcolor);
        spen.setWidth(10);
        QPointF left = (QPointF(ra + 0.2, ti + 0.5) * DPI) + gridTopLeft;
        QPointF right = (QPointF(ra + 0.8, ti + 0.5) * DPI) + gridTopLeft;
        QGraphicsLineItem * line = scene->addLine(QLineF(left, right));
        line->setPen(spen);
      } else {
        QPointF tr = (QPointF(ra, ti) * DPI) + gridTopLeft;
        QSize str = QSize(ra == NULL_TEMPORARY ? 1 : w, 1) * DPI;
        QColor color = type_color(m.input->type[m.input->def_opr[t]]);
        draw_rectangle(tr, str, color);
      }
    } else { // Draw domain arrows
      IntVarRanges tregs(m.r(t));
      Gecode::Region r;
      Iter::Ranges::RangeListIter ranges = extend(r, tregs, w);
      for (; ranges(); ++ranges) {
        QPointF left = (QPointF(ranges.min(), ti + 0.5) * DPI) + gridTopLeft;
        QPointF right = (QPointF(ranges.max() + 1, ti + 0.5) * DPI) + gridTopLeft;
        draw_horizontal_double_arrow(left, right, Qt::black);
      }
    }
  }

}

void GlobalAssignmentInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 2,
      yoff = 2;

  int w = m.input->RA.size() + xoff + 1;

  Dot cfg;

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    int h = temporaries(m, b).size() + yoff;
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

void LocalAssignmentInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 2,
      yoff = 2;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;
  inspectb(m, m.b, topLeft);

  int w = m.input->RA.size() + xoff + 1;
  int h = temporaries(m, m.b).size() + yoff;

  QRectF boundRect = QRectF(QPointF(0, 0), (QSizeF(w, h) * DPI));

  show_window(boundRect);

}
