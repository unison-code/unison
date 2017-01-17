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


#include "registerarrayinspector.hpp"

std::string RegisterArrayInspector::name(void) { return "Register array"; }

register_atom RegisterArrayInspector::max_atom(const Model& m, block b) {
  register_atom max = 0;
  for (temporary t : m.input->tmp[b]) {
    if (m.l(t).assigned() && m.l(t).val() && m.r(t).assigned()) {
      register_atom right = m.r(t).val() + m.input->width[t];
      if (right > max) max = right;
    }
  }
  for (register_space rs : m.input->RS) {
    if (rs == 0) continue; // "all" register space
    register_atom right = m.input->range[rs][1];
    if (!m.input->infinite[rs] && right > max) max = right;
  }
  return max;
}

void RegisterArrayInspector::inspectb(const Model& m, block b, QPointF& topLeft) {

  QPen spen;
  spen.setWidth(2);

  operation out = m.input->out[b];

  draw_horizontal_register_array_label(m, max_atom(m, b), topLeft);
  draw_grid(max_atom(m, b), m.c(out).max(), topLeft, "cycle");

  // Rows (cycles)
  for (int c = 0; c < m.c(out).max(); c++) {
    QPointF clp = (QPointF(-0.6, c + 0.2) * DPI) + topLeft;
    QGraphicsTextItem *cl = scene->addText(QString("%1").arg(c));
    cl->setPos(clp);
    setFontPointSize(cl, FONTSIZE);
  }

  // Draw temporary placements

  for (temporary t : m.input->tmp[b]) {

    int ls = m.ls(t).min(),
        le = m.le(t).max();

    if (m.l(t).assigned() && m.l(t).val() &&
        m.r(t).assigned() && m.ls(t).assigned() && m.le(t).assigned()) {
      QPointF tr = (QPointF(m.r(t).val(), ls) * DPI) + topLeft;
      QSize str = QSize(m.input->width[t], le - ls) * DPI;
      // Place non-zero temporary rectangles that are already decided
      if ((le - ls) > 0) {
        QColor color = type_color(m.input->type[m.input->def_opr[t]]);
        draw_rectangle(tr, str, color);
        QGraphicsTextItem *l = scene->addText(emit_temporary(t));
        l->setPos(tr);
        setFontPointSize(l, FONTSIZE);
      } else { // Draw 0-length value rectangles
        QPointF right = tr + QPointF(str.width(), 0);
        QGraphicsLineItem * sep = scene->addLine(QLineF(tr, right));
        QPen spen;
        QVector<qreal> dashes;
        qreal space = 5;
        dashes << 1 << space;
        spen.setDashPattern(dashes);
        sep->setPen(spen);
      }
    }
  }
}

void GlobalRegisterArrayInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 2,
      yoff = 2;

  Dot cfg;

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    int w = max_atom(m, b) + xoff;
    int cycles = m.c(m.input->out[b]).max();
    int h = cycles + yoff;
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

void LocalRegisterArrayInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 2,
      yoff = 1;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;
  inspectb(m, m.b, topLeft);

  int cycles = m.c(m.input->out[m.b]).max();
  int w = max_atom(m, m.b) + xoff;
  int h = cycles + yoff;

  QRectF boundRect = QRectF(QPointF(0, 0), (QSizeF(w, h) * DPI));

  scene->setSceneRect (boundRect);
  view->fitInView (boundRect, Qt::KeepAspectRatio);

  mw->show();

}
