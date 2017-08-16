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


#include "resourceconsumptioninspector.hpp"

std::string ResourceConsumptionInspector::name(void) {
  return "Resource consumption";
}

void ResourceConsumptionInspector::
inspectb(const Model& m, block b, QPointF& topLeft) {

  QPen spen;
  spen.setWidth(2);

  int factor = capFactor(m, b);

  operation out = m.input->out[b];

  QPointF gridTopLeft = topLeft + (QPointF(0, 1) * DPI);
  draw_grid(m.input->cap[r] / factor, m.c(out).max(), gridTopLeft, "cycle");

  // x label (resource units)
  for (int ru = 0; ru < m.input->cap[r] / factor; ru++) {
    QPointF rp = (QPointF(ru + 0.2, 0) * DPI) + topLeft;
    QGraphicsTextItem *l = scene->addText(QString("%1").arg(ru * factor));
    l->setPos(rp);
    setFontPointSize(l, FONTSIZE);
  }

  // Rows (cycles)
  for (int c = 0; c < m.c(out).max(); c++) {
    QPointF clp = (QPointF(-0.6, c + 1.2) * DPI) + topLeft;
    QGraphicsTextItem *cl = scene->addText(QString("%1").arg(c));
    cl->setPos(clp);
    setFontPointSize(cl, FONTSIZE);
  }

  // Compute profile

  vector<operation> os; // Fixed operations
  for (operation o : m.input->ops[b])
    if (m.a(o).assigned() && m.a(o).val() &&
        m.c(o).assigned() && m.i(o).assigned()) {
      instruction i = m.input->instructions[o][m.i(o).val()];
      if (m.input->con[i][r] > 0) os.push_back(o);
    }
  int top = 0;
  vector<int> total_consumption;
  map<int, set<operation> > consumers;
  consumers[-1] = set<operation>();
  for (int c = 0; c < m.c(out).max() + 1; c++) {
    set<operation> cycle_consumers = consumers[c - 1];
    // Remove outs
    for (operation o : os) {
      instruction i = m.input->instructions[o][m.i(o).val()];
      if (c == (m.c(o).val() + m.input->dur[i][r])) {
        cycle_consumers.erase(o);
        top -= (m.input->con[i][r] / factor);
      }
    }
    // Add ins
    for (operation o : os) {
      instruction i = m.input->instructions[o][m.i(o).val()];
      if (c == m.c(o).val()) {
        cycle_consumers.insert(o);
        top += (m.input->con[i][r] / factor);
      }
    }
    consumers[c] = cycle_consumers;
    total_consumption.push_back(top);
  }

  for (int c = 0; c < m.c(out).max(); c++) {
    if (total_consumption[c] == 0) continue;
    QPointF ir = (QPointF(0, c + 1) * DPI) + topLeft;
    QSize sir  = QSize(total_consumption[c], 1) * DPI;
    int red =
      ((double)total_consumption[c] / (double)(m.input->cap[r] / factor)) * 255;
    int green = 255 - red;
    QColor color = QColor(red, green, 0);
    QPen spen(color);
    QBrush rbrush(color);
    QGraphicsRectItem *r =
      scene->addRect(ir.x(), ir.y(), sir.width(), sir.height());
    r->setPen(spen);
    r->setBrush(rbrush);
    vector<QString> cs;
    for (operation o : consumers[c]) cs.push_back("o" + QString::number(o));
    QGraphicsTextItem *l = scene->addText("{" + sepBy(cs, QString(", ")) + "}");
    QPointF lp = (ir + QPointF(0.2, 0.2) * DPI);
    l->setPos(lp);
    setFontPointSize(l, FONTSIZE);
  }

}

int ResourceConsumptionInspector::capFactor(const Model& m, block b) {
  vector<operation> os;
  map<operation, int> con;
  for (operation o : m.input->ops[b])
    for (instruction i : m.input->instructions[o])
      if (m.input->con[i][r] > 0) {
        os.push_back(o);
        con[o] = m.input->con[i][r];
      }
  if (os.size() < 2) return 1;
  int f = con[os[0]];
  for (operation o : os) f = gcd(f, con[o]);
  return f;
}

void GlobalResourceConsumptionInspector::inspect(const Space& s) {

  const Model& m = static_cast<const Model&>(s);

  initialize();

  int xoff = 2,
      yoff = 3;

  Dot cfg;

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    int cycles = m.c(m.input->out[b]).max();
    int h = cycles + yoff + 1;
    int w = (m.input->cap[r] / capFactor(m, b)) + xoff + 1;
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

void LocalResourceConsumptionInspector::inspect(const Space& s) {

  const LocalModel& m = static_cast<const LocalModel&>(s);

  initialize();

  int xoff = 2,
      yoff = 3;

  QPointF topLeft = QPointF(xoff, yoff) * DPI;
  inspectb(m, m.b, topLeft);

  int cycles = m.c(m.input->out[m.b]).max();
  int h = cycles + yoff + 1;
  int w = (m.input->cap[r] / capFactor(m, m.b)) + xoff + 1;
  QRectF boundRect = QRectF(QPointF(0, 0), (QSizeF(w, h) * DPI));

  show_window(boundRect);

}
