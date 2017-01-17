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


#include "modelinspector.hpp"

QString fill(QString s, int n, QChar c) {
  int l = max(0, n - s.length());
  return s + QString(c).repeated(l);
}

ModelInspector::ModelInspector(void) :
  scene(NULL),
  view(NULL),
  mw(NULL),
  gridcolor(160,160,160),
  first(true),
  DPI(96.0),
  FONTSIZE(25) {

  typeColor = {
    QColor(141, 211, 199),
    QColor(255, 255, 179),
    QColor(190, 186, 218),
    QColor(251, 128, 114),
    QColor(128, 177, 211),
    QColor(253, 180, 98),
    QColor(179, 222, 105),
    QColor(252, 205, 229),
    QColor(217, 217, 217),
    QColor(188, 128, 189),
    QColor(204, 235, 197),
    QColor(255, 237, 111),
    QColor(241, 150, 112),
    QColor(204, 235, 197)
  };

  globalCongruenceColor = {
    QColor(68, 124, 105),
    QColor(116, 196, 147),
    QColor(142, 140, 109),
    QColor(228, 191, 128),
    QColor(233, 215, 142),
    QColor(226, 151, 93),
    QColor(241, 150, 112),
    QColor(225, 101, 82),
    QColor(201, 74, 83),
    QColor(190, 81, 104),
    QColor(163, 73, 116),
    QColor(153, 55, 103),
    QColor(145, 99, 182),
    QColor(226, 121, 163),
    QColor(224, 89, 139),
    QColor(124, 159, 176),
    QColor(86, 152, 196),
    QColor(154, 191, 136),
    QColor(255, 20, 23),
    QColor(255, 102, 17),
    QColor(255, 136, 68),
    QColor(255, 238, 85),
    QColor(254, 254, 56),
    QColor(255, 255, 153),
    QColor(170, 204, 34),
    QColor(187, 221, 119),
    QColor(200, 207, 130),
    QColor(146, 167, 126),
    QColor(85, 153, 238),
    QColor(0, 136, 204),
    QColor(34, 102, 136),
    QColor(85, 119, 119),
    QColor(221, 187, 51),
    QColor(211, 167, 109),
    QColor(169, 131, 75),
    QColor(170, 102, 136),
    QColor(118, 118, 118)
  };

}

void ModelInspector::initialize(void) {

  if (!scene) {
    mw = new QMainWindow();
    mw->setWindowTitle(QString(name().c_str()));
    scene = new QGraphicsScene();
    view = new ModelGraphicsView(scene);
    view->setRenderHints(QPainter::Antialiasing);
    mw->setCentralWidget(view);
    mw->setAttribute(Qt::WA_QuitOnClose, false);
    mw->setAttribute(Qt::WA_DeleteOnClose, false);
    QAction* closeWindow = new QAction("Close window", mw);
    closeWindow->setShortcut(QKeySequence("Ctrl+W"));
    mw->connect(closeWindow, SIGNAL(triggered()), mw, SLOT(close()));
    mw->addAction(closeWindow);
  }

  QList <QGraphicsItem*> itemList = scene->items();
  for (QGraphicsItem* i : scene->items()) {
    scene->removeItem(i);
    delete i;
  }

}

void ModelInspector::show_window(QRectF boundRect) {
  scene->setSceneRect (boundRect);
  if (first) view->fitInView (boundRect, Qt::KeepAspectRatio);
  mw->show();
  first = false;
}

void ModelInspector::finalize(void) {
  delete mw;
  mw = NULL;
}

QString ModelInspector::emit_temporary(temporary t) {
  std::stringstream out;
  out << "t" << t;
  return QString(out.str().c_str());
}

QColor ModelInspector::type_color(int type) {
  return typeColor[type];
}

QColor ModelInspector::congruence_color(operand p) {
  unsigned n = globalCongruenceColor.size();
  return globalCongruenceColor[p % n];
}

void ModelInspector::setFontPointSize(QGraphicsTextItem * t, int pointSize) {
  QFont f = t->font();
  f.setPointSize(pointSize);
  t->setFont(f);
  return;
}

void ModelInspector::draw_cfg_edges(Dot& cfg) {

  for (DotEdge edge : cfg.getEdges()) {

    QGraphicsPathItem * e = new QGraphicsPathItem(edge.path);
    QGraphicsPathItem * a = new QGraphicsPathItem(edge.arrow);

    QBrush brush(Qt::black);
    a->setBrush(brush);

    scene->addItem(e);
    scene->addItem(a);
  }

}

void ModelInspector::
draw_grid(unsigned int w, unsigned int h, QPointF & topLeft,
          QString ylab, unsigned int cw) {
  for (unsigned int r = 0; r <= h; r++) {
    QPointF left  = (QPointF(0, r) * DPI) + topLeft;
    QPointF right = left + (QPointF(w * cw, 0) * DPI);
    QGraphicsLineItem * line = scene->addLine(QLineF(left, right));
    line->setPen(QPen(gridcolor));
  }
  for (unsigned int c = 0; c <= w; c++) {
    QPointF top    = (QPointF(c*cw, 0) * DPI) + topLeft;
    QPointF bottom = top + (QPointF(0, h) * DPI);
    QGraphicsLineItem * line = scene->addLine(QLineF(top, bottom));
    line->setPen(QPen(gridcolor));
  }
  if (ylab.length() > 0) {
    QGraphicsTextItem *ylabtext = scene->addText(ylab);
    QPointF ylabPos = topLeft +
      QPointF((-((double)cw * 1.5)) * DPI,
              (((double)(h * cw) / 2.0) + (cw / 2.0)) * DPI);
    ylabtext->setPos(ylabPos);
    ylabtext->setRotation(270);
    setFontPointSize(ylabtext, FONTSIZE);
  }
}

void ModelInspector::
draw_rectangle(QPointF topLeft, QSize size, QColor color) {

  QPen spen;
  spen.setColor(color.darker(110));
  spen.setWidth(6);
  QBrush rbrush(color);

  QGraphicsRectItem *rect =
    scene->addRect(topLeft.x(), topLeft.y(), size.width(), size.height());
  rect->setPen(spen);
  rect->setBrush(rbrush);

}

void ModelInspector::
draw_vertical_double_arrow(QPointF top, QPointF bottom, QColor color) {

  QPen spen(color);
  spen.setWidth(10);

  int learrow = 20;

  QGraphicsLineItem * tline =
    scene->addLine(QLineF(QPointF(top.rx() - learrow, top.ry()),
                          QPointF(top.rx() + learrow, top.ry())));
  tline->setPen(spen);

  QGraphicsLineItem * line = scene->addLine(QLineF(top, bottom));
  line->setPen(spen);

  QGraphicsLineItem * bline =
    scene->addLine(QLineF(QPointF(bottom.rx() - learrow, bottom.ry()),
                          QPointF(bottom.rx() + learrow, bottom.ry())));
  bline->setPen(spen);

}

void ModelInspector::
draw_horizontal_double_arrow(QPointF left, QPointF right, QColor color) {

  QPen spen(color);
  spen.setWidth(10);

  int learrow = 20;

  QGraphicsLineItem * lline =
    scene->addLine(QLineF(QPointF(left.rx(), left.ry() - learrow),
                          QPointF(left.rx(), left.ry() + learrow)));

  lline->setPen(spen);

  QGraphicsLineItem * line = scene->addLine(QLineF(left, right));
  line->setPen(spen);

  QGraphicsLineItem * rline =
    scene->addLine(QLineF(QPointF(right.rx(), right.ry() - learrow),
                          QPointF(right.rx() , right.ry() + learrow)));

  rline->setPen(spen);

}

void ModelInspector::
draw_horizontal_register_array_label(const Model & m, register_atom max,
                                     QPointF & topLeft, bool nullReg) {
  vector<register_atom> RA;
  if (nullReg) RA.push_back(-1);
  for (register_atom ra = 0; ra < max; ra++) RA.push_back(ra);

  // x label (register atoms)
  for (register_atom a : RA) {
    QPointF topClp = topLeft + (QPointF(a, 0) * DPI);
    draw_atom_label(m, topClp, a);
  }
}

void ModelInspector::
draw_atom_label(const Model & m, QPointF & topClp, register_atom a) {

  QPointF atomicRegNamePos = topClp + (QPointF(0.1, -1.4) * DPI);
  QString name = (a == -1 ? "null" : m.input->atomname[a].c_str());
  QGraphicsTextItem *nl = scene->addText(name);
  nl->setPos(atomicRegNamePos);
  setFontPointSize(nl, FONTSIZE);

  QPointF atomNumberPos = topClp + (QPointF(0.2, -0.8) * DPI);
  QGraphicsTextItem *l = scene->addText(QString("%1").arg(a));
  l->setPos(atomNumberPos);
  setFontPointSize(l, FONTSIZE);
}


QString ModelInspector::
show_global_congruence(const Model & m, global_congruence g) {
  congruence c = m.input->regular[g];
  vector<QString> ps;
  for (operand p : m.input->congr[c]) ps.push_back("p" + QString::number(p));
  return "{" + sepBy(ps, QString(", ")) + "}";
}

QString ModelInspector::sign(const BoolVar & v) {
  if (!v.assigned()) return "?";
  if (v.val()) return "1"; else return "0";
}

vector<temporary>
ModelInspector::temporaries(const Model& m, block b) const {
  if (show_dead_temporaries) return m.input->tmp[b];
  vector<temporary> ts;
  for (temporary t : m.input->tmp[b])
    if (!m.is_dead(t)) ts.push_back(t);
  return ts;
}

vector<operation>
ModelInspector::operations(const Model& m, block b) const {
  if (show_inactive_operations) return m.input->ops[b];
  vector<operation> os;
  for (operation o : m.input->ops[b])
    if (!m.is_inactive(o)) os.push_back(o);
  return os;
}

vector<operand>
ModelInspector::operands(const Model& m, block b) const {
  if (show_inactive_operands) return m.input->ope[b];
  vector<operand> ps;
  for (operand p : m.input->ope[b])
    if (!m.is_inactive(m.input->oper[p])) ps.push_back(p);
  return ps;
}
