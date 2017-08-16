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


#include "alignmentinspector.hpp"

std::string
GlobalAlignmentInspector::name(void) {
  return "Operand alignment";
}

void
GlobalAlignmentInspector::
inspectb(const GlobalModel & m, const DotNode & n, disjointSet<operand> & cs) {

  block b = n.name.toInt();

  vector<operand> in = m.input->operands[m.input->in[b]],
    out = m.input->operands[m.input->out[b]];

  QRectF br = n.rect;
  br.adjust(0, 5, 0, -5);
  QGraphicsRectItem *r = scene->addRect(br);
  QColor light = QColor(Qt::gray).lighter(140);
  QBrush brush(light);
  r->setBrush(brush);
  QPen pen(light);
  r->setPen(pen);

  map<operand, QRectF> rectangle;

  unsigned i = 0;
  for (; i < in.size(); i++) rectangle[in[i]] = n.rects[i];
  i++;
  unsigned j = 0;
  for (; i < in.size() + 1 + out.size(); i++, j++)
    rectangle[out[j]] = n.rects[i];

  for (alignable a : m.input->A) {
    operand p = m.input->pairs[a].first,
            q = m.input->pairs[a].second;
    if (!(m.input->pb[p] == b)) continue;
    if (m.oa(a).assigned() && !m.oa(a).val()) continue;

    QPointF top(rectangle[p].center().x(), rectangle[p].top());
    QPointF bottom(rectangle[q].center().x(), rectangle[q].bottom());
    QLineF line(top, bottom);
    QGraphicsLineItem *l = scene->addLine(line);
    QPen pen;
    pen.setWidth(m.oa(a).assigned() ? 3 : 1);
    pen.setColor(m.oa(a).assigned() ? congruence_color(cs.find(p)) : Qt::gray);
    l->setPen(pen);
  }

  for (operand p : in) {
    QGraphicsRectItem *r = scene->addRect(rectangle[p]);
    QGraphicsTextItem *l = scene->addText(QString("p%1").arg(p));
    QBrush brush(congruence_color(cs.find(p)));
    r->setBrush(brush);
    QPen pen;
    pen.setColor(congruence_color(cs.find(p)));
    r->setPen(pen);
    l->setPos(rectangle[p].bottomLeft());
    QFont f("Arial", 14);
    l->setFont(f);
  }
  for (operand q : out) {
    QGraphicsRectItem *r = scene->addRect(rectangle[q]);
    QGraphicsTextItem *l = scene->addText(QString("p%1").arg(q));
    QBrush brush(congruence_color(cs.find(q)));
    r->setBrush(brush);
    QPen pen;
    pen.setColor(congruence_color(cs.find(q)));
    r->setPen(pen);
    l->setPos(rectangle[q].bottomLeft());
    QFont f("Arial", 14);
    l->setFont(f);
  }

}

void GlobalAlignmentInspector::inspect(const Space& s) {

  const GlobalModel& m = static_cast<const GlobalModel&>(s);

  initialize();

  Dot cfg;
  cfg.setGlobalNodeAttribute("shape", "record");
  cfg.setGlobalNodeAttribute("fixedsize", "false");

  // Add getNodes() and set their size
  for (block b : m.input->B) {
    vector<operand> in  = m.input->operands[m.input->in[b]],
                    out = m.input->operands[m.input->out[b]];

    if (in.empty() && out.empty()) continue;

    QString bn = QString::number(b);
    cfg.insert(bn);

    QString nodeLabel =
      "{{" + operandsLabel(in) + "}||{" + operandsLabel(out) + "}}";

    cfg.setNodeAttribute(bn, "label", nodeLabel);
  }

  // Add getEdges()

  map<pair<block, int>, operand> op;

  int instance = 0;
  for (vector<block> e : m.input->cfg) {
    block b1 = e[0], b2 = e[1];
    for (operand p : m.input->operands[m.input->out[b1]]) {
      for (operand q : m.input->operands[m.input->in[b2]]) {
        if (m.input->operand_congruence[p] == m.input->operand_congruence[q]) {
          EdgeId key = edgeId(b1, b2, instance);
          cfg.insert(key);
          cfg.setEdgeAttribute(key, "tailport", portName(p));
          cfg.setEdgeAttribute(key, "headport", portName(q) + ":n");
          op[make_pair(b1, instance)] = p;
          op[make_pair(b2, instance)] = q;
          instance++;
        }
      }
    }
  }

  // Compute graph layout
  cfg.draw();

  // This looks funny, but without it the "rect" fields are empty!
  cfg.dump(NULL);

  // Compute congruences, taking into account alignments
  disjointSet<operand> cs;
  for (block b : m.input->B) {
    for (operand p : m.input->operands[m.input->in[b]])
      cs.insert(p, m.input->representative[m.input->operand_congruence[p]]);
    for (operand q : m.input->operands[m.input->out[b]])
      cs.insert(q, m.input->representative[m.input->operand_congruence[q]]);
  }
  for (alignable a : m.input->A)
    if (m.oa(a).assigned() && m.oa(a).val())
      cs.merge(m.input->pairs[a].first, m.input->pairs[a].second);

  // Test: for each pair of operands (p, q) in each in- and out-delimiter, if p
  // =a= q then it must be the case that p =c= q

  for (block b : m.input->B) {
    cout << "b" << b << ":" << endl;
    vector<vector<operand> > ops =
      { m.input->operands[m.input->in[b]], m.input->operands[m.input->out[b]] };
    for (int i = 0; i < 2; i++) {

      vector<operand> ps = ops[i];
      for (unsigned int i = 0; i < ps.size(); i++)
        for (unsigned int j = i + 1; j < ps.size(); j++) {
          operand p = ps[i], q = ps[j];
          if (cs.find(p) == cs.find(q)) { // p and q better are copy-related!
            cout << "\tp" << p << " =a= p" << q << ":";
            if (disjoint_sets(m.input->temps[p], m.input->temps[q])) {
              cout << " conflict!" << endl;
            } else {
              cout << " copy-related" << endl;
            }
          }
        }

    }
  }

  // End of test



  for (DotEdge edge : cfg.getEdges()) {
    QGraphicsPathItem * e = new QGraphicsPathItem(edge.path);
    block b = get<0>(edge.key).toInt();
    int instance = get<2>(edge.key);
    operand p = op[make_pair(b, instance)];
    QPen pen(congruence_color(cs.find(p)));
    pen.setWidth(3);
    e->setPen(pen);
    scene->addItem(e);
  }

  for (DotNode node : cfg.getNodes()) inspectb(m, node, cs);

  show_window(cfg.box());

}

QString GlobalAlignmentInspector::operandsLabel(vector<operand> ps) {
  vector<QString> ops;
  for (operand p : ps) ops.push_back(operandLabel(p));
  return sepBy(ops, QString("|"));
}

QString GlobalAlignmentInspector::operandLabel(operand p) {
  QString op = "p" + QString::number(p);
  return "<" + op + ">" + op;
}

QString GlobalAlignmentInspector::portName(operand p) {
  return "p" + QString::number(p);
}
