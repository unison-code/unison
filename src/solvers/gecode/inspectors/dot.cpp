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


#include "dot.hpp"

EdgeId edgeId(const NodeId& source, const NodeId& target, int instance) {
  return make_tuple(source, target, instance);
}

EdgeId edgeId(int source, int target, int instance) {
  return make_tuple(QString::number(source),
                    QString::number(target),
                    instance);
}

const double Dot::DPI = 96.0;
const double Dot::DOT2QT = DPI/72.0;

Dot::Dot() :
        context(gvContext()),
        graph(agopen(const_cast<char *>("foo"), Agdirected, NULL))
{
    QLocale::setDefault(QLocale::German);
    setGlobalGraphAttribute("pad", "0,2");
    setGlobalNodeAttribute("shape", "box");
    setGlobalNodeAttribute("width", "0.2");
    setGlobalNodeAttribute("height", "0.2");
    setGlobalNodeAttribute("fixedsize", "true");
    QString w("%1");
    w.arg(50/DPI);
    setGlobalNodeAttribute("width", w.replace('.', ","));
    setGlobalEdgeAttribute("dir", "forward");
}

Dot::~Dot() {
    gvFreeLayout(context, graph);
    agclose(graph);
    gvFreeContext(context);
}

void Dot::setGlobalGraphAttribute(const QString &attr, const QString &val) {
  gvSet(graph, attr, val);
}

void Dot::setGlobalNodeAttribute(const QString &attr, const QString &val) {
  agattr(graph, AGNODE, const_cast<char *>(qPrintable(attr)),
         const_cast<char *>(qPrintable(val)));
}

void Dot::setGlobalEdgeAttribute(const QString &attr, const QString &val) {
  agattr(graph, AGEDGE, const_cast<char *>(qPrintable(attr)),
         const_cast<char *>(qPrintable(val)));
}

void Dot::insert(const NodeId& name) {
  nodes[name] = gvNode(name);
}

void Dot::insert(const EdgeId& key) {
  NodeId source = get<0>(key), target = get<1>(key);
  assert(nodes.count(source));
  assert(nodes.count(target));
  edges[key] = agedge(graph, nodes[source], nodes[target], const_cast<char *>(""), 1);
  if (source == target) setEdgeAttribute(key, "dir", "back");
}

void Dot::
setEdgeAttribute(EdgeId key, const QString &attr, const QString &val) {
  gvSet(edges[key], attr, val);
}

void Dot::setNodeSize(const NodeId &name, double w, double h) {
  setNodeAttribute(name, "width", QString("%1").arg(w));
  setNodeAttribute(name, "height", QString("%1").arg(h));
}

void Dot::setNodeAttribute(const NodeId &name,
                           const QString &attr, const QString &val) {
  gvSet(nodes[name], attr, val);
}

void Dot::draw() {
    gvFreeLayout(context, graph);
    gvLayout(context, graph, "dot");
}

void Dot::dump(FILE * file) {
  gvRender(context, graph, "dot", file);
}

QRectF Dot::box() const {
    return QRectF(BB(graph).LL.x*DOT2QT, BB(graph).LL.y*DOT2QT,
                  BB(graph).UR.x*DOT2QT, BB(graph).UR.y*DOT2QT);
}

vector<DotNode> Dot::getNodes() const {
  vector<DotNode> dotNodes;
  for (auto node : nodes) {
        DotNode gvnode;
        gvnode.name=NDNAME(node.second);
        gvnode.height = NDHEIGHT(node.second)*DPI;
        gvnode.width = NDWIDTH(node.second)*DPI;
        qreal x = NDCOORD(node.second).x*DOT2QT,
              y = (BB(graph).UR.y - NDCOORD(node.second).y)*DOT2QT;
        gvnode.topLeftPos = QPointF(x - gvnode.width/2.0, y - gvnode.height/2.0);
        gvnode.rect =
          QRectF(gvnode.topLeftPos, QSizeF(gvnode.width, gvnode.height));
        QStringList rects =
          QString(agget(node.second, const_cast<char *>("rects")))
          .split(" ", QString::SkipEmptyParts);
        for (QString r : rects) {
          QStringList rcs = r.split(",", QString::SkipEmptyParts);
          qreal x1=rcs[0].toDouble() * DOT2QT;
          qreal y1=(BB(graph).UR.y - rcs[1].toDouble()) * DOT2QT;
          QPointF bottomLeftPos(x1, y1);
          qreal x2=rcs[2].toDouble() * DOT2QT;
          qreal y2=(BB(graph).UR.y - rcs[3].toDouble()) * DOT2QT;
          QPointF topRightPos(x2, y2);
          QRectF rect(bottomLeftPos, topRightPos);
          gvnode.rects.push_back(rect);
        }
        dotNodes.push_back(gvnode);
    }
    return dotNodes;
}

vector<DotEdge> Dot::getEdges() const {
  vector<DotEdge> dotNodes;
  for (auto edge : edges) {
    DotEdge gvedge;
    if (EDLABEL(edge.second) != NULL) {
      textlabel_t * lab = EDLABEL(edge.second);
      qreal x = POSITION(lab).x * DOT2QT,
            y = (BB(graph).UR.y - POSITION(lab).y) * DOT2QT;
      gvedge.height=lab->dimen.y*DOT2QT;
      gvedge.width=lab->dimen.x*DOT2QT;
      gvedge.topLeftPos=QPointF(x - gvedge.width/2.0, y - gvedge.height/2.0);
    }
    gvedge.key =
      edgeId(EDTAILNAME(edge.second), EDHEADNAME(edge.second), get<2>(edge.first));
    if ((EDSPLLIST(edge.second)!=0) && (EDSPLLIST(edge.second)->size%3 == 1)) {
      QPointF firstCurvePoint(EDSPLLIST(edge.second)->list[0].x,
                              BB(graph).UR.y - EDSPLLIST(edge.second)->list[0].y);
      firstCurvePoint *= DOT2QT;
      if (EDSPLLIST(edge.second)->sflag) {
        gvedge.path.moveTo(EDSPLLIST(edge.second)->sp.x*DOT2QT,
                           (BB(graph).UR.y - EDSPLLIST(edge.second)->sp.y)*DOT2QT);
        QPointF previousPoint = gvedge.path.currentPosition();
        gvedge.path.lineTo(firstCurvePoint);
        QLineF line(firstCurvePoint, previousPoint);
        gvedge.arrow = drawArrow(line);
      } else {
        gvedge.path.moveTo(firstCurvePoint);
      }
      for (int i = 1; i < EDSPLLIST(edge.second)->size; i += 3) {
        QPointF c1(EDSPLLIST(edge.second)->list[i].x,
                   BB(graph).UR.y - EDSPLLIST(edge.second)->list[i].y);
        c1 *= DOT2QT;
        QPointF c2(EDSPLLIST(edge.second)->list[i+1].x,
                   BB(graph).UR.y - EDSPLLIST(edge.second)->list[i+1].y);
        c2 *= DOT2QT;
        QPointF
          endPoint(EDSPLLIST(edge.second)->list[i+2].x,
                   BB(graph).UR.y - EDSPLLIST(edge.second)->list[i+2].y);
        endPoint *= DOT2QT;
        gvedge.path.cubicTo(c1, c2, endPoint);
      }
      if (EDSPLLIST(edge.second)->eflag) {
        QPointF previousPoint = gvedge.path.currentPosition();
        QPointF lastPoint(EDSPLLIST(edge.second)->ep.x,
                          BB(graph).UR.y - EDSPLLIST(edge.second)->ep.y);
        lastPoint *= DOT2QT;
        gvedge.path.lineTo(lastPoint);
        QLineF line(previousPoint, lastPoint);
        gvedge.arrow = drawArrow(line);
      }
    }
    dotNodes.push_back(gvedge);
  }
  return dotNodes;
}

QPainterPath Dot::drawArrow(QLineF line) const {
  QPainterPath path;
  QLineF normal = line.normalVector();
  QPointF head(normal.dx(), normal.dy());
  head /= 2.0;
  path.moveTo(line.p1());
  QPolygonF arrow;
  arrow.append(line.p1() + head);
  arrow.append(line.p2());
  arrow.append(line.p1() - head);
  arrow.append(line.p1() + head);
  path.addPolygon(arrow);
  return path;
}
