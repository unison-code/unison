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


#ifndef __DOT__
#define __DOT__

#include <tuple>
#include <vector>
#include <map>

#include <QtGui>
#include <graphviz/gvc.h>

using namespace std;

typedef QString NodeId;

typedef tuple<NodeId, NodeId, int> EdgeId;

EdgeId edgeId(const NodeId& source, const NodeId& target, int instance = 0);

EdgeId edgeId(int source, int target, int instance = 0);

#define POSITION(l) (l->pos)
#define BB(g) (GD_bb(g))
#define NDNAME(n) (agnameof(n))
#define NDCOORD(n) (ND_coord(n))
#define NDHEIGHT(n) (ND_height(n))
#define NDWIDTH(n) (ND_width(n))
#define EDLABEL(e) (ED_label(e))
#define EDTAILNAME(e) (NDNAME(agtail(e)))
#define EDHEADNAME(e) (NDNAME(aghead(e)))
#define EDSPLLIST(e) (ED_spl(e)->list)

class DotNode {
public:
  NodeId name;
  QPointF topLeftPos;
  double  width, height;
  QRectF rect;
  vector<QRectF> rects;
};

class DotEdge {
public:
  EdgeId key;
  QPointF topLeftPos;
  double width, height;
  QPainterPath path;
  QPainterPath arrow;
};

class Dot {
public:

  static const double DPI;
  static const double DOT2QT;

  Dot();
  ~Dot();

  void setGlobalGraphAttribute(const QString &attr, const QString &val);
  void setGlobalNodeAttribute(const QString &attr, const QString &val);
  void setGlobalEdgeAttribute(const QString &attr, const QString &val);
  void insert(const NodeId& name);
  void insert(const EdgeId& key);
  void setEdgeAttribute(EdgeId key, const QString &attr, const QString &val);
  void setNodeSize(const NodeId &name, double w, double h);
  void setNodeAttribute(const NodeId &name,
                        const QString &attr, const QString &val);
  void draw(void);
  void dump(FILE * file);
  QRectF box() const;
  vector<DotNode> getNodes() const;
  vector<DotEdge> getEdges() const;
  QPainterPath drawArrow(QLineF line) const;

private:
  GVC_t * context;
  Agraph_t *graph;
  map<NodeId, Agnode_t*> nodes;
  map<EdgeId, Agedge_t*> edges;

  static void gvSet(void *object, QString attr, QString value) {
    agsafeset(object, const_cast<char *>(qPrintable(attr)),
              const_cast<char *>(qPrintable(value)),
              const_cast<char *>(qPrintable(value)));
  }

  Agnode_t * gvNode(NodeId node) {
    return agnode(graph, const_cast<char *>(qPrintable(node)),1);
  }

};

#endif
