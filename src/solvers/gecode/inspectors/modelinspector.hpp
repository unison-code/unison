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


#ifndef __MODEL_INSPECTOR__
#define __MODEL_INSPECTOR__

#include <deque>

#include <QtGui>

#include <gecode/gist.hh>
#include <graphviz/gvc.h>

#include "models/model.hpp"
#include "common/definitions.hpp"
#include "common/util.hpp"
#include "modelgraphicsview.hpp"
#include "dot.hpp"

// Please do not ask
#pragma GCC diagnostic ignored "-Wall"
#define QLOCATION "\0" __FILE__ ":" QTOSTRING(__LINE__)
#pragma GCC diagnostic warning "-Wall"

// Gives a string with s filled with character c until occupying width n
QString fill(QString s, int n, QChar c = ' ');

class ModelInspector : public Gecode::Gist::Inspector {

  static const bool show_dead_temporaries = false;
  static const bool show_inactive_operations = false;
  static const bool show_inactive_operands = false;

public:

  QGraphicsScene* scene;
  ModelGraphicsView* view;
  QMainWindow* mw;
  QColor gridcolor;
  bool first;
  double DPI;
  int FONTSIZE;
  vector<QColor> typeColor;
  vector<QColor> globalCongruenceColor;

  // Constructor
  ModelInspector(void);

  // Set up main window
  void initialize(void);

  // Show window, fit the first time it is shown
  void show_window(QRectF boundRect);

  // Finalize inspector
  virtual void finalize(void);

  // Give the name of a temporary
  QString emit_temporary(temporary t);

  // Give the color of an operation
  QColor type_color(int type);

  // Give the color of a congruence defined by its representative operand
  QColor congruence_color(operand p);

  // Set font size of the given text item
  void setFontPointSize(QGraphicsTextItem * t, int pointSize);

  // Draw control-flow graph getEdges()
  void draw_cfg_edges(Dot& cfg);

  // Draw a grid of width w and height h
  void draw_grid(unsigned int w, unsigned int h, QPointF & topLeft,
                 QString ylab = QString(""), unsigned int cw = 1);

  // Draw a rectangle filled with the grid color
  void draw_rectangle(QPointF topLeft, QSize size, QColor color);

  // Draw a vertical double-arrow
  void draw_vertical_double_arrow(QPointF top, QPointF bottom, QColor color);

  // Draw a horizontal double-arrow
  void draw_horizontal_double_arrow(QPointF left, QPointF right, QColor color);

  // Draw the horizontal register array label
  void draw_horizontal_register_array_label(const Model & m, QPointF & topLeft,
                                            bool nullReg = false);

  // Draw register atom label for array label
  void draw_atom_label(const Model & m, QPointF & topClp, register_atom a);

  // Show textual form of global congruence g
  QString show_global_congruence(const Model& m, global_congruence g);

  // Show textual form of Boolean variable
  QString sign(const BoolVar & v);

  // Gives the temporaries to be inspected for the given block
  vector<temporary> temporaries(const Model& m, block b) const;

  // Gives the operations to be inspected for the given block
  vector<operation> operations(const Model& m, block b) const;

  // Gives the operands to be inspected for the given block
  vector<operand> operands(const Model& m, block b) const;

};

#endif
