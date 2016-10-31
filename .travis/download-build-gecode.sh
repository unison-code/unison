#!/bin/bash
wget http://www.gecode.org/download/gecode-5.0.0.tar.gz
tar -zxf gecode-5.0.0.tar.gz
cd gecode-5.0.0
./configure --disable-examples --disable-float --disable-flatzinc
make
cd -
