#!/bin/bash
wget https://github.com/Gecode/gecode/archive/release-6.0.0.tar.gz
tar -xvzf release-6.0.0.tar.gz
cd gecode-release-6.0.0 && ./configure --disable-examples --disable-float --disable-flatzinc && make && cd -
