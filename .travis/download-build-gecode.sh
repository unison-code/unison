#!/bin/bash
./.travis/accept-svn-certificate.sh anonymous robcasloz@gmail.com https://svn.gecode.org/svn/gecode/trunk
svn --non-interactive --username anonymous --password robcasloz@gmail.com checkout -r16106 https://svn.gecode.org/svn/gecode/trunk gecode-trunk
cd gecode-trunk
./configure --disable-examples --disable-float --disable-flatzinc
make
cd -
