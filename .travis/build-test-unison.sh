#!/bin/bash
cd src && make build test UNISON_GHC_CONFIG="-O0" && cd -
