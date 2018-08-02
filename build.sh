#!/usr/bin/env bash
rm -f ./scripts/test
cobc -x -free -o scripts/test scripts/testdriver.cbl
./scripts/test