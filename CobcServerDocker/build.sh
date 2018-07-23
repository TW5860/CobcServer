#!/usr/bin/env bash
cobc -x -free -o scripts/test scripts/testdriver.cbl
./scripts/test
