#!/usr/bin/env bash
ID=$1
rm -f ./scripts/test-${ID}
cobc -x -free -o scripts/test-${ID} scripts/testdriver-${ID}.cbl
./scripts/test-${ID}