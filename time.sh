#!/bin/bash
echo "foldM version"
time stack exec slow-frp fold > /dev/null

echo
echo

echo "reactive-banana version"
time stack exec slow-frp reactive > /dev/null
