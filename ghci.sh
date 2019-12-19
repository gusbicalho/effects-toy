#! /usr/bin/env bash
PACKAGE="$1"
stack ghci "$PACKAGE:lib" "$PACKAGE:$PACKAGE-test"
