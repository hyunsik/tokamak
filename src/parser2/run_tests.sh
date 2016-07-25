#!/bin/sh

cmd="target/debug/parser-tests tests out -a $@"
if [ -z $DEBUG ]
then
  $cmd
else
  rust-gdb --args $cmd
fi
