#!/bin/sh
#-*-sh-*-

#
# Copyright © 2009 CNRS, INRIA, Université Bordeaux 1
# Copyright © 2010 Cisco Systems, Inc.  All rights reserved.
# See COPYING in top-level directory.
#

# Check the conformance of `lstopo' for all the XML
# hierarchies available here.  Return true on success.

HWLOC_top_builddir="/home/david/work/current/development/src/Dlighthouse/btoserver/bto/hardware_profiling/hwloc-1.0.2"
HWLOC_top_srcdir="/home/david/work/current/development/src/Dlighthouse/btoserver/bto/hardware_profiling/hwloc-1.0.2"
lstopo="/home/david/work/current/development/src/Dlighthouse/btoserver/bto/hardware_profiling/hwloc-1.0.2/utils/lstopo"

error()
{
    echo $@ 2>&1
}

if [ ! -x "$lstopo" ]
then
    error "Could not find executable file \`$lstopo'."
    exit 1
fi


: ${TMPDIR=/tmp}
{
  tmp=`
    (umask 077 && mktemp -d "$TMPDIR/fooXXXXXX") 2>/dev/null
  ` &&
  test -n "$tmp" && test -d "$tmp"
} || {
  tmp=$TMPDIR/foo$$-$RANDOM
  (umask 077 && mkdir "$tmp")
} || exit $?
file="$tmp/lstopo_xml.output.xml"

set -e
$lstopo --xml "$1" "$file"
diff -u "$1" "$file"
if [ -n "" ]
then
  cp "$HWLOC_top_srcdir"/src/hwloc.dtd "$tmp/"
   --valid "$file" > /dev/null
fi
rm -rf "$tmp"
