#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);

if($#ARGV != 0) {
	print "Usage: sa_merge.pl <runSet>\n";
	exit -1;
}

$runSet = shift;

print `cd $ws/output/$runSet.d; cat *.sdss > ../$runSet.sdss`;

# @outs = split(/ +/, `cd $ws/output/$runSet.d; ls *.out`);
# $first = 1;
# foreach $i (@outs) {
#	if($first) {
#		print `cd $ws/output/$runSet.d; cat $i > ../$runSet.out`;
#		open(T, ">>$ws/output/$runSet.out") or die "Cannot open $ws/output/$runSet.out";
#	} else {
#		open(F, "$ws/output/$runSet.d/$i") or die "Cannot open $ws/output/$runSet.d/$i";
#		while(<F>) {
#			print T $_;
#		}
#		close F;
#	}
# }
# print `cd $ws/output/$runSet.d; cat *.out > ../$runSet.out`;
