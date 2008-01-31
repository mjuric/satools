#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
use POSIX;
STDOUT->autoflush(1);

if($#ARGV < 0) {
	print "Checks the existence of run_geometry entries for runs of a given runSet\n";
	print "Usage: sa_checkRunGeometry.pl <runSet>\n";
	exit -1;
}

# load geometries
$geom = "$ws/lib/run_geometry.txt";
open GEOM, $geom or die "Cannot open run geometry file $geom";
<GEOM>;
while(<GEOM>) {
	s/^\s*(.*?)\s*$/$1/;
	($run) = split / +/;
	$geom{$run} = 1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
open EPOCHS, $epochs or die "Cannot open $epochs file";

while(<EPOCHS>) {
	($epoch, $runs) = split / +: +/;
	$runs =~ s/^\s*(.*?)\s*$/$1/;
	@runs = split(/ +/, $runs);

	foreach $run(@runs) {
		if(not defined $geom{$run}) {
			print "Run $run has no run_geometry entry\n";
		}
	}
}
