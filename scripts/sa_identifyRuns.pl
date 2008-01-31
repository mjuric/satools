#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);

if($#ARGV < 1) {
	print "Identify asteroids in a given runSet. If explicit runs are not listed, the whole runset is identified.\n";
	print "Usage: sa_identifyRuns.pl <runSet> <radius> [run1 [run2 [...]]]\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
$runSetDir = $runSet . ".d";
open EPOCHS, $epochs or die "Cannot open $epochs file";
$radius = shift;

`mkdir -p $ws/output/$runSetDir`;

sub identifyRun {
	my $run = shift;
	print "id2.x $runSetDir/$run $radius $runSetDir/$run $run $run CAT\n";
	$ret = `id2.x $runSetDir/$run $radius $runSetDir/$run $run $run CAT`;
	print "$ret-------------\n";
}

if($#ARGV != -1) {
	for(@ARGV) { identifyRun $_; }
} else {
	while(<EPOCHS>) {
		($epoch, $runs) = split / +: +/;
		$runs =~ s/^\s*(.*?)\s*$/$1/;
		@runs = split(/ +/, $runs);

		foreach $run(@runs) {
			identifyRun $run;
		}
	}
}
