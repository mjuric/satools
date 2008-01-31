#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);

if($#ARGV < 0) {
	print "Check .sdss files of a given runSet for validity. Currently implemented sanity checks include :\n";
	print "\t1) No two same moID's in a same file\n";
	print "Usage: sa_identifyRuns.pl <runSet> [run1 [run2 [...]]]\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
$runSetDir = "$ws/output/$runSet.d";
open EPOCHS, $epochs or die "Cannot open $epochs file";

sub checkRun {
	my $run = shift;

	$sdss = "$runSetDir/$run.sdss";
	open(DB, $sdss) or die "Cannot open $sdss file\n";
	# print "Checking $sdss\n";
	$cnt = 1;
	while(<DB>) {
		$moID = substr($_, 0, 6);
		if(defined $ids{$moID}) {
			print "run $run : $moID : line $ids{$moID}, line $cnt\n";
		} else {
			$ids{$moID} = $cnt;
		}
		$cnt++;
	}
	close DB;
}

$runsChecked = 0;
if($#ARGV != -1) {
	for(@ARGV) { checkRun $_; $runsChecked++; }
} else {
	while(<EPOCHS>) {
		($epoch, $runs) = split / +: +/;
		$runs =~ s/^\s*(.*?)\s*$/$1/;
		@runs = split(/ +/, $runs);

		foreach $run(@runs) {
			checkRun $run; $runsChecked++;
		}
	}
}

print "$runsChecked runs checked\n";
