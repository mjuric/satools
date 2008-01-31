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
	print "Create .sdss files for runs of a given runSet, or the complete runSet\n";
	print "Usage: sa_out2SDSS.pl <runSet> [run1 [run2 [...]]]\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
open EPOCHS, $epochs or die "Cannot open $epochs file";

sub out2sdss {
	my $run = shift;
	$ret = `out2sdss.x $runSet.d/$run $runSet.d/$run`;
	print "$ret--------------\n";
}

if($#ARGV != -1) {
	for(@ARGV) { out2sdss $_; }
} else {
	while(<EPOCHS>) {
		if(substr($_, 1, 0) eq "#") { next; }

		($epoch, $runs) = split / +: +/;
		$runs =~ s/^\s*(.*?)\s*$/$1/;
		@runs = split(/ +/, $runs);

		foreach $run(@runs) {
			out2sdss $run;
		}
	}
}
