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
	print "Propagate runs\n";
	print "Usage: propagateRun.pl <runSet> <astorbDate> <run1> [run2 [run3 [...]]]\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
open EPOCHS, $epochs or die "Cannot open $epochs file";
$astorb = shift;

while(<EPOCHS>) {
	($epoch, $runs) = split / +: +/;
	$runs =~ s/^\s*(.*?)\s*$/$1/;
	@runs = split(/ +/, $runs);

	# filter out only requested runs
	for $r (@runs) {
		for $i (@ARGV) {
			if($i == $r) { push @r, $r; last; }
		}
	}
	@runs = reverse @r; undef @r;
#	print "[@runs] $#runs\n";
#	next;

	$output = "$ws/tmp/propagated/$astorb.$epoch.obj";

	if($skips != $#runs + 1) {
		print "Propagating to $epoch... ";
		$cmd = "propagate.x $astorb.$epoch $epoch $astorb|";
#		print "$cmd\n";
		open PROP, $cmd;
		PROP->autoflush(1);
		while(<PROP>) { print; }
		print "...done\n";
	}

	foreach $run(@runs) {
		print "\tLinking run $run -> epoch $epoch\n";
		`ln -sf $output $ws/tmp/propagated/$run.obj`;
	}
}
