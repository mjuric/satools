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
	print "Propagate complete runSet\n";
	print "Usage: propagateRuns.pl <runSet> <astorbDate>\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
$astorb = shift;

# propagate
if(1) {
$cmd = "propagate.x $runSet $astorb|";
open PROP, $cmd;
PROP->autoflush(1);
while(<PROP>) { print; }
close PROP;
}

# create links
open EPOCHS, $epochs or die "Cannot open $epochs file";
while(<EPOCHS>) {
	($epoch, $runs) = split / +: +/;
	$runs =~ s/^\s*(.*?)\s*$/$1/;
	@runs = split(/ +/, $runs);

	$output = "$ws/tmp/propagated/$astorb.$epoch.obj";

	foreach $run(@runs) {
		print "\tLinking run $run -> epoch $epoch\n";
		`ln -sf $output $ws/tmp/propagated/$run.obj`;
	}
}
close EPOCHS;
