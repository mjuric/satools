#!/usr/bin/perl

use Getopt::Std;
getopts('h');

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);

if($#ARGV < 1) {
	$runSet = "<runSet>";
	$catid = "<catalogDate>";
} else {
	($runSet, $catid) = @ARGV;
}

@commands = (
#	{ cmd => "sa_prepareRunset.pl $runSet"}, 
#	{ cmd => "sa_propagateRunset.pl $runSet $catid"},	# comment this out once caches are made
	{ cmd => "sa_createCaches.pl $runSet"},			# comment this out once caches are made
	{ cmd => "sa_identifyRuns.pl $runSet 30"},
	{ cmd => "sa_out2SDSS.pl $runSet"},
	{ cmd => "sa_merge.pl $runSet"},
	{ cmd => "sa_updateOE.pl $runSet $catid"},
	{ cmd => "sa_updatePE.pl $runSet $catid"},
	{ cmd => "sa_updateApp.pl $runSet"},
	{ cmd => "sa_addFlags.pl $runSet"}
	{ cmd => "sa_sanityCheck.pl $runSet"},
	{ cmd => "sa_unique.pl $runSet"},
	{ cmd => "sa_splitByQuality.pl $runSet"}
);

#
# Argument checking
#

if($#ARGV < 1) {
	print "Do complete runset processing.\n";
	print "Usage: sa_processRunset.pl <runSet> <catalogDate> [<+|-> stage1 [stage2 [...]]]\n";

	print "Stages Available:\n";
	for(@commands) {
		printf "%2d  $_->{cmd}\n", $n;
		$n++;
	}

	exit -1;
}

$epochs = "$ws/input/$runSet.d/epochs.txt";
@catalogs = (
	"$ws/catalogs/astorb.dat.$catid",
	"$ws/catalogs/allnum.pro.$catid",
	"$ws/catalogs/ufitobs.pro.$catid"
);

for (@catalogs) { die "Cannot open $_ catalog.\nMisspelled catalog name?\n" if not -f $_; }

#
# Pipeline execution
#

sub execute {
	my $command = shift;
	my $cmd = "$command|";
	print "  - - - - - - - - - - - - - - - - - - - - -\n";
	print "| $command\n";
	print "  - - - - - - - - - - - - - - - - - - - - -\n";
	print "\n";
	open PROP, $cmd;
	while(<PROP>) { print "\t$_"; }
	print "\n";

	if(not close PROP) {
		die "Error trying to execute $command [$!]\n" if $!;
		die "Error executing $command\n";
	}
}

#
# Processing
#

#open(STDERR, ">&STDOUT") or die "Cannot dupe";
#select(STDOUT) or die "Cannot select"; $| = 1;

for(@commands) {
	execute $_->{'cmd'};
}

# execute "sa_prepareRunset.pl $runSet";
#
# die "Cannot open $epochs file (misspelled runset name ?)" if not -f $epochs;
# 
# execute "sa_propagateRunset.pl $runSet $catid";
# execute "sa_createCaches.pl $runSet";
# execute "sa_identifyRuns.pl $runSet 30";
# execute "sa_out2SDSS.pl $runSet";
# execute "sa_merge.pl $runSet";
# execute "sa_updateOE.pl $runSet $catid";
# execute "sa_updatePE.pl $runSet $catid";
# execute "sa_updateApp.pl $runSet";
# execute "sa_sanityCheck.pl $runSet";
# execute "sa_unique.pl $runSet";
