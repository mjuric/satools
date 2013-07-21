#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

# Find the number of cores in this machine (note: assuming we're running on Linux)
$MAX_PARALLEL=`grep "^processor" /proc/cpuinfo | wc -l` + 0;

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);
$| = 1;

if($#ARGV < 0) {
	print "Create caches for a given runSet\n";
	print "Usage: sa_createCaches.pl <runSet> [run1 [run2 [...]]]\n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
open EPOCHS, $epochs or die "Cannot open $epochs file";

sub createCache {
	my $run = shift;

	$cache = "$ws/tmp/skies/$run.cache";
#	if(-f $cache) { print "Cache for $run exists. Using cached copy.\n"; return; }

	system("createcache.x $run $run $run &");
	print "$ret\n--------------\n";
}

if($#ARGV != -1) {
	for(@ARGV) { createCache $_; }
} else {
	while(<EPOCHS>) {
		if(substr($_, 1, 0) eq "#") { next; }

		($epoch, $runs) = split / +: +/;
		$runs =~ s/^\s*(.*?)\s*$/$1/;
		@runs = split(/ +/, $runs);

		foreach $run(@runs) {
			do {
				$nrunning = `/sbin/pidof createcache.x | wc | awk '{print \$2}'`;
				#print "Number of instances running: $nrunning\n";
				if($nrunning >= $MAX_PARALLEL) { sleep 1; }
			} while($nrunning >= $MAX_PARALLEL);
			createCache $run;
		}
	}
}
