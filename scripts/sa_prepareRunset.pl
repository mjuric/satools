#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

if($#ARGV != 0) {
	print "SDSS moving object file preparation script\n";
	print "This script takes the output of moving objects SQL query and produces a series of files in /input directory, ready to be processed by sa_idenfity script\n";
	print "Author/maintainer: majuric\@astro.hr\n\n";

	print "Usage: sa_prepareRuns.pl <runSet>\n\n";

	exit(-1);
}

use Fcntl;
use IO::Handle;

STDOUT->autoflush(1);

sub round { my($number) = shift;  return int($number + .5 * ($number <=> 0)); }
sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }

$file = shift;
$input = "$ws/input/$file.tbl";
$path = "$ws/input/$file.d";
open INP, $input or die "Cannot open [$input] file. Misspeled runSet name?\n";

$cnt = 0;
while(defined($l = <INP>)) {
	trim $l;
	next if substr($l, 0, 1) eq "#";

	($run) = split(/ +/, $l, 2);

	if(not defined $runs{$run}) {$runs{$run} = ()};

	push @{$runs{$run}}, $l;
	$cnt++;
	
	if($cnt % 1000 == 0) { print "$cnt "; }
}
print "\n\n";
close TBL;

##########

print "Storing output to $path\n";

`mkdir -p $path`;

foreach $run ( sort { $b <=> $a } keys %runs) {
	$a = $runs{$run};

	$output = "$path/$run.data";
	open RUN, ">$output" or die "Cannot open [$output]. Do directories exist and have right permissions?";
	print "Run " . sprintf("%04d", $run) . " ";

	$t0 = 1E10; $t1 = 0;
	foreach $l (@{$a}) {
		print RUN "$l\n";
#		($t) = reverse(split(/ +/, $l));
		@tt = split(/ +/, $l);
		$t = $tt[26];
		if($t0 > $t) { $t0 = $t };
		if($t1 < $t) { $t1 = $t };
	}
	$t = sprintf("%06.4f", $t1 - $t0);
	$obj = sprintf("%05d", $#{$a}+1);
	print "[$t0 - $t1 : $t] $obj objects\n";

	$epoch = round($t0 / 10) * 10;

	if(not defined $times{$epoch}) { $times{$epoch} = (); }

	push @{$times{$epoch}}, $run;
}

close RUN;

#########

open E, ">$path/epochs.txt" or die "Cannot open [$path/epochs.txt] output file";

print "\n";
foreach $time (sort keys %times) {
	print "$time : ";
	print E "$time : ";
	foreach $run (@{$times{$time}}) {
		print "$run ";
		print E "$run ";
	}
	print "\n";
	print E "\n";
}

close E;

#########

# CHECK stage

undef $failed;

print "\n";

print "- - - -   make check   - - - -\n\n";

if(not $failed) {
	print "1. Epoch file exists?\n";
	$epochs = "$path/epochs.txt";
	if(-f $epochs) {
		print "\tYes.\n";
	} else {
		$failed = "Cannot open $epochs file (misspelled runset name ?)"
	}
}

if(not $failed) {
	print "2. Runs exist in runList?\n";
	open RUNLIST, "$ws/lib/run_geometry.txt" or die "Cannot open runlist file $ws/lib/run_geometry.txt\n";
	while($_ = <RUNLIST>) {
		next if /^#.*$/;
		($run) = /^\s*(\d+)\s.*$/;
		$rl{$run} = 1;
	}
	close RUNLIST;

	foreach $run ( keys %runs) {
		next if exists $rl{$run};
	
		print "\t==> Run $run missing.\n";
		$failed = "Add the necessary runs to $ws/lib/run_geometry.txt and try again.";
	}
}

die "\n- - - -   check FAILED\n\nSuggested remedy: $failed.\n" if $failed;
print "Passed.\n";

0;
