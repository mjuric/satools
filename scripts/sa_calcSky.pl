#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
STDOUT->autoflush(1);

# calcsky.x out.txt 52705.2222 20020912.52705 native

if($#ARGV < 1) {
	print "Calculate skies for a given runset and store it in a set of SM readable files.\n";
	print "Usage: sa_calcSky.pl <runSet> <astorb_date> \n";
	exit -1;
}

$runSet = shift; $epochs = "$ws/input/$runSet.d/epochs.txt";
$astorb = shift;
$runSetDir = "$ws/output/$runSet.d";
open EPOCHS, $epochs or die "Cannot open $epochs file";

use Time::JulianDay;
sub mjd { ($y, $m, $d) = @_; return julian_day($y, $m, $d) - 2400000.5; }

while(<EPOCHS>) {
	($epoch, $runs) = split / +: +/;
	$runs =~ s/^\s*(.*?)\s*$/$1/;
	@runs = split(/ +/, $runs);

	foreach $run(@runs) {
		# the date should be encoded in the run
		($y, $m, $d) = ($run =~ /(\d{1})(\d{2})(\d{2})/);
		$y+=2000;
		$mjd = mjd($y, $m, $d) - .5;

		$cmd = "calcsky.x sky.$y$m$d.txt $mjd $astorb.$epoch native";
		print "$epoch, $run, $y, $m, $d : $cmd\n";
		print `$cmd`;
	}
}
