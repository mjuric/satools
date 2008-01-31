#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
use Time::JulianDay;

STDOUT->autoflush(1);

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }
sub mjd { ($y, $m, $d) = @_; return julian_day($y, $m, $d) - 2400000.5; }

print "Utility for updating .sdss files with current ASTORB osculating elements\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if($#ARGV < 1) {
	print "Usage : updateOE.pl <runSet> <astorb> [run1 [run2 [...]]]\n";
	exit;
}

$runSet = shift;
$astorbid = shift;
$astorbfile = "$ws/catalogs/astorb.dat.$astorbid";
open(ASTORB, $astorbfile) or die "Cannot open ASTORB catalog file $astorbfile";

# load up the astorb hash
print "Loading ASTORB...\n";
while(defined ($s = <ASTORB>)) {
	($numeration, $designation) = ($s =~ /([ 0-9]{6}) (.{18}).*$/);
	trim $designation; $designation =~ tr/ /_/;
	($epoch, $M, $aop, $lan, $inc, $e, $a) = split(/ +/, substr($s, 106));
	($H, $G) = (substr($s, 41) =~ / *(\S+) +(\S+)/);
	($arc) = (substr($s, 94, 5) =~ / *([0-9]*)/);

	($y, $m, $d) = ($epoch =~ /([0-9]{4})([0-9]{2})([0-9]{2})/);
	$epoch = mjd $y, $m, $d;

###	$cntx++; if($cntx == 10000) { print '[' . substr($s, 41) . "] $H, $G\n"; exit; }

	$astorb{$designation} = { num => $numeration, epoch => $epoch, M => $M, aop => $aop, lan => $lan, inc => $inc, e => $e, a => $a, H => $H, G => $G, arc => $arc };

	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}
print "\n";

# identification string
$ident = "ASTORB_$astorbid";

# update SDSS files

sub updateOE {
	$db = shift;

	# open .sdss file
	$dbtmp = "$db.tmp";
	print "Updating orbital elements...\n";
	open(DB, $db) or die "Cannot open SDSS file $db";
	open(TMP, ">$dbtmp") or die "Cannot open temporary file $dbtmp";
	$cnt = 0;
	while(defined($s = <DB>)) {
		($desig) = split(/ +/, substr($s, 252), 2);
		if($desig ne "-" and $desig ne "" and substr($s, 0, 1) ne "#") {
			if(defined($r = $astorb{$desig})) {
				for $k (keys %{$r}) { ${$k} = ${$r}{$k} } # expand to local namespace

				# update OE
				$r = sprintf "%-20s %5.2f %4.2f %5.0f  %12.6f %12.8f %10.8f %10.6f %10.6f %10.6f %10.6f ", $ident, $H, $G, $arc, $epoch, $a, $e, $inc, $lan, $aop, $M;
				$rep++;

				# use this occasion to update numeration
				if($num) { substr($s, 244, 7) = sprintf("%7d", $num); }
			} else {
				if(substr($desig, 0, 1) ne "s") {
					print "--> $s\n";
					print "\n$desig [line $cnt] not in ASTORB\n"; next;
				}
				$r = "-                     0.00 0.00     0      0.000000   0.00000000 0.00000000   0.000000   0.000000   0.000000   0.000000";
			}
			substr($s, 341, length $r) = $r;
		}

		if(!(++$cnt % 1000)) { print "#"; }
		if(!($cnt % 50000)) { print " [$cnt]\n"; }
		
		print TMP "$s";
	}
	print "\n";

	close DB; close TMP;

	$bkp = "$db." . time . ".oeupdate";

#	rename $db, $bkp or die "Cannot rename $db to $bkp. Aborting.\n";
	rename $dbtmp, $db or die "Cannot rename $dbtmp to $db. Aborting.\n";

	print "Updating completed. $rep asteroids updated.\n";
#	print "Old SDSS file has been backed up as $bkp.\n";
}

if($#ARGV == -1) { # complete runSet
	updateOE "$ws/output/$runSet.sdss";
} else {
	foreach (@ARGV) {
		updateOE "$ws/output/$runSet.d/$_.sdss";
	}
}
