#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
use Time::JulianDay;
use Time::ParseDate;
use Time::CTime;

STDOUT->autoflush(1);

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }
sub mjd { ($y, $m, $d) = @_; return julian_day($y, $m, $d) - 2400000.5; }
sub openCat {
	my ($fname) = @_; my $ident;

	open(PROELE, $fname) or die "Cannot open ASTORB catalog file '$ARGV[1]'";
	print "Loading proper elements (numbered)...\n";

#
#   There's a bug in AstDys code -- this field isn't updated
#
#	<PROELE>; ($ident) = (<PROELE> =~ / version[ \t]+[0-9]+[ \t]+(.+)$/); <PROELE>;
#	$ident = parsedate($ident) or die "Cannot read the date of file (expected on line 2) from file '$ARGV[1]'";
#	return strftime("%Y%m%d", localtime($ident));

	return $catid;
}

print "Utility for updating asteroid.sdss database with ASTORB osculating elements\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if($#ARGV < 1) {
	print "Usage : updateOE.pl <runSet> <proele> [run1 [run2 [...]]]\n";
	exit;
}

$runSet = shift;
$catid = shift;

# load
$identN = "ASTDYS_N_" . openCat "$ws/catalogs/allnum.pro.$catid";
while(defined($s = <PROELE>)) {
	($num, $ap, $ep, $sinip) = split(/ +/, $s);
	$ast{$num} = { ap => $ap, ep => $ep, sinip => $sinip, ident => $identN };

	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}
print " [$cnt]\n";

$identU = "ASTDYS_U_" . openCat "$ws/catalogs/ufitobs.pro.$catid";
while(defined($s = <PROELE>)) {
	($desig, $ap, $ep, $sinip) = split(/ +/, $s);
	$ast{$desig} = { ap => $ap, ep => $ep, sinip => $sinip, ident => $identU };

	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}
print " [$cnt]\n";

sub updatePE {
	$db = shift;

	# open .sdds file
	$dbtmp = "$db.tmp";
	print "Updating proper elements...\n";
	open(DB, $db) or die "Cannot open SDSS file '$ARGV[0]'";
	open(TMP, ">$dbtmp") or die "Cannot open temporary file '$ARGV[0].tmp'";
	$cnt = 0;
	while(defined($s=<DB>)) {
		($dummy, $num, $desig) = split(/ +/, substr($s, 243));
		if($desig ne "-" and $desig ne "" and substr($s, 0, 1) ne "#") {
			$desig =~ s/_//g;
			if(defined($r = $ast{$desig}) || defined($r = $ast{$num})) {
				for $k (keys %{$r}) { ${$k} = ${$r}{$k} } # expand to local namespace

				$r = sprintf "%-20s %12.8f %10.8f %10.6f", $ident, $ap, $ep, $sinip;
				$rep++;
			} else {
				$r = "-                      0.00000000 0.00000000   0.000000";
			}
			substr($s, 462, length $r) = $r;
		}

		if(!(++$cnt % 1000)) { print "#"; }
		if(!($cnt % 50000)) { print " [$cnt]\n"; }
		
		print TMP "$s";
	}
	print " [$cnt]\n";

	close DB; close TMP;

	$bkp = "$db." . time . ".peupdate";

#	rename $db, $bkp or die "Cannot rename $db to $bkp. Aborting.\n";
	rename $dbtmp, $db or die "Cannot rename $dbtmp to $db. Aborting.\n";

	print "Updating complete. $rep of $cnt asteroids have proper elements.\n";
#	print "Old SDSS file has been backed up as $bkp.\n";
}

if($#ARGV == -1) { # complete runSet
	updatePE "$ws/output/$runSet.sdss";
} else {
	foreach (@ARGV) {
		updatePE "$ws/output/$runSet.d/$_.sdss";
	}
}
