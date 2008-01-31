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

print "Utility for adding flags from input .tbl files to output .sdss files\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if(scalar(@ARGV) < 1) {
	print "Usage : sa_addFlags.pl <runSet>\n";
	exit;
}

$runSet = shift;
$sdssFile = "$ws/output/$runSet.sdss";
$tblFile = "$ws/input/$runSet.tbl";
$outFile = "$ws/output/$runSet.sdss.tmp";

open(TBL, $tblFile) or die "Cannot open .tbl file $tblFile";
open(SDSS, $sdssFile) or die "Cannot open .sdss file $sdssFile";
open(OUT, ">$outFile") or die "Cannot open .out file $outFile";

# load tbl object identifiers and flags
print "Loading .tbl file...\n";
while(<TBL>)
{
	trim $_;
	($a, $b, $c, $d) = split /\s+/;

	$id = sprintf("%5d %1d %4d %5d", $a, $b, $c, $d);
	$flags = substr($_, -127);
	$tbl{$id} = $flags;

	#print "[$id] [$flags]\n";

	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}

$cnt = 0;
print "\nAdding flags...\n";
while(<SDSS>)
{
	chomp;
	$id = substr($_, 7, 18);
	defined $tbl{$id} or die("[$id] is not defined in .tbl file\n");
	
	print OUT "$_ " . $tbl{$id} . "\n";
	
	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}
print "\n";
close OUT;

$bkp = "$sdssFile." . time . ".addflags";
rename $sdssFile, $bkp or die "Cannot rename $sdssFile to $bkp. Aborting.\n";
rename $outFile, $sdssFile or die "Cannot rename $outFile to $sdssFile. Aborting.\n";
