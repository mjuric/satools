#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;

STDOUT->autoflush(1);

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }

print "Utility for updating apparence counts in .sdss file\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if($#ARGV != 0) {
	print "Usage : updateApp.pl <runset>\n";
	exit;
}

$runSet = shift;
if(substr($runSet, -4) ne "sdss" and substr($runSet, -3) ne "dat")
{
	$db = "$ws/output/$runSet.sdss";
} else {
	$db = $runSet;
}

# read and process it
open(DB, $db) or die "Cannot open .sdss database file $db";
while(defined($s=<DB>)) {
	($oid, $desig) = ($s =~ /(.{6}).{246}(.{20}).*$/);
	trim($desig);
#	print "[$oid], [$desig]\n"; exit -1;

	if($desig ne "-") {
		$counts{$desig}++;
		$counter{$desig} = 1;
		push @ref, $desig;
	} else {
		$counts{$oid}++;
		$counter{$oid} = 1;
		push @ref, $oid;
	}
}
close DB;

# write it
$dbtmp = "$db.tmp";
open(DB, $db) or die "Cannot open SDSS file $db";
open(TMP, ">$dbtmp") or die "Cannot open temporary file '$ARGV[0].tmp'";

$cnt = 0;
while(defined($s=<DB>)) {
	substr($s, 273, 5) = sprintf("%2.0f %2.0f", $counter{$ref[$cnt]}++, $counts{$ref[$cnt++]});
	print TMP $s;
}

close DB; close TMP;

$bkp = "$db." . time . ".appupdate";

# rename $db, $bkp or die "Cannot rename $db to $bkp. Aborting.\n";
rename $dbtmp, $db or die "Cannot rename $dbtmp to $db. Aborting.\n";

# print "Old SDSS file has been backed up as $bkp.\n";

# dump some statistics
foreach (values %counts) {
	$stats{$_}++;
}

print "\nStatistics (number of apparences/number of objects)\n";
foreach (sort {$a <=> $b} keys %stats) {
	print "$_ : $stats{$_}\n";
}
