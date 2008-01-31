#!/usr/bin/perl

if($#ARGV != 0) {
	print "Loads 94.mpec.sdss file and the new 94 file and checks if objects with same moID have the same (run, col, field, id) quadruplet\n";
	print "Also checks vice-versa - if the objects with the same (run, col, field, id) quadruplet have the same moID\n";
	print "Usage: oidchk.pl <94.new.sdss>\n";
	exit -1;
}

$mpec = "94.mpec.sdss";
$db = shift;

# load MPEC
open (MPC, $mpec) or die "Cannot open $mpec";
while(<MPC>) {
	($run, $col, $field, $id) = split(/ +/, substr($_, 84, 20));
	$unq = sprintf("%d %d %d %d", $run, $col, $field, $id);

	$oid = substr($_, 5, 6);
	$ampec{$oid} = $unq;
	$ampecrev{$unq} = $oid;
}
close MPC;

$diffs = 0; $revdiffs = 0;
$nnew = 0;
$unmatched = 0; $revunmatched = 0;

# load SDSS
open (DB, $db) or die "Cannot open $db";
while(<DB>) {
	($oid, $run, $col, $field, $id) = split / +/;
	$unq = sprintf("%d %d %d %d", $run, $col, $field, $id);	

	#########
	if(defined $ampec{$oid}) {
		$match++;
		if($unq ne $ampec{$oid}) {
			$diffs++;
			print "$oid : new [$unq] != old [$ampec{$oid}]\n";
#			if($diffs == 2) { exit -1;}
		}
	} else {
		$unmatched++;
	}

	########
	if(defined $ampecrev{$unq}) {
		$revmatch++;
		if($oid ne $ampecrev{$unq}) {
			$revdiffs++;
			print "$unq : new [$oid] != old [$ampecrev{$unq}]\n";
		}
	} else {
		$revunmatched++;
	}

	$nnew++;
}
close DB;

$nmpec = keys %ampec;

print "\n";
print "94.mpec.sdss : $nmpec\n";
print "94.sdss      : $nnew\n";
print "\n";
print "Matched      : $match\n";
print "Unmatched    : $unmatched\n";
print "Differences  : $diffs\n";
print "\n";
print "RMatched     : $revmatch\n";
print "RUnmatched   : $revunmatched\n";
print "RDifferences : $revdiffs\n";
