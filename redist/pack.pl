#!/usr/bin/perl

# force execution from redist directory
if(not -X "./pack.pl") {
	print "Please execute pack.pl from redist directory.\n";
	exit -1;
}

$verbose = shift eq "-v";
$base = 'unstable';

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday) = gmtime(time);
$year += 1900; $mon++;

$rel = sprintf("satools-$base-orbfitless-%04d%02d%02d-%02d%02d%02d.tar.bz2", $year, $mon, $mday, $hour, $min, $sec);
print "Packing release $rel\n";

$cmd = "tar cjvf snapshots/$rel -C ../../ " .
	"--exclude=$base/workspace " .
	"--exclude=$base/redist/snapshots ".
	"--exclude=$base/orbfit/lib/jpleph " .
	"--exclude=$base/orbfit " .
	" $base";
print "$cmd\n";
open TAR, "$cmd |";
while(<TAR>) { 
	if($verbose) { print; }
}
close TAR;
