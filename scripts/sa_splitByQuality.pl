#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};

use Fcntl;
use IO::Handle;
use Time::JulianDay;
use LibPeyton::Coordinates ':all';

STDOUT->autoflush(1);

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }
sub mjd { ($y, $m, $d) = @_; return julian_day($y, $m, $d) - 2400000.5; }

print "Utility splitting up the catalog to high and low quality observations\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if(scalar(@ARGV) != 1) {
	print "Usage : sa_splitByQuality.pl <runSet>\n";
	exit;
}

$runSet = shift;
$sdss = "$ws/output/$runSet.sdss";
$prefix = "$ws/output/$runSet";

open GPLANE, ">$prefix.galacticPlane.sdss" or die("Could not open [$prefix.galacticPlane.sdss] for writing");
sub filter_galactic_plane
{
	$line = shift;

	$ra = $_[8]; $dec = $_[9];
	($l, $b) = equ_gal($ra, $dec);
	##print "$ra, $dec -> $l, $b\n";

	if(abs($b) < 20)
	{
		print GPLANE $line;
		return 1;
	}

	return 0;
}

open RUN4905, ">$prefix.run4905.sdss" or die("Could not open [$prefix.run4905.sdss] for writing");
sub filter_run_4905
{
	$line = shift;
	($run, $field) = ($_[1], $_[3]);
	##print "$run, $field\n";

	if($run == 4905 && $field < 40)
	{
		print RUN4905 $line;
		return 1;
	}

	return 0;
}

open MAIN, ">$prefix.good.sdss" or die("Could not open [$prefix.good.sdss] for writing");
sub store_good
{
	print MAIN $line;
	return 1;
}

open IN, $sdss or die("Could not open $sdss\n");
print "Filtering to subsets...\n";
while($line = <IN>)
{
	@fields = split /\s+/, $line;

	   filter_galactic_plane($line, @fields)
	or filter_run_4905($line, @fields)
	or store_good($line, @fields);

	if(!(++$cnt % 1000)) { print "#"; }
	if(!($cnt % 50000)) { print " [$cnt]\n"; }
}

print " done.\n\n";

sub postproc
{
	$fn = shift;

	print "$fn...\n";

	open STAT, ">$fn.txt" or die("Could not open [$fn.txt] for writing");
	print STAT `sa_updateApp.pl $fn`;
	print STAT "\n\n";
	print STAT `sa_unique.pl $fn`;
	close STAT;
}

print "Postprocessing subsets:\n";
postproc("$prefix.galacticPlane.sdss");
postproc("$prefix.run4905.sdss");
postproc("$prefix.good.sdss");
postproc("$prefix.sdss");
print "\nDone.\n";
