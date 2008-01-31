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

print "Utility for getting unique asteroid counts from .sdss files\n";
print "Author     : majuric\@astro.hr\n";
print "Maintainer : majuric\@astro.hr\n";

print "\n";

if($#ARGV < 0) {
	print "Usage 1 : unique.pl <runSet> [run1 [run2 [run3 [...]]]]\n";
	print "Usage 2 : unique.pl <file.sdss>\n";
	exit;
}

if($#ARGV != 0) {
	$runSet = shift;
	foreach $i(@ARGV) { $i = "$ws/output/$runSet.d/$i.sdss"; }
} else {
#	if(substr($runSet = $ARGV[0], -4) ne "sdss") { $ARGV[0] = "$ws/output/$runSet.sdss"; }
	if(substr($runSet = $ARGV[0], -4) ne "sdss" and
	   substr($runSet = $ARGV[0], -3) ne "dat"
	) { $ARGV[0] = "$ws/output/$runSet.sdss"; }
}

while(defined($l = <>)) {
	($u, $ue, $g, $ge, $r, $re, $i, $ie, $z, $ze, $b, $be) = split(/ +/, substr($l, 163));

	if($b == 99.99) { $colorUnknown++; }
	else {
		if($b < 0) { $blue++; } else { $red++; }
	}

	if(substr($l, 242, 1) eq "0") { next; }
	$total++;

	if($b == 99.99) { next; }

	($num, $name) = ($l =~ /.{244} *([0-9]+) *([^ ]+)/);

	$ast{$name} = "bbb";
	$numer{$num} = $name;

	if($b < 0) {
		if(defined $astred{$name}) {
			delete $astred{$name};
			$bland{$name} = "bbb";
		} else {
			$astblue{$name} = "bbb";
		}
	} else {
		if(defined $astblue{$name}) {
			delete $astblue{$name};
			$bland{$name} = "bbb";
		} else {
			$astred{$name} = "bbb";
		}
	}
}

## print everything
$tot = $red + $blue + $colorUnknown;
print "Total: $tot\n";
print "Red: $red\n";
print "Blue: $blue\n";
print "Unknown: $colorUnknown\n";

print "\n";

print "Total identified: $total\n";
print "\n";

@keys = keys %ast;     $unique = $#keys + 1;     print "Unique: $unique\n";
@keys = keys %astblue; $uniqueblue = $#keys + 1; print "Unique blue: $uniqueblue\n";
@keys = keys %astred;  $uniquered = $#keys + 1;  print "Unique red: $uniquered\n";
@keys = keys %bland;  $uniquebland = $#keys + 1; print "Unique bland: $uniquebland\n";
@keys = keys %numer;   $num = $#keys;            print "Unique numerated: $num\n";
#$t = $uniqueblue + $uniquered + $uniquebland; print "$t\n";
