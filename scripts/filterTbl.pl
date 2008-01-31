#!/usr/bin/perl

#
#	Clean up the input .tbl file by imposing magnitude and velocity constraints
#		14.5 < r < 21.5
#		0.05 < v < 0.5
#

$v0 = 0.05;
$v1 = 0.5;

$v0 *= $v0;
$v1 *= $v1;

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }

while(defined($_=<>)) {
	trim $_;
	@rec = split / +/;
	($r, $rc, $rr) = ($rec[20], $rec[7], $rec[8]);
	$v = $rc*$rc + $rr*$rr;

#	print "$_\n";
#	print "[$r] [$v] [$rc]\n";
	
	next if not (14.5 < $r and $r < 21.5);
	next if not ($v0  < $v and $v < $v1);
	
	print "$_\n";
}
