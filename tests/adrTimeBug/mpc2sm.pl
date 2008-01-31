#!/usr/bin/perl

use Time::JulianDay;

sub rd {
	my $cat = shift;
	my %res;
	open(C, $cat) or die "Cannot open $cat\n";
	for(<C>) {
		next if(substr($_, 70, 1) eq 'B');
		next if(substr($_, 0, 1) ne ' ');
		undef($ds);
		(undef, $oid, $y, $m, $d, $rh, $rm, $rs, $dd, $dm, $ds) = split /(?: |\*)+/;

		next if not defined $ds;
		next if defined $res{$oid};

#		print "($oid, $y, $m, $d, $rh, $rm, $rs, $dd, $dm, $ds)\n";

		$ra = 15. * ($rh + $rm / 60. + $rs / 3600.);
		if(substr($dd, 0, 1) eq '-') { $dsgn = -1; } else { $dsgn = 1; }
		$dec = $dsgn * (abs($dd) + $dm / 60. + $ds / 3600.);
		$y = substr($y, 1);
		$mjd = julian_day($y, $m, $d) + ($d - int($d)) - 2400001;

		$res{$oid} = [$mjd, $ra, $dec, $rh, $rm, $rs];
	}
	close CAT;
	return %res;
}

($mold, $mnew) = @ARGV;

%old = rd($mold);
%new = rd($mnew);

#print STDERR "old: ", scalar(keys %old), " new: ", scalar(keys %new), "\n";

for(keys %old) {
	if(not exists $new{$_}) { # and $_ eq "s0061d") {
		printf STDERR "$_ %.5f %12.5f %12.5f\n", $old{$_}[0], $old{$_}[1], $old{$_}[2];
#		printf STDERR "$_ %12.5f %12.5f %12.5f\n", $old{$_}[3], $old{$_}[4], $old{$_}[5];
		$n++;
		next;
	}

	printf "$_ %.5f %12.5f %12.5f %.5f %12.5f %12.5f %12.5f\n",
		$old{$_}[0], $old{$_}[1], $old{$_}[2], 
		$new{$_}[0], $new{$_}[1], $new{$_}[2],
		$new{$_}[0] - $old{$_}[0];
}

print STDERR "$n objects does not appear in ADR\n";
