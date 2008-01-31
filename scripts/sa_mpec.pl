#!/usr/bin/perl

use Time::JulianDay;
use POSIX;
use LibPeyton::Coordinates ':all';

if(scalar(@ARGV) < 1) {
die "
Converts SDSS MOC asteroid catalogs to MPEC format suitable for
submission to the Minor Planet Center

	Usage: sa_moc2mpec.pl <catalog.sdss>

The results are printed to standard output.\n

DOES NOT APPLY THE ADR1 TIME BUG CORRECTION (it's assumed the catalog has
correct times).
";
}

sub formatCoord
{
	my($ra, $dec) = @_;
	my($ras, $decs, $neg, $a, $b, $c);

	$neg = $dec > 0 ? '+' : '-';
	if($neg eq '-') { $dec = -$dec; }

	RA: {
		$a = POSIX::floor($ra / 15); $ra -= $a * 15;
		$b = POSIX::floor($ra * 4); $ra -= $b / 4;
		$c = $ra * 240;

		if(sprintf("%05.2f", $c) eq "60.00") {
			$c = 0; $b++;
			if($b == 60) { $b = 0; $a++; }
			if($a == 360) { $a = 0; }
		}
	}
	$ras = sprintf "%02.0f %02.0f %05.2f", $a, $b, $c;

	DEC: {
		$a = POSIX::floor($dec); $dec -= $a;
		$b = POSIX::floor($dec * 60); $dec -= $b / 60;
		$c = $dec * 3600;

		if(sprintf("%04.1f", $c) eq "60.0") {
			$c = 0; $b++;
			if($b == 60) { $b = 0; $a++; }
		}
	}
	$decs = sprintf "$neg%02.0f %02.0f %04.1f", $a, $b, $c;

	return ($ras, $decs);
}

sub getYMD {
	my $mjd = shift;
	my($yd, $year, $month, $day);

	$jd = $mjd + 2400001; # inverse_jd routine has a bug - counts JDs from midnight
	($year, $month, $day) = inverse_julian_day($jd);
	$day += $mjd - POSIX::floor($mjd);

	return ($year, $month, $day);
}

@mocrecord = (
        'oid',
        'run', 'col', 'field', 'id',
        'rowc', 'colc',

        'mjd', 'ra_o', 'dec_o', 'lambda_o', 'beta_o', 'phi_o',
        'vmu_o', 'vmuErr_o', 'vnu_o', 'vnuErr_o',
        'vlambda_o', 'vbeta_o',

        'u', 'uErr', 'g', 'gErr', 'r', 'rErr', 'i', 'iErr', 'z', 'zErr', 'b', 'bErr', 'V', 'B',

        'isIdentified',
        'numeration',
        'designation',
        'iapp', 'napp', 'flags',

        'ra_c', 'dec_c', 'mag_c',
        'R', 'geo_dist', 'phase',

        'oe_cat',
        'H', 'G', 'arc', 'epoch', 'a', 'e', 'inc', 'lan', 'aop', 'M',

        'pe_cat',
        'ap', 'ep', 'sinip'
);

# Preparations and headers
$t = 5 / (60 * 24); # 5 minute interval
sub compact { my $c = shift; while($c < 0) {$c += 360;}; while($c >= 360) { $c -=360; } }

$headers = 
"COD 645
OBS SDSS Collaboration
MEA M. Juric, Z.Ivezic, R. Lupton
TEL 2.5m SDSS Telescope + SDSS camera
NET UCAC 1
ACK SDSS ADR2 release, generated on May 13th, 2003
";

print "$headers\n";

# MOC loading

$mocfile = shift;
open(MOC, $mocfile) or die "Cannot open MOC catalog [$mocfile]";

$idx = 0;
while($_ = <MOC>) {
	@r = split /\s+/;
	foreach(@mocrecord) { $moc[$idx]{$_} = shift @r; } $idx++;

	# MPEC creation
	%r = %{$moc[$idx-1]};
	for(keys %r) { $$_ = $r{$_}; }

#	next if $run != 1037 || $designation eq '-';
#	$node = 95; $inc = 10;
#	($mu, $nu) = equgcs($node, $inc, $ra_o, $dec_o);
#	($vra2, $vdec2) = vgcsvequ($node, $inc, $mu, $nu, $vmu_o, $vnu_o);	# -> vra, vdec
	($vra, $vdec) = vgcsvequ(0, +23.439291, $lambda_o, $beta_o, $vlambda_o, $vbeta_o);	# vlambda, vbeta -> vra, vdec
#	print "$oid ($ra_o, $dec_o), ($mu, $nu), vg=($vmu_o, $vnu_o) veo=($vra2, $vdec2) =? ($vra, $vdec)\n";
#	next;

	# second position
	#
	$ra2  = $ra_o  + $t * $vra;  compact($ra2);
	$dec2 = $dec_o + $t * $vdec; compact($dec2);

	#
	# Apply ADR time bug correction
	#    mjd(real) - mjd(ADR) = 3.056e-07 * rowc
	#
	#$timeCorrection = $rowc * 0.396 / (361*3600);
	#print STDERR "($rowc, $timeCorrection)\n" if($oid eq "s00001");
	#$mjd += $timeCorrection;

	# datetime
	#
	($year, $month, $day) = getYMD($mjd);

	# ra/dec
	#
	($ras, $decs) = formatCoord $ra_o, $dec_o;
	($ras2, $decs2) = formatCoord $ra2, $dec2;

	# mags
	$V = $V != 99.99 ? sprintf("%4.1f V", $V) : "      ";
	$B = $B != 99.99 ? sprintf("%4.1f B", $B) : "      ";

	$record1 = sprintf("     %-7s  C%4d %02d %08.5f $ras $decs          $V      645", $oid, $year, $month, $day, $V);
	if($vlambda_o != 9.99 && $vbeta_o != 9.99) {
		($year, $month, $day) = getYMD($mjd + $t);
		$record2 = sprintf("     %-7s  C%4d %02d %08.5f $ras2 $decs2          $B      645", $oid, $year, $month, $day, $B);
	} else {
		die "$oid has no B measurement\n";
	}

	if(($l = length($record1) != 80) || ($l = length($record1) != 80)) {
		die "Record length violation [$l] - record MUST be 80 characters long (something got formatted wrong, probably because it's out of range - check the output)\n";
	}

	print "$record1\n$record2\n";
}
