#!/usr/bin/perl

scalar(@ARGV) == 2 or die("Usage: sa_diff.pl <old.sdss> <new.sdss>\n");

@mocrecord = (
        'oid',
        'run', 'col', 'field', 'id',
        'rowc', 'colc',

        'mjd', 'ra_o', 'dec_o', 'lambda_o', 'beta_o', 'phi_o',
        'vmu_o', 'vmuErr_o', 'vnu_o', 'vnuErr_o',
        'vra_o', 'vdec_o',

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

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }

$|=1; # flushing off

($f1, $f2) = @ARGV;

# load the new catalog into a hash table
open(MOC, $f2) or die "Cannot open MOC catalog [$f2]";
$idx = 0;
print STDERR "Loading [$f2] ";
while($_ = <MOC>) {
	if(++$idx % 10000 == 0) { print STDERR "."; }

	@r = split /\s+/;
	foreach $k (@mocrecord) { $$k = shift @r; }

	#($oid, $num, $desig, $arc) = ($r[0], $r[34], $r[35], $r[48]);
	($oid, $num, $desig, $arc) = ($oid, $numeration, $designation, $arc);
	$dra = ($ra_o - $ra_c)*3600;
	$ddec = ($dec_o - $dec_c)*3600;

	$oids{$oid}= [$desig, $num, $arc, $dra, $ddec];
#	if($oid eq 's1259d') { print "$oid $desig, $num, $arc, $dra, $ddec\n"; }
}
print STDERR " loaded.\n";

# loop over the old catalog, comparing it to the new
open(MOC, $f1) or die "Cannot open MOC catalog [$f1]";
$idx = 0;
$lost = 0; # asteroids identified in one run, but not identified in the second run
for(<MOC>) {
	$idx++;
#	if($idx % 1000 == 0) { print "."; }

	@r = split /\s+/;
	($oid, $num, $desig) = ($r[0], $r[34], $r[35]);		# old designation

	if(not defined $oids{$oid}) { $lost++; $next; }

	($newdesig, $newnum, $newarc, $newdra, $newddec) = @{$oids{$oid}};
	$nameChange = $newdesig ne $desig;
	next if not $nameChange;		# if linkage has not changed, it's OK.

	next if $desig eq '-';			# if old designation is '-', and new isn't, that's OK (we've simply identified a the asteroid)
	next if $num == 0 && $newnum != 0;	# designations and numeration differ, but old numeration was 0. Maybe the asteroid got numerated, and named, in between catalogs.
	next if $num != 0 && $num == $newnum;	# designations differ, but numeration is the same, that's because the asteroid has been named (OK)

	# this is a problematic asteroid...
	if($newdesig eq '-') { $lostlink++; $ll=1; } else { $changedlink++; $ll=0; }

	foreach $k (@mocrecord) { $$k = shift @r; }

#	if($exists and $nameChange and substr($newdesig, 0, 1) eq '-') { $lost++; $lflag = 1; } else { $lflag = 0; }

	$dra = ($ra_o - $ra_c)*3600;
	$ddec = ($dec_o - $dec_c)*3600;
	if($ll) { $newdra = $newddec = 0; }

	# table format
	printf  "%-6s %14.6f %10.6f %10.6f %5.2f  %5.1f %5.1f  %6d %7d %-20s  -->  %5.1f %5.1f  %6d %7d %-20s   %1d %1d\n",
		$oid, $mjd+2400000.5, $ra_o, $dec_o, $V, $dra, $ddec, $arc, $numeration, $desig,   $newdra, $newddec, $newarc, $newnum, $newdesig, $nameChange, $ll;
#	printf  "%-6s %4d %1d %4d  %8.3f %8.3f   %10.6f %10.6f  %5.2f %7d %-20s  -->  %7d %-20s %1d %1d %1d\n",
#		$oid, $run, $col, $field, $rowc, $colc,  $ra_o, $dec_o, $V,    $numeration, $desig, $newnum, $newdesig, !$exists, $nameChange, $ll;


}
print "\n";
print STDERR "$lost object(s) lost.\n";
print STDERR "$lostlink matches lost.\n";
print STDERR "$changedlink matches changed.\n";
