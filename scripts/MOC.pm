package MOC;
require 5.004;

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
        'ap', 'ep', 'sinip',
);

sub scatter
{
	my ($rec, $ns) = @_;
	foreach $fld (@mocrecord) { ${"$ns::$fld"} = $rec->{$fld}; }
}

sub gather
{
	my ($rec, $ns) = @_;
	foreach $fld (@mocrecord) { $rec->{$fld} = ${"$ns::$fld"}; }
}

sub localized(&$)
{
	my ($func, $rec) = @_;
	local (	$oid, $run, $col, $field, $id, $rowc, $colc,
		$mjd, $ra_o, $dec_o, $lambda_o, $beta_o, $phi_o, $vmu_o, $vmuErr_o, $vnu_o, $vnuErr_o, $vlambda_o, $vbeta_o,
		$u, $uErr, $g, $gErr, $r, $rErr, $i, $iErr, $z, $zErr, $b, $bErr, $V, $B,
		$isIdentified, $numeration, $designation, $iapp, $napp, $flags,
		$ra_c, $dec_c, $mag_c, $R, $geo_dist, $phase,
		$oe_cat, $H, $G, $arc, $epoch, $a, $e, $inc, $lan, $aop, $M,
		$pe_cat, $ap, $ep, $sinip
	);
	scatter $rec, "MOC";
	return &$func;
}

sub moc_string
{
	localized {
		moc_string_scattered();
	} shift;
}

sub moc_string_scattered
{
	$s = "";

	# moving object record
	$s .= sprintf("%-6s %5d %1d %4d %5d %8.3f %8.3f  ", $oid, $run, $col, $field, $id, $rowc, $colc);

	# time in MJD and position in deg (equatorial and ecliptic)
	$s .= sprintf(" %12.6f %10.6f %10.6f %10.6f %10.6f %11.6f ", $mjd, $ra_o, $dec_o, $lambda_o, $beta_o, $phi_o);

	# velocities
	$s .= sprintf(" %+6.4f %6.4f %+6.4f %6.4f %+6.4f %+6.4f ", $vmu_o, $vmuErr_o, $vnu_o, $vnuErr_o, $vlambda_o, $vbeta_o);

	# SDSS magnitudes
	$s .= sprintf(" %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f %5.2f %4.2f ",
		$u, $uErr, $g, $gErr, $r, $rErr, $i, $iErr, $z, $zErr, $b, $bErr);

	# Johnson magnitudes
	$s .= sprintf(" %5.2f %5.2f ", $V, $B);


	
	# identification information
	$s .= sprintf("%1d %5d %-20s %2d %2d %08x ", $isIdentified, $numeration, $designation, $iapp, $napp, $flags);

	# identification data
	$s .= sprintf(" %10.6f %10.6f %5.2f ", $ra_c, $dec_c, $mag_c);
	$s .= sprintf(" %7.3f %7.3f %5.2f ", $R, $geo_dist, $phase);



	# orbital elements
	$s .= sprintf(" %-20s %5.2f %4.2f %5.0f ", $oe_cat, $H, $G, $arc);
	$s .= sprintf(" %12.6f %12.8f %10.8f %10.6f %10.6f %10.6f %10.6f ", $epoch, $a, $e, $inc, $lan, $aop, $M);

	# proper elements
	$s .= sprintf(" %-20s %12.8f %10.8f %10.6f", $pe_cat, $ap, $ep, $sinip);

	return $s;
}


#
# 'MOC SQL' routines
#
sub query(&$) {
	my ($filter, $moc) = @_;

	my $rmoc = [];
	$idx = 0;
	for $rec (@{$moc}) {
		# localize this record for easy acccess by variables
		# and initialize the reference to record copy which we'll return
		my $record = {};
		foreach $fld (@mocrecord) { $$fld = ${$record}{$fld} = ${$rec}{$fld}; }

		# call filtering subroutine
		# the filtering subroutine has all @mocrecord variables accessible to itself
		# plus the $idx and $record automatic variables
		# -- word of caution: at this point, $record is _empty_. If you want to change
		# the values of individual fields, use local variables. However, $record is a valid
		# reference to the record which will be returned (for the use of this, 
		# see sub groupby())
		next if not &{$filter};

		# collect @mocrecord fields into the record hash
		foreach(@mocrecord) { ${$record}{$fld} = $$fld; }
		push @{$rmoc}, $ret;

		$idx++;
	}
	return $rmoc;
}

# stream($filename)
sub stream(&$) {
	# load the file
	my ($action, $mocfile) = @_;
	open(MOC, $mocfile) or die "Cannot open MOC catalog [$mocfile]";

	my $idx = 0;
	while(<MOC>) {
		my @r = split /\s+/;

		$_ = {};
		foreach $k (@mocrecord) { $_->{$k} = shift @r; }
		$_->{'idx'} = $idx;

		&$action();

		$idx++;
	}

	close(MOC);
}

# load($mocfile)
sub load {
	my $moc = ();
	stream { push @{$moc}, $_; } shift;
	return $moc;
}

# output(FILEHANLE, $moc)
sub output {
	my $fh = shift;
	my $moc = shift;
	filter { print $fh moc_string_scattered() . "\n"; } $moc;
}

# save($filename, $moc)
sub save {
	my ($fname, $moc) = @_;
	open(RMOC, ">$fname") or die "Cannot open [$fname] for writing! Aborting query.\n";
	output(\*RMOC, $moc);
	close(RMOC);
}

############################
