package LibPeyton::Coordinates;

use 5.006;
use strict;
use warnings;
use Carp;

require Exporter;
require DynaLoader;
use AutoLoader;

our @ISA = qw(Exporter DynaLoader);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use LibPeyton::Coordinates ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	angular_dist
	is_bounded_by

	transform_pos
	transform_vel

	ecl_equ equ_ecl
	gal_equ equ_gal
	gcs_equ equ_gcs
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);
our $VERSION = '0.02';

sub AUTOLOAD {
    # This AUTOLOAD is used to 'autoload' constants from the constant()
    # XS function.  If a constant is not found then control is passed
    # to the AUTOLOAD in AutoLoader.

    my $constname;
    our $AUTOLOAD;
    ($constname = $AUTOLOAD) =~ s/.*:://;
    croak "& not defined" if $constname eq 'constant';
    my $val = constant($constname, @_ ? $_[0] : 0);
    if ($! != 0) {
	if ($! =~ /Invalid/ || $!{EINVAL}) {
	    $AutoLoader::AUTOLOAD = $AUTOLOAD;
	    goto &AutoLoader::AUTOLOAD;
	}
	else {
	    croak "Your vendor has not defined LibPeyton::Coordinates macro $constname";
	}
    }
    {
	no strict 'refs';
	# Fixed between 5.005_53 and 5.005_61
	if ($] >= 5.00561) {
	    *$AUTOLOAD = sub () { $val };
	}
	else {
	    *$AUTOLOAD = sub { $val };
	}
    }
    goto &$AUTOLOAD;
}

bootstrap LibPeyton::Coordinates $VERSION;

# Preloaded methods go here.

sub ecl_equ($$)
{
	# transform from equatorial to ecliptic coordinates
	return transform_pos(0, -23.4392911, $_[0], $_[1]);
}

sub equ_ecl($$)
{
	# transform from equatorial to ecliptic coordinates
	return transform_pos(0, 23.4392911, $_[0], $_[1]);
}

sub equ_gcs($$$$)
{
	# transform from equatorial to arbitrary GCS coordinates
	# basically, an alias for transform_pos
	return transform_pos($_[0], $_[1], $_[2], $_[3]);
}

sub gcs_equ($$$$)
{
	# transform from equatorial to arbitrary GCS coordinates
	return transform_pos($_[0], -$_[1], $_[2], $_[3]);
}

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is stub documentation for your module. You better edit it!

=head1 NAME

LibPeyton::Coordinates - Perl extension for blah blah blah

=head1 SYNOPSIS

  use LibPeyton::Coordinates;
  use LibPeyton::Coordinates ':all';

=head1 DESCRIPTION

Module for spherical coordinate system manipulation. Uses libPeyton C++ routines.
Main difference between PERL and C++ interface is that in PERL all angular values
are in degrees.

=head1 EXPORT

=head2 angular_dist(ra1, dec1, ra2, dec2)

Angular distance between two points

=head2 is_bounded_by(ra, dec, ra_lo, dec_lo, ra_hi, dec_hi)

Is (ra, dec) bounded by ((ra_lo, dec_lo), (ra_hi, dec_hi)) spherical rectangle. 
Note that:

C<is_bounded_by(0, 0, 350, -10, 10, 10) == TRUE>

=head2 ($mu, $nu) = transform_pos($node, $incl, $ra, $dec)

Transforms (ra, dec) to coordinate system specified by (node, incl).

=head2 ($vra, $vdec) = transform_vel($node, $incl, $mu, $nu, $vmu, $vnu)

Transforms (vmu, vnu) of a point at ($mu, $nu) to ($vra, $vdec)

=head2 ($mu, $nu) = equ_gcs($node, $incl, $ra, $dec)
=head2 ($ra, $dec) = gcs_equ($node, $incl, $mu, $nu)

Convenience functions (just calls to transform_pos).

=head2 ($lambda, $beta) = equ_ecl($ra, $dec)
=head2 ($ra, $dec) = ecl_equ($lambda, $beta)

Transforms from equatorial to ecliptic coordinates and vice versa.

=head1 AUTHOR

A. U. Thor, E<lt>a.u.thor@a.galaxy.far.far.awayE<gt>

=head1 SEE ALSO

L<perl>.

=cut
