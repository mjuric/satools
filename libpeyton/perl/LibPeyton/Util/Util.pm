package LibPeyton::Util;

use 5.006;
use strict;
use warnings;

require Exporter;
require DynaLoader;

our @ISA = qw(Exporter DynaLoader);

# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.

# This allows declaration	use LibPeyton::Util ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(

	format_coords
	format_coord

	approx_sun_longitude

	packed_form

	mpec_time_format
	ymd
	dayfrac
	mjd
	
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);
our $VERSION = '0.02';

bootstrap LibPeyton::Util $VERSION;

# Preloaded methods go here.

1;
__END__
# Below is stub documentation for your module. You better edit it!

=head1 NAME

LibPeyton::Util - Perl extension for blah blah blah

=head1 SYNOPSIS

  use LibPeyton::Util;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for LibPeyton::Util, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head1 EXPORT

	($ras, $decs) = format_coords(double ra, double dec, string format)

	$str = format_coord(string format, double v)



	$ra = approx_sun_longitude($mjd)

	$designation = packed_form($provisional_desig)



	$str = mpec_time_format($mjd)4

	($y, $m, double $d) = ymd($mjd)

	$dayfrac = dayfrac($h, $m, $s)

	$mjd = mjd($yy, $mm, double $dd, [$h, $m, $s])

=head1 AUTHOR

A. U. Thor, E<lt>a.u.thor@a.galaxy.far.far.awayE<gt>

=head1 SEE ALSO

L<perl>.

=cut
