# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 5 };
use LibPeyton::Util;

($ras, $dec) = &LibPeyton::Util::format_coords(40, 20.5, 1);
ok("[$ras, $dec]", "[02 40 00.00,  20 30 00.0]");

ok(&LibPeyton::Util::approx_sun_longitude(52000), "12.7642235704069");
print &LibPeyton::Util::mjd(2003, 8, 12, 2) . "\n";
print 180 - &LibPeyton::Util::approx_sun_longitude(52863.2006944) . "\n";
ok(&LibPeyton::Util::mpec_time_format(53000.4568), "2003 12 27.45680");
ok(&LibPeyton::Util::packed_form("1997 XF11"), "J97X11F");
ok(&LibPeyton::Util::format_coord("%02.0H %02.0M %05.2S", 11.5), "00 46 00.00");

($y, $m, $d) = LibPeyton::Util::ymd(53000.4);
ok("($y, $m, $d)", "(2003, 12, 27.4000000000015)");
ok(&LibPeyton::Util::dayfrac(10, 5, 2), 0.420162037037037);
ok(&LibPeyton::Util::mjd($y, $m, $d), 53000.4);

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

