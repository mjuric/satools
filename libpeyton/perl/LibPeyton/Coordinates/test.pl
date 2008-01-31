# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test;
BEGIN { plan tests => 3 };
use LibPeyton::Coordinates ':all';

print angular_dist(0, 0, 90, 0) . "\n";
$node = 0; $inc = 23.4392911;
($ra, $dec) = transform_pos($node, $inc, 10, 10); print "($ra, $dec)\n";
($mu, $nu) = equ_ecl(10, 10); print "($mu, $nu)\n";

print "bounded = " . is_bounded_by(5, -5, -10, -10, 10, 10) . "\n";
print "bounded = " . is_bounded_by(5, -5, 350, -10, 10, 10) . "\n";

#########################

# Insert your test code below, the Test module is use()ed here so read
# its man page ( perldoc Test ) for help writing this test script.

