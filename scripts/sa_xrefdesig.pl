#!/usr/bin/perl

use MOC;
use LibPeyton::Util ':all';

die  '
	sa_xrefdesig.pl - crossreferences MPC identifications with MOC
	identifications and generates a set of output files.

	Usage: sa_xrefdesig.pl <mpc_designations_file> <moc> <output_prefix>\n

	Output files:
		$output_prefix.unm.sdss - asteroids not id-ed by MPC
		$output_prefix.err.sdss - asteroids with differing ids
		$output_prefix.new.sdss - asteroids not id-ed by SDSS

' if(scalar(@ARGV) != 3);

($desigs, $mocfile, $p) = @ARGV;

open(DESIG, $desigs) or die "Cannot open [$desigs] designation file\n";
open(UNM, ">$p.unm.sdss") or die "Cannot open [$p.unm.sdss] for writing\n";
open(ERR, ">$p.err.sdss") or die "Cannot open [$p.err.sdss] for writing\n";
open(NEW, ">$p.new.sdss") or die "Cannot open [$p.new.sdss] for writing\n";

while(<DESIG>) {
	%dtmp = /^\s* 
		(?:(s[0-9a-f]{5})\s+(\S*)) \s*
		(?:(s[0-9a-f]{5})\s+(\S*))*\s*
		(?:(s[0-9a-f]{5})\s+(\S*))*\s*
		(?:(s[0-9a-f]{5})\s+(\S*))*\s*
		\s* $
	     /x or next;


	while(($k, $v) = each %dtmp) {
		if($k ne "" and exists $d{$k}) { print "double: $k\n"; }
		$d{$k} = $v;
	}
}
delete $d{""};
print scalar(keys %d) . " designations loaded.\n";

MOC::stream {
	# MOC identification
	my $oid = $_->{'oid'};
	my $desig = $_->{designation} ne '-' ? packed_form($_->{designation}) : '-';
	my $num = sprintf("%05d", $_->{numeration});
	my $isours = 0;
	
	if(not exists $d{$oid}) {
		# SDSS identification, no MPC identification
		if($desig ne '-') {
			$nomatch++;
			print UNM MOC::moc_string($_) . "\n";
		}
		next;
	}

	# MPC identification
	my $mdesig = $d{$oid};
	delete $d{$oid};
	if(substr($mdesig, 0, 1) eq '(') { $mdesig = substr($mdesig, 1); }
	else { $ours++; $isours = 1;  print "OURS!!! - "; }

	# if they're the same, we're finished
	if($desig eq $mdesig || $num eq $mdesig) { next; }

	# if this is an asteroid that MPC matched and we did not
	if($desig eq '-') {
		$newmatch++;
		print NEW MOC::moc_string($_) . "\n";

		# make a note if this asteroid got a designation
		if($isours) { 
			print "$oid $_->{designation} ($desig) : $mdesig *\n";
		}
		next;
	}

	# if this is an erroneous match
	print "$oid $_->{designation} ($desig) : $mdesig \n";
	print ERR MOC::moc_string($_) . "\n";
	$misident++;
} $mocfile;

print $misident . " misidentifications ($p.err.sdss).\n";
print $newmatch . " new maches found by MPC ($p.new.sdss).\n";
print $nomatch . " not matched by MPC ($p.unm.sdss).\n";

if($left = scalar(keys %d)) {
	# this should not happen - this means that Gareth sent us a designation
	# change for an oid not in MOC (or, more often, this means that the script
	# misparsed the input file
	print $left . " designations left (?!).\n";
	while(($k, $v) = each %d) {
		print "$k $v\n";
	}
}
