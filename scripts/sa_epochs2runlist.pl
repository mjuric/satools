#!/usr/bin/perl

($epochs) = @ARGV;
open EPOCHS, $epochs or die "Cannot open $epochs file";

while(<EPOCHS>) {
	($epoch, $runs) = split / +: +/;
	$runs =~ s/^\s*(.*?)\s*$/$1/;
	@runs = split(/ +/, $runs);

	foreach $run(@runs) {
	  print "$run\n";
	}
}
