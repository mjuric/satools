#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};
chdir("$ws/catalogs");

sub trim { $_[0] =~ s/^\s*(.*?)\s*$/$1/; }

#############

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday) = gmtime(time());
$year += 1900; $mon++;
$id = sprintf "%04d%02d%02d", $year, $mon, $mday;

if(not -f "astorb.dat.$id")
{
	print `sa_downloadCatalogs.pl`;
	($? >> 8) == 0 or die("Error downloading ASTORB catalog.\n");
}
-f "astorb.dat.$id" or die("Error downloading ASTORB catalog.\n");

#############

# PHA list
@phalist = `wget --quiet -O - http://www.cfa.harvard.edu/iau/lists/Dangerous.html`;
($? >> 8) == 0 or die("Error downloading PHA list.\n");
while(scalar(@phalist) && chomp($_ = shift(@phalist)) && $_ ne "<pre>") { }
shift(@phalist); shift(@phalist);

while(chomp($_ = shift(@phalist)) && $_ ne "</pre>")
{
	(($name, $desig) = /^.{9}(.{18})(.{12}).*$/) or die("Error reading PHA list entry\n");
	trim($name); trim($desig);
	$key = $name ne "" ? $name : $desig;
	$pha{$key} = 1;

	#print "$key -> ($name, $desig)\n";
}
print "List of " . ($npha = scalar(keys %pha)) . " PHAs loaded from MPC website\n";

# Filter orbital elements
open(ASTORB, "astorb.dat.$id") or die("Error opening ASTORB catalog.\n");
open(PHAORB, ">astorb.dat.${id}pha") or die("Cannot open astorb.dat.${id}pha for writing.\n");
while($_=<ASTORB>)
{
	($name) = /^.{7}(.{19})/ or die("Error reading ASTORB entry\n");
	trim($name);
	next if not defined $pha{$name};

	delete $pha{$name};
	print PHAORB $_;
	$count++;
}

print "$count orbital elements found in astorb.dat.$id.\n";
print ($npha - $count) . " PHAs had no orbital elements in ASTORB.\n";
foreach(keys %pha)
{
	print "\t$_\n";
}
