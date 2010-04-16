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
@neolist = `wget --quiet -O - http://newton.dm.unipi.it/~neodys/neo_name.list`;
($? >> 8) == 0 or die("Error downloading PHA list.\n");
while(chomp($_ = shift(@neolist)))
{
	(($desig) = /^.{9}(.*)$/) or die("Error reading PHA list entry\n");
	trim($desig);

	# asteroids requiring special dispensation
	$desig == 'DonQuixote' && ($desig = "Don Quixote");

	if(($yr = substr($desig, 0, 4)) != 0)
	{
		$key = $yr . " " . substr($desig, 4);
	} else {
		$key = $desig;
	}
	$neo{$key} = 1;

	#print "$key -> ($desig)\n";
}
print "List of " . ($nneo = scalar(keys %neo)) . " NEOs loaded from NeoDyS website\n";

# Filter orbital elements
open(ASTORB, "astorb.dat.$id") or die("Error opening ASTORB catalog.\n");
open(PHAORB, ">astorb.dat.${id}neo") or die("Cannot open astorb.dat.${id}neo for writing.\n");
while($_=<ASTORB>)
{
	($name) = /^.{7}(.{19})/ or die("Error reading ASTORB entry\n");
	trim($name);
	next if not defined $neo{$name};

	delete $neo{$name};
	print PHAORB $_;
	$count++;
}

print "$count orbital elements found in astorb.dat.$id.\n";
print ($nneo - $count) . " PHAs had no orbital elements in ASTORB.\n";
foreach(keys %neo)
{
	print "\t$_\n";
}
