#!/usr/bin/perl

if(!defined($ENV{SDSSAST_WORKSPACE})) {
	print "\nSDSSAST_WORKSPACE environment variable must point to a workspace directory.\n";
	exit -1;
}
$ws = $ENV{SDSSAST_WORKSPACE};
chdir("$ws/catalogs");

#############

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday) = gmtime(time());
$year += 1900; $mon++;
$id = sprintf "%04d%02d%02d", $year, $mon, $mday;

#############

print "Downloading ASTORB ...\n";
`wget ftp://ftp.lowell.edu/pub/elgb/astorb.dat.gz -O astorb.dat.$id.gz`;

print `gunzip astorb.dat.$id.gz && ln -sf astorb.dat.$id astorb.dat.latest`;

print "ASTORB downloaded\n";

#############

print "Downloading AstDys proper elements ...\n";
`wget http://hamilton.dm.unipi.it/astdys/catalogs/ufitobs.pro -O ufitobs.pro.$id && ln -sf ufitobs.pro.$id ufitobs.pro.latest`;
`wget http://hamilton.dm.unipi.it/astdys/catalogs/allnum.pro  -O allnum.pro.$id  && ln -sf allnum.pro.$id allnum.pro.latest`;

print "Proper elements downloaded.\n";

#############

print "\n\nCatalog ID: $id\n\n";

