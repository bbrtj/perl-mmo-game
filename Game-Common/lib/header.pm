package header;

use strict;
use warnings;
use utf8;
use feature ':5.30';

use experimental;
require namespace::clean;
require Exporter;
require Safe::Isa;

sub import
{
	strict->import;
	warnings->import;
	utf8->import;
	feature->import(':5.30');
	experimental->import('signatures');
	warnings->unimport('experimental::signatures');
	{ local $Exporter::ExportLevel = 1; Safe::Isa->import; }
	return;
}

sub unimport
{
	warnings->unimport('experimental::signatures');
	namespace::clean->import(-cleanee => scalar(caller));
}

1;
