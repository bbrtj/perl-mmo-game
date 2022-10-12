package testheader;

use header;
use MockObject;

require Test2::V0;
require Test2::Tools::Provider;
require Data::Dumper;

sub import ($class)
{
	my $pkg = caller;

	Test2::V0->import::into($pkg);
	Test2::Tools::Provider->import::into($pkg);
	Data::Dumper->import::into($pkg);

	header->import::into($pkg);

	# make sure we have a language
	$i18n::CURRENT_LANG = 'en';

	return;
}

