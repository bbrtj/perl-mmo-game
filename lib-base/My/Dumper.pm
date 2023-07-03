package My::Dumper;

use v5.38;

use Data::Dumper;

sub _get_dumper (@data)
{
	my $dumped;
	if (@data > 1) {
		$dumped = \@data;
	}
	else {
		$dumped = $data[0];
	}

	my $dd = Data::Dumper->new([$dumped]);

	$dd->Terse(1);
	$dd->Sortkeys(1);

	return $dd;
}

sub ddshort ($self, @data)
{
	my $dd = _get_dumper(@data);
	$dd->Indent(0);
	return $dd->Dump;
}

sub dd ($self, @data)
{
	my $dd = _get_dumper(@data);
	$dd->Varname('v');
	return $dd->Dump;
}

