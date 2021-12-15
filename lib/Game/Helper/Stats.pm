package Game::Helper::Stats;

use Moo;

use header;

my $part_separator = ';';
my $value_separator = ':';

sub from_string ($self, $string)
{
	my @split = split /$part_separator/, $string;
	return {
		map { split /$value_separator/, $_ } grep { $_ } @split
	};
}

sub to_string ($self, $hash)
{
	return join $part_separator, map { join $value_separator, $_, $hash->{$_} }
		keys $hash->%*;
}

sub weld_strings ($self, @strings)
{
	return {
		map { $self->from_string($_)->%* } @strings
	};
}

