package Utils;

use Class::Inspector;
use all 'noimport';
use Exporter qw(import);
use Server::Config;

use header;

our @EXPORT_OK = qw(
	find_subclasses
	pascal_case

	transport_float
	transport_floats
	transport_float_rev
	transport_floats_rev
);

sub find_subclasses ($name)
{
	all::->import($name);
	return grep { $_ =~ /^${name}::/ }
		(Class::Inspector->subclasses($name) || [])->@*;
}

sub pascal_case ($name)
{
	$name =~ s{(?:^|_) ([a-z])}{uc $1}exg;
	return $name;
}

sub transport_float :prototype($) ($float)
{
	return int($float * Server::Config::TRANSPORT_FLOAT_PRECISION);
}

sub transport_floats (@floats)
{
	return map { int($_ * Server::Config::TRANSPORT_FLOAT_PRECISION) } @floats;
}

sub transport_float_rev :prototype($) ($int)
{
	return $int / Server::Config::TRANSPORT_FLOAT_PRECISION;
}

sub transport_floats_rev (@ints)
{
	return map { $_ / Server::Config::TRANSPORT_FLOAT_PRECISION } @ints;
}

