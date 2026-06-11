package Utils;

use Class::Inspector;
use all 'noimport';

use header;

sub find_subclasses ($class, $name)
{
	all::->import($name);
	return grep { $_ =~ /^${name}::/ }
		(Class::Inspector->subclasses($name) || [])->@*;
}

sub pascal_case ($class, $name)
{
	$name =~ s{(?:^|_) ([a-z])}{uc $1}exg;
	return $name;
}

