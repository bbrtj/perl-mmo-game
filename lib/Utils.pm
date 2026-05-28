package Utils;

use Game::LoreLoader;
use Class::Inspector;

use header;

sub find_subclasses ($class, $name)
{
	require all;

	all::->import($name);
	return grep { $_ =~ /^${name}::/ }
		(Class::Inspector->subclasses($name) || [])->@*;
}

