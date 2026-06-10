package Utils;

use Game::LoreLoader;
use Class::Inspector;
use all 'noimport';

use header;

sub find_subclasses ($class, $name)
{
	all::->import($name);
	return grep { $_ =~ /^${name}::/ }
		(Class::Inspector->subclasses($name) || [])->@*;
}

