package Unit;

use My::Moose;

use header;

with 'Role::Identified';

# return models that should be saved
# (proper order is needed)
sub models ($self)
{
	return [];
}

