package Role::Identified;

use My::Moose::Role;

use header;

has field 'id' => (
	writer => -hidden,
	default => sub { Types::ULID::ulid },
);

