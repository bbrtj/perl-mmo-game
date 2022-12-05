package Role::Identified;

use My::Moose::Role;
use Data::ULID::XS qw(ulid);

use header;

has field 'id' => (
	writer => -hidden,
	default => sub { ulid },
);

