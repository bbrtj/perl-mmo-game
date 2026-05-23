package Types;

use v5.42;

use Type::Library -base;
use Types::Common -types;
use Types::DateTime qw(Format);
use Type::EmailAddress -types;
use Types::ULID -types;

my $ShortStr = __PACKAGE__->add_type(
	name => 'ShortStr',
	parent => StrLength [1, 32],
);

my $LoreId = __PACKAGE__->add_type(
	name => 'LoreId',
	parent => $ShortStr,
);

my $DateTime = __PACKAGE__->add_type(
	name => 'DateTime',
	parent => Types::DateTime::DateTime,

	coercion => [
		Num, q{ Types::DateTime::DateTime->coerce($_) },
		Format ['Pg'],
	]
);

__PACKAGE__->make_immutable;

