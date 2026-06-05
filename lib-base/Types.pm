package Types;

use v5.42;

use Type::Library -base;
use Types::Common -types;
use My::Time::Piece;
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
	parent => InstanceOf->of('My::Time::Piece'),

	coercion => [
		Num, q{ My::Time::Piece->from_timestamp($_) },
		Str, q{ My::Time::Piece->from_string($_) },
	],
);

__PACKAGE__->make_immutable;

