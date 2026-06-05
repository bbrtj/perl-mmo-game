package Types;

use v5.42;

use Type::Library -extends => [qw(Types::Common Type::EmailAddress Types::ULID)];

use My::Time::Piece;

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

