package Types;

use v5.36;

use Type::Libraries;
use Type::Tiny;
use Types::Standard qw(Num);
use Types::Common::String qw(StrLength);
use Types::DateTime qw(Format);

Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
		Types::Common::String
		Type::EmailAddress
		Types::ULID
	),
);

my $ShortStr = Type::Tiny->new(
	name => 'ShortStr',
	parent => StrLength[1, 32],
);

my $LoreId = Type::Tiny->new(
	name => 'LoreId',
	parent => $ShortStr,
);

my $DateTime = Type::Tiny->new(
	name => 'DateTime',
	parent => Types::DateTime::DateTime,

	coercion => [
		Num, q{ Types::DateTime::DateTime->coerce($_) },
		Format ['Pg'],
	]
);

__PACKAGE__->add_type($ShortStr);
__PACKAGE__->add_type($LoreId);
__PACKAGE__->add_type($DateTime);

__PACKAGE__->make_immutable;

