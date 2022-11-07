package Types;

use v5.36;

use Type::Libraries;
use Type::Tiny;
use Types::Standard qw(Num);
use Types::Common::String qw(NonEmptySimpleStr);
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

my $LoreId = Type::Tiny->new(
	name => 'LoreId',
	parent => NonEmptySimpleStr,
	constraint => q{ length $_ <= 32 },
	inlined => sub {
		my $varname = pop;
		return (undef, "length $varname <= 32");
	},
);

my $DateTime = Type::Tiny->new(
	name => 'DateTime',
	parent => Types::DateTime::DateTime,

	coercion => [
		Num, q{ Types::DateTime::DateTime->coerce($_) },
		Format ['Pg'],
	]
);

__PACKAGE__->add_type($LoreId);
__PACKAGE__->add_type($DateTime);

__PACKAGE__->make_immutable;

