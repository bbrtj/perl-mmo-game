package Game::Types;

use header;
use Type::Libraries;
use Type::Tiny;
use Types::Standard qw(Num);
use Types::Common::String qw(NonEmptySimpleStr);
use Types::DateTime qw(DateTime Format);

Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
		Types::Common::String
		Types::UUID
		Type::EmailAddress
		),
);

my $LoreId = Type::Tiny->new(
	name => 'LoreId',
	parent => NonEmptySimpleStr,
	constraint => q{ length $_ <= 20 },
	inlined => sub {
		my $varname = pop;
		return (undef, "length $varname <= 20");
	},
);

my $DateTime = Type::Tiny->new(
	name => 'DateTime',
	parent => DateTime,
);

__PACKAGE__->add_type($LoreId);
__PACKAGE__->add_type($DateTime)->coercion->add_type_coercions(
	Num, q{ Types::DateTime::DateTime->coerce($_) },
	Format ['Pg'],
)->freeze;

1;
