package Types;

use Type::Libraries;
use Type::Tiny;
use Types::Standard qw(Num Undef);
use Types::Common::String qw(NonEmptySimpleStr StrLength);
use Types::DateTime qw(DateTime Format);
use Data::ULID;

use header;

Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
		Types::Common::String
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

my $ULID = Type::Tiny->new(
	name => 'Ulid',
	parent => (StrLength [26])->where(q{ /\A[0-9a-zA-Z]+\z/ }),
);

__PACKAGE__->add_type($LoreId);

__PACKAGE__->add_type($DateTime)->coercion->add_type_coercions(
	Num, q{ Types::DateTime::DateTime->coerce($_) },
	Format ['Pg'],
)->freeze;

__PACKAGE__->add_type($ULID)->coercion->add_type_coercions(
	Undef, q{ Data::ULID::ulid() },
)->freeze;

