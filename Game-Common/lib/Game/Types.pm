package Game::Types;

use header;
use Type::Libraries;
use Type::Tiny;
use Types::Common::String qw(NonEmptySimpleStr);

Type::Libraries->setup_class(
	__PACKAGE__,
	qw(
		Types::Standard
		Types::Common::Numeric
		Types::Common::String
		Types::UUID
		Types::DateTime
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

__PACKAGE__->add_type($LoreId);

1;
