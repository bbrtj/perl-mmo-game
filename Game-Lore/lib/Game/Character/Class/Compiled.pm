package Game::Character::Class::Compiled;

use header;
use Moo;
use Game::Types qw(LoreId ArrayRef ConsumerOf Maybe Bool PositiveInt Num NonEmptySimpleStr);

no header;

with 'Game::Character::Class';

use constant ABILITY_CHECK => ConsumerOf ['Game::Ability'];

has 'id' => (
	is => 'ro',
	isa => LoreId,
);

sub lore_id ($self)
{
	return $self->id;
}

has 'playable' => (
	is => 'ro',
	isa => Bool,
	required => 1,
);

has 'abilities' => (
	is => 'ro',
	isa => ArrayRef [ABILITY_CHECK],
	init_arg => undef,
	default => sub { [] },
);

has 'base_health' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'health_per_level' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'base_health_regen' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'health_regen_per_level' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'base_focus' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'focus_per_level' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'base_focus_regen' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'focus_regen_per_level' => (
	is => 'ro',
	isa => Num,
	required => 1,
);

has 'base_stats' => (
	is => 'ro',
	isa => NonEmptySimpleStr,
	default => sub { undef },
);

has 'stats_per_level' => (
	is => 'ro',
	isa => NonEmptySimpleStr,
	default => sub { undef },
);

sub add_ability ($self, $ability)
{
	ABILITY_CHECK->assert_valid($ability);
	push $self->abilities->@*, $ability;
	return;
}

1;
