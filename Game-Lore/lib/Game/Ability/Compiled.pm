package Game::Ability::Compiled;

use Mojo::Base -signatures;
use Moo;
use Game::Types qw(ArrayRef HashRef InstanceOf ConsumerOf Maybe Bool PositiveInt PositiveOrZeroInt);
use Game::Ability::Compiled::Group;

has 'groups' => (
	is => 'ro',
	isa => HashRef[InstanceOf['Game::Ability::Compiled::Group']],
	init_arg => undef,
	default => sub { {} },
);

has 'attribute' => (
	is => 'ro',
	isa => ConsumerOf['Game::Ability::Attribute'],
	required => 1,
);

has 'passive' => (
	is => 'ro',
	isa => Bool,
	required => 1,
);

has 'cost' => (
	is => 'ro',
	isa => Maybe[PositiveOrZeroInt],
	required => 1,
);

has 'cooldown' => (
	is => 'ro',
	isa => Maybe[PositiveOrZeroInt],
	required => 1,
);

has 'range' => (
	is => 'ro',
	isa => Maybe[PositiveOrZeroInt],
	required => 1,
);

has 'target_self' => (
	is => 'ro',
	isa => Maybe[Bool],
	required => 1,
);

has 'target_ally' => (
	is => 'ro',
	isa => Maybe[Bool],
	required => 1,
);

has 'target_foe' => (
	is => 'ro',
	isa => Maybe[Bool],
	required => 1,
);

has 'target_ground' => (
	is => 'ro',
	isa => Maybe[Bool],
	required => 1,
);

has 'effect_table' => (
	is => 'ro',
	isa => ArrayRef,
	lazy => 1,
	builder => '_build_effect_table',
	init_arg => undef,
);

sub _build_effect_table($self)
{
	my @results;
	for my $number (sort { $a <=> $b } keys $self->groups->%*) {
		my @group;

		for my $effect ($self->group($number)->effects->@*) {
			my $val = $effect->value;
			my $dev = $effect->deviation // 0;
			push @group, [$effect->effect_type, $effect->attribute, (defined $val ? ($val - $dev, $val + $dev) : ())];
		}

		push @results, \@group;
	}

	return \@results;
}

sub group($self, $number)
{
	state $check = PositiveInt;
	$check->assert_valid($number);

	if (!exists $self->groups->{$number}) {
		$self->groups->{$number} = Game::Ability::Compiled::Group->new(number => $number);
	}

	return $self->groups->{$number};
}

# TODO: also compile talents

1;
