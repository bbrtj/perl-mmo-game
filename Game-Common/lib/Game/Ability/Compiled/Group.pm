package Game::Ability::Compiled::Group;

use Mojo::Base -signatures;
use Moo;
use Types::Standard qw(PositiveInt ArrayRef InstanceOf);

use constant EFFECT_CHECK => InstanceOf['Game::Ability::Compiled::Effect'];

has 'number' => (
	is => 'ro',
	isa => PositiveInt,
	required => 1,
);

has 'effects' => (
	is => 'ro',
	isa => ArrayRef[EFFECT_CHECK],
	init_arg => undef,
	default => sub { [] },
);

sub add_effect($self, $effect)
{
	EFFECT_CHECK->assert_valid($effect);
	push $self->effects->@*, $effect;
	return;
}

1;

