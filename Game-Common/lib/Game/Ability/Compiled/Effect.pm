package Game::Ability::Complied::Effect;

use Mojo::Base -signatures;
use Moo;
use Types::Standard qw(ConsumerOf Maybe Num);

has 'effect_type' => (
	is => 'ro',
	isa => ConsumerOf['Game::Ability::EffectType'],
	required => 1,
);

has 'attribute' => (
	is => 'ro',
	isa => ConsumerOf['Game::Ability::Attribute'],
	required => 1,
);

has 'value' => (
	is => 'ro',
	isa => Maybe[Num],
	required => 1,
);

has 'deviation' => (
	is => 'ro',
	isa => Maybe[Num],
	required => 1,
);

1;


