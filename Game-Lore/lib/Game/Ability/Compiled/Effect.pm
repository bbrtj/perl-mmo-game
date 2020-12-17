package Game::Ability::Compiled::Effect;

use header;
use Moo;
use Game::Types qw(ConsumerOf Maybe Num);

no header;

has 'effect_type' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Ability::EffectType'],
	required => 1,
);

has 'attribute' => (
	is => 'ro',
	isa => ConsumerOf ['Game::Ability::Attribute'],
	required => 1,
);

has 'value' => (
	is => 'ro',
	isa => Maybe [Num],
	required => 1,
);

has 'deviation' => (
	is => 'ro',
	isa => Maybe [Num],
	required => 1,
);

1;

