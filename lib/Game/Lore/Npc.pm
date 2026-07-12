package Game::Lore::Npc;

use My::Moose;
use Utils qw(pascal_case);

use header;

extends 'Game::Lore::Class';

use constant prefix => 'npc';

has param 'level' => (
	isa => PositiveInt,
);

has param 'respawn_time' => (
	isa => PositiveInt,
);

has option 'ai' => (
	isa => Str,
);

has param 'ai_args' => (
	isa => HashRef,
	default => sub { {} },
);

has cached 'ai_class' => (
	isa => Str,
	lazy => 1,
);

sub _build_ai_class ($self)
{
	return 'Game::Object::Actor::Npc::Ai::' . pascal_case($self->ai);
}

